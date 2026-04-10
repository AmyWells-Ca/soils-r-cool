################################################################################
#                                                                              #
#   Initialization of KFN Datasets from Imported .xlsx as well as some         #
#   quick and easy functions specific to this dataset                          #
#                                                                              #
################################################################################
#                                                                              #
# CONTACT INFORMATION                                                          #
#                                                                              #
#   Author: Amy Wells                                                          #
#   Email 1: amys2001@student.ubc.ca                                           #
#   Email 2: github@amywells.ca                                                #
#   Start Date: April 8, 2026                                                  #
#                                                                              #
################################################################################
#                                                                              #
#                              DATASET IMPORT                                  #
#                                                                              #
################################################################################

# Load Packages & Build Utilities
source("./r/functions.R")

# Import Data from "
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")
# data <- readxl::read_xlsx("./input/kwiakah_sr3_soildata", sheet = "Machine Readable")

projectSetup = c()

projectSetup$outliers = showQuestion(
  title = "Data CleanUp",
  message = "Filter out Outliers?",
  ok = TRUE,
  cancel = FALSE
)

if(projectSetup$outliers == TRUE){
  data = filter(data, Outlier < 1)
  projectSetup$filtered = TRUE
} else {
  projectSetup$filtered = FALSE
}

################################################################################
#                                                                              #
#                            FACTOR INITIALIZATION                             #
#                                                                              #
################################################################################

# Site
# Currently the only site is Matsayno
data$Site.f = as.factor(data$Site)
data$Site.f = factor(data$Site.f, levels = c("Matsayno")) # Ensures that Matsayno stays as level one, add additional sites in series

# Plot ID
# data$Plot.f = as.factor(data$Plot) # Pending PlotID rework (1 ID per Pit/Microplot)

# Transect
data$Transect.f = as.factor(data$Transect)
data$Transect.f = factor(data$Transect.f, levels = c("A", "B", "C"))

# Plot Type
data$Type.f = as.factor(data$Type)
data$Type.f = factor(data$Type.f, levels = c("Pit", "Microplot"))

# Land_Use
data$Land_Use.f = as.factor(data$Land_Use)
data$Land_Use.f = factor(data$Land_Use.f, levels = c("Cutblock", "Periphery", "Forest Garden"))

projectSetup$factors = TRUE

################################################################################
#                                                                              #
#                                DATA SUBSETS                                  #
#                                                                              #
################################################################################

# Composite of *All* Samples
data_c = data

# Soil Pit Data on All Transects
data_p = filter(data, Type == "Pit")

# Soil Pits in Transect B --> Primarily For Textural Modelling
data_pB = filter(data_p, Transect == "B")

# Micro Plot Data
data_m = filter(data, Type == "Microplot")

# Top Soil Samples
data_t = filter(data, Depth_Top == 0)

# NOTE: You *need* to ensure that the plot type does not have a significant
#       effect on the response variables of interest to be able to combine
#       the micro plot samples and the topsoil samples of the pits.
#
#       The below function can be used to simply test this effect: 
#
#        fn_effectTest(dataSource, responseVariable)
#
#        e.g. fn_effectTest(data_t, data_t$pH_H2O)
#
#             Transect P --> 0.480
#             Plot Type P --> 0.00117
#
#             Therefore, you cannot use data_t for soil pH

################################################################################
#                                                                              #
#                            QUICK PLOT FUNCTIONS                              #
#                                                                              #
################################################################################

theme_arrow = arrow(length = unit(0.3, "cm"), type = "closed", ends = "both")

qp_landUse = function(dataSource, responseVariable, baselineValue = NULL, agRange = NULL){
  
  # ggplot Initialization
  p_temp = ggplot(data = dataSource, mapping = aes(x = Land_Use.f, y = responseVariable, fill = Land_Use.f))
  
  # Prior tests for for not(is.null(variable))
  
  plotBaseline = !(is.null(baselineValue))
  plotTarget = !(is.null(agRange))
  
  # Calculate Values for Reference Levels
  baselineValue.avg = mean(baselineValue, na.rm = TRUE)
  baselineValue.min = min(baselineValue, na.rm = TRUE)
  baselineValue.max = max(baselineValue, na.rm = TRUE)
  agRange.avg = mean(agRange, na.rm = TRUE)
  agRange.min = min(agRange, na.rm = TRUE)
  agRange.max = max(agRange, na.rm = TRUE)
    
  # Extend X-Axis Depending on Available Comparisons
  
  if( plotBaseline & !plotTarget ){                                             # Case 1: There are only reference values
    p_temp = p_temp + 
      scale_x_discrete(
        limits = c("Cutblock", "Periphery", "Forest Garden","ref"),
        breaks = c("Cutblock","Periphery","Forest Garden"),
        labels = c("Cutblock", "Periphery","Forest Garden")
      )
  
  } else if ( !plotBaseline & plotTarget ) {                                    # Case 2: There are only target values
    p_temp = p_temp + 
      scale_x_discrete(
        limits = c("Cutblock", "Periphery", "Forest Garden", "ref2"),
        breaks = c("Cutblock","Periphery","Forest Garden",""),
        labels = c("Cutblock", "Periphery","Forest Garden","")
      )
    
    agTarget = "ref2"
  
  } else if ( plotBaseline & plotTarget ) {                                    # Case 3: There are baseline and target values
    
    if(baselineValue.max < agRange.min | agRange.max < baselineValue.min){
      p_temp = p_temp + 
        scale_x_discrete(
          limits = c("Cutblock", "Periphery", "Forest Garden","ref"),
          breaks = c("Cutblock","Periphery","Forest Garden",""),
          labels = c("Cutblock", "Periphery","Forest Garden","")
        )
      
      agTarget = "ref"
      
    } else {
      p_temp = p_temp + 
        scale_x_discrete(
          limits = c("Cutblock", "Periphery", "Forest Garden","ref","ref2"),
          breaks = c("Cutblock","Periphery","Forest Garden","",""),
          labels = c("Cutblock", "Periphery","Forest Garden","","")
        )
      
      agTarget = "ref2"
      
    }
    
  }
  
  # Add Annotation Regions
  
  if(plotBaseline){
    # Test if variable is a dataframe
    if(length(baselineValue) == 1){                                             # Case 1: Not a dataframe --> Line
      p_temp = p_temp +
        geom_hline(yintercept = baselineValue)
      
    } else if (length(baselineValue) > 1) {                                     # Case 2: A dataframe --> Region
      p_temp = p_temp +
        annotation_raster(
          "#d3d3d360",
          -Inf, Inf,
          baselineValue.min, baselineValue.max
        )
    }
  }
  
  if(plotTarget){
    # Test if variable is a dataframe
    if(length(agRange) == 1){                                                   # Case 1: Not a dataframe --> Line
      p_temp = p_temp +
        geom_hline(yintercept = agRange)
      
    } else if (length(agRange) > 1) {                                           # Case 2: A dataframe --> Region
      p_temp = p_temp +
        annotation_raster(
          "#d3d3d360",
          -Inf, Inf,
          agRange.min,
          agRange.max
        )
    }
  }
  
  # Text Above Annotations
  
  if(plotBaseline){
    
    # Is it a target range?
    if(baselineValue.min != baselineValue.max){
      p_temp = p_temp +
        geom_segment(
          aes(x="ref",y=baselineValue.min,xend="ref",yend=baselineValue.max),
          arrow = theme_arrow, 
          linetype = "dashed",
        )
    }
    p_temp = p_temp +
      geom_label(
        label = "[ Baseline ]",
        x = "ref",
        y = baselineValue.avg,
        fill = "white",
        label.padding = unit(0.5,"lines"),
      )
  }
  
  if(plotTarget){
    
    # Is it a target range?
    if(agRange.min != agRange.max){
      p_temp = p_temp +
        geom_segment(
          aes(x=agTarget,y=agRange.min,xend=agTarget,yend=agRange.max),
          arrow = theme_arrow, 
          linetype = "dashed"
        )
    }
    
    p_temp = p_temp +
      annotate(
        geom = "label",
        label = "[ Target ]",
        x = agTarget,
        y = agRange.avg,
        label.padding = unit(0.5,"lines"),
      )
  }
  
  # Build the rest of the plot
  
  p_temp = p_temp +
    geom_boxplot(colour = "black") +
    guides(fill = "none") +
    fn_fillScale() +
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 4, 
      size = 4, 
      colour = "black"
    ) +
    labs(
      x = "Land Management",
      y = glue("{deparse(substitute(responseVariable))}")
    )
  
  return(p_temp)
}

qp_depth = function(dataSource, responseVariable, reference){
  
}

projectSetup$complete = TRUE