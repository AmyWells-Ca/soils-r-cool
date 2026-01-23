################################################################################
#                                          
# Soils Code for Soils Sub-cluster in Kwiakah Research Cluster
#                                          
################################################################################
#
# CONTACT INFORMATION
#
#   Author: Amy Wells
#   Email 1: amys2001@student.ubc.ca
#   Email 2: github@amywells.ca
#   Start Date: January 19, 2026
#
################################################################################

# Libraries ####
# install pacman (package manager) if needed
if (!require("pacman")) install.packages("pacman")
# Load contributed packages with pacman
pacman::p_load(
  tidyverse,  # Base
  remote,     # Install package from GitHub
  readxl,     # Import Excel files
  writexl,    # Write Excel files
  emmeans,    # Estimate Means
  car,        # Type III Anova Analysis
  styler,     # Automatic cleanup 
  renv,       # Save and Re-upload environment
  plotly,     # Interactive plots
  DT,         # Interactive tables
  patchwork,  # Combine charts together
  hrbrthemes  #"ipsum" theme for ggplot2 charts
)

# Import ICP-OES Data
data_raw_ICP_OES = readxl::read_xlsx("./input/icp_oes_data.xlsx")


# Function to correct blank reading
correctBlank = function(dataSource, analysisType) {
  
  elements = dataSource[dataSource$SampleAnalysis == analysisType,]
  elementsBlank = elements[elements$PlotID == "Blank",] 
  elementsMeasured = elements[elements$PlotID != "Blank",]
  
  results = elementsBlank
  nEvaluated = ncol(elementsMeasured)
  
  # Iterates over each row of data for the analysis
  for (x in 1:nrow(elementsMeasured)) {
    
    # PlotID, Sample Analysis, Sample Mass, Sample Volume, and Dilution Factor Metadata
    temp = elementsMeasured[x,1:5] 
    
    # Constant   =          Volume          /      Sample             *    Dilution Factor
    tempConstant = (((elementsMeasured[x,4])/(elementsMeasured[x,3])) * elementsMeasured[x,5])
    
    for (y in 6:ncol(elementsMeasured)){
      temp = cbind(temp, (elementsMeasured[x,y]-elementsBlank[1,y])*tempConstant)
    }
    
    results = rbind(results, temp)
  }
  return(results)
}

SiCaCl2 = correctBlank(data_raw_ICP_OES, "Si_CaCl2")


correctBlank2 = function(dataSource) {
  
  analysisTypes = unique(datasource$SampleAnalysis)
  
  for (step in AnalysisTypes){
    print(step)
    analysisSet = dataSource[dataSource$SampleAnalysis == step]
    
    
    
  }
  
  elementsBlank = analysisSet[analysisSet$PlotID == "Blank",] 
  elementsMeasured = analysisSet[analysisSet$PlotID != "Blank",]
  
  results = elementsBlank
  nEvaluated = ncol(elementsMeasured)
  
  # Iterates over each row of data for the analysis
  for (x in 1:nrow(elementsMeasured)) {
    
    # PlotID, Sample Analysis, Sample Mass, Sample Volume, and Dilution Factor Metadata
    temp = elementsMeasured[x,1:5] 
    
    # Constant   =          Volume          /      Sample             *    Dilution Factor
    tempConstant = (((elementsMeasured[x,4])/(elementsMeasured[x,3])) * elementsMeasured[x,5])
    
    for (y in 6:ncol(elementsMeasured)){
      temp = cbind(temp, (elementsMeasured[x,y]-elementsBlank[1,y])*tempConstant)
    }
    
    results = rbind(results, temp)
  }
  
  results = results[-c(3,4,5)]
  
  return(results)
}

corrected = correctBlank2(data_raw_ICP_OES)



analysisTypes = unique(data_raw_ICP_OES$SampleAnalysis)


for (step in analysisTypes){
  print(step)
}

  

# Export Corrected ICP-OES Data

write_xlsx()



# Import Data set
data <- readxl::read_xlsx("./input/cleaned-data.xlsx")





# Plot 1: Soil Organic Matter and Cation Exchange Capacity & pH
# plot(rnorm(50),rnorm(50))
# 
# ggplot(data = soil, mapping = aes(x = Oven_Dried_SOM, y = CEC, color = pH_H2O)) +
#   geom_point(
#     mapping = aes(x= Oven_Dried_SOM, y = CEC)
#   ) +
#   scale_x_continuous(breaks = seq(10, 80, by = 5)) +
#   ylim(0, 60) +
#   labs(x = "Percent Soil Organic Matter", y = "Cation Exchange Capacity (meq/100g)", color = "pH in H2O")+
#   theme(
#     axis.title = element_text(face = "bold"),
#     legend.title = element_text(face = "bold"),
#     panel.background = element_rect(fill = "white"),
#     panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
#     plot.background = element_rect(fill = "white"),
#   )
# 
# p1
# 
# ggsave (
#   filename = "Figure 1.png",
#   plot = p1,
#   device = png(),
#   path = "./OUTPUT",
#   scale = 1,
#   width = 6.5,
#   height = 4,
#   units = c ("in"),
#   dpi = 300,
#   limitsize = TRUE,
#   bg = NULL,
#   create.dir = FALSE
# )
