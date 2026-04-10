################################################################################
#                                          
#   Project Initialization Code & Graphing Functions
#                                          
################################################################################
#
# CONTACT INFORMATION
#
#   Author: Amy Wells
#   Email 1: amys2001@student.ubc.ca
#   Email 2: github@amywells.ca
#   Start Date: March 9, 2026
#
################################################################################

# Libraries
# install pacman (package manager) if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(
  rstudioapi, # R-Studio API
  ragg,       # Render Engine needed for Plots
  tidyverse,  # Base
  dplyr,      # Analysis
  remote,     # Install package from GitHub
  readxl,     # Import Excel files
  writexl,    # Write Excel files
  emmeans,    # Estimate Means
  car,        # Type III Anova Analysis
  styler,     # Automatic cleanup 
  renv,       # Save and Re-upload environment
  systemfonts,# Times New Roman and Aral
  latex2exp,  # Latex based figure captions
  ggplot2,    # GGPlot
  glue,       # Variable 
  # plotly,     # Interactive plots
  # DT,         # Interactive tables
  patchwork,  # Combine charts together
  moderndive,  # Paralell Lines???
  ggpubr
)

# Set Render Engine
knitr::opts_chunk$set(dev = "ragg_png")

################################################################################
#                                                                              #
#                         Themes for ggplot2 Figures                           #
#                                                                              #
################################################################################

windowsFonts() # Added to try fixing inconsistent bug with fonts
system_fonts()
match_fonts("Times New Roman")
match_fonts("Arial")
match_fonts("Trebuchet MS")

## Base Theme (Replicates style of graphs Amy makes in excel)
##
##   This is based on tweaks that I consistently made to graphs in excel, that you may
##   also want to include on your graphs!

theme_main = theme_gray() + theme(
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", linetype = "solid"),
  panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
  plot.background = element_rect(fill = "white"),
)

## Presentation Theme!
##
##   My presentations tend to use Trebuchet MS for the slide text, and I wanted
##   to have my figures have a matching font that is large enough to be read 
##

font_presentation = "Trebuchet MS" # <-- Change the font here

theme_presentation = theme(
  plot.title = element_text(size = 20, family = font_presentation, face = "bold", color = "black"),
  plot.subtitle = element_text(size = 16, family = font_presentation, color = "black"),
  plot.caption = element_text(size = 14, family = font_presentation, color = "black"),
  legend.title = element_text(size = 14, family = font_presentation),
  axis.title.x = element_text(size = 16, family = font_presentation),
  axis.title.y = element_text(size = 16, family = font_presentation),
  axis.text.x = element_text(size = 14, family = font_presentation),
  axis.text.y = element_text(size = 14, family = font_presentation)
)

## Paper Theme!
##
##   Papers tend to use Times New Roman for the content text, and I wanted
##   to have my figures have a matching font! 
##

font_paper = "Times New Roman" # <-- Change the font here

theme_paper = theme(
  plot.title = element_text(size = 12, family = font_paper, face = "bold", color = "black"),
  plot.subtitle = element_text(size = 11, family = font_paper, color = "black"),
  plot.caption = element_text(size = 9, family = font_paper, color = "black"),
  legend.title = element_text(size = 10, family = font_paper),
  axis.title.x = element_text(size = 12, family = font_paper),
  axis.title.y = element_text(size = 12, family = font_paper),
  axis.text.x = element_text(size = 10, family = font_paper),
  axis.text.y = element_text(size = 10, family = font_paper)
)

legend_tr = theme(
  legend.position = c(0.95, 0.95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(3, 3, 3, 3)
)

# Can call this theme to remove labels on the X-Axis (if needed)
theme_noX = theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)

## Updates Default Theme
theme_set(theme_main)

################################################################################
#                                                                              #
#               Process to Test for Statistical Significance                   #
#                                                                              #
################################################################################
#
# Step 1. Create model for testing
# Step 2. Check model assumptions
#        - Linearity --> Plot errors of model
#        - Equal Variance
#        - Normality --. Shapiro-Wilk Test on n < 30
#        - Spatial and Temporal Independence
#
#
# Step 3.  Is the model significant?
#        - F-Test
#            p < 0.05 --> Statistically significant model
#            0.05 < p < 0.10 --> Potential statistically significant model
#            p > 0.10 --> Not statistically significant model
#
#
# Step 4. Which of the variables are statistically significant?
#         - TYPE III ANOVA TESTING
#         - Partial F-Tests using the car package
#
#
# Step 5. Are there differences between levels?
#        - Pairwise T-Test**
#        - Bonferroni Adjusted T-Test
#        - Scheffe's Test
#
################################################################################

fn_statTest = function(testModel, testTheme = theme_presentation, saveTest = FALSE){
  
  statTest_residuals = resid(testModel)
  statTest_fitted = fitted(testModel)
  statTest_formula = deparse1(formula(testModel))
  statTest_summary = summary(testModel)
  
  ## Plot Model
  p1 <- ggplot(data = testModel) +
    geom_point(mapping =aes(x = testModel$model[[1]], y = statTest_fitted)) +
    labs(
      title = glue("{substitute(testModel)} | {statTest_formula}"),
      subtitle = glue("df={testModel$df.residual[1]}; Adjusted R Squared: {round(statTest_summary$adj.r.squared,digits=2)}"),
      x = TeX("$\\bf{Measured\\,Values}"),
      y = TeX("$\\bf{Predicted\\,Values}")
    ) + testTheme
  
  ## Test for Linearity & Equal Variance
  p2 <- ggplot(data = testModel) +
    geom_point(mapping = aes(x = statTest_fitted, y = statTest_residuals)) +
    geom_hline(aes(yintercept = 0), color="red") +
    labs(
      title = "Residuals vs. Predicted Values",
      x = TeX("$\\bf{Predicted\\,Values\\,\\hat{y}}"),
      y = TeX("$\\bf{Residuals}")
    ) + testTheme
  
  ## Testing for Normality
  p3 <- ggplot(testModel, aes(sample = statTest_residuals))+stat_qq()+stat_qq_line(color="red")+
    labs(
      title =  "Normality QQ Plot",
      x = TeX("$\\bf{Theoretical}"),
      y = TeX("$\\bf{Sample}")
    ) + testTheme
  
  ## Shaprio-Wilks Test?
  if(testModel$df.residual[1] <= 30){
    statTest_SW <- shapiro.test(statTest_residuals)
    statTest_SW <- statTest_SW$p.value
    statTest_SW <- glue("Shapiro-Wilk Test P-Value: {round(statTest_SW,digits=4)}")
  } else {
    statTest_SW <- ""
  }
  
  ## Histogram Plot
  p4 <- ggplot(testModel, aes(x=statTest_residuals)) +
    geom_histogram(binwidth = round(max(statTest_residuals)-min(statTest_residuals),digits=1)/5, fill = "lightgray", color="darkgray") +
    labs(
      title = "Histogram of Residuals",
      subtitle = substitute(statTest_SW),
      y = TeX("$\\bf{Count}"),
      x = TeX("$\\bf{Residual}")
    ) + testTheme
  
  p5 <- (p1) / (p2 | p3) / (p4)
  print(p5)
  
  if(saveTest==TRUE){
    ## Exports composite graph as .png
    ggsave (
      filename = glue("{substitute(testModel)}.png"),
      plot = p5,
      device = agg_png,
      path = "./output",
      scale = 1,
      width = 6,
      height = 7,
      units = c ("in"),
      dpi = 300,
      limitsize = TRUE,
      bg = NULL,
      create.dir = FALSE
    )
  }
}

################################################################################
#                                                                              #
#                          CONSISTENT COLOURS                                  #
#                                                                              #
################################################################################

# Named R Colours can be found here:
#    https://r-graph-gallery.com/42-colors-names.html
#
# Colour Contrast can be checked using online tools like:
#    https://www.color-blindness.com/coblis-color-blindness-simulator/
# 
# R Point Shapes can be found here:
#    https://www.sthda.com/english/wiki/ggplot2-point-shapes

col_CB = "gold"

col_PF = "darkorange"

col_FG = "purple"

################################################################################
#                                                                              #
#                             GRAPHING AIDS                                    #
#                                                                              #
################################################################################

fn_xLim = function(localMin, localMax){
  return(scale_x_continuous(limits = c(localMin, localMax), expand = c(0,0)))
}

fn_xLimR = function(localMin, localMax){
  return(scale_x_reverse(limits=c(localMin, localMax), expand = c(0,0)))
}

fn_yLim = function(localMin, localMax){
  return(scale_y_continuous(limits = c(localMin, localMax), expand = c(0,0)))
}

fn_yLimR = function(localMin, localMax){
  return(scale_y_reverse(limits=c(localMin, localMax), expand = c(0,0)))
}

fn_shapeScale = function(){
  return(
      scale_shape_manual(
      name = "Land Use",
      labels = c("Cutblock", "Periphery", "Forest Garden"),
      values = c(22,21,24)
    )
  )
}

fn_colourScale = function(){
  return(
    scale_colour_manual(
      name = "Land Use",
      labels = c("Cutblock", "Periphery", "Forest Garden"),
      values = c(col_CB,col_PF,col_FG)
    )
  )
}

fn_slrScale = function(){
  return(
    scale_colour_manual(
      name = "Land Use",
      labels = c("Cutblock", "Periphery", "Forest Garden"),
      values = c(col_CB,col_PF,col_FG)
    )
  )
}

fn_fillScale = function(){
  return(
    scale_fill_manual(
      name = "Land Use",
      labels = c("Cutblock", "Periphery", "Forest Garden"),
      values = c(col_CB,col_PF,col_FG)
    )
  )
}

addBoxPlot = function(xValues, yValues){
  return(
    geom_boxplot(aes(xValues, yValues))
  )
}



fn_compare = function(variable, variableName = variable, compareAxis = "y", compareMode = 0){
  # Compare Modes
  # 0 --> Average
  variable.avg = mean(variable, na.rm = TRUE)
  
  # 1 --> Min to Max Range
  variable.min = min(variable, na.rm = TRUE)
  variable.max = max(variable, na.rm = TRUE)

  # X axis or Y Axis
  if(compareAxis == "y"){
    if(compareMode == 0){
      return(
        last_plot()+
          geom_hline(yintercept = variable.avg, linetype=2)
        )
      
    } else if (compareMode == 1){
      return(
        last_plot() + 
          geom_hline(yintercept = variable.max, linetype=2) + 
          annotation_raster("#d3d3d360", -Inf, Inf, variable.min, variable.max) + 
          geom_hline(yintercept = variable.min, linetype=2)
      )
    }
  } else if(compareAxis == "x") {
    if(compareMode == 0){
      return(
        last_plot()+
          geom_vline(xintercept = variable.avg, linetype=2)
      )
      
    } else if (compareMode == 1){
      return(
        last_plot() + 
          geom_vline(xintercept = variable.max, linetype=2) + 
          annotation_raster("#d3d3d360", variable.min, variable.max, -Inf, Inf) + 
          geom_vline(xintercept = variable.min, linetype=2)
      )
    }
  }
  return()
}

################################################################################
#
#
#
################################################################################

fn_simpleF = function(modelSummary){
  return(1 - pf(modelSummary$fstatistic[1],modelSummary$fstatistic[2],modelSummary$fstatistic[3]))
}

fn_effectTest = function(dataSource, variable){
  
  # Transect Effect?
  model_temp <- lm(variable ~ Transect.f, data = dataSource)
  summary_temp <- summary(model_temp)
  print(glue("Likelyhood that Transect (A/B/C) does not affect {deparse(substitute(variable))}: {as.numeric(fn_simpleF(summary_temp))}"))
  
  # Plot Type Effect?
  model_temp <- lm(variable ~ Type.f, data = dataSource)
  summary_temp <- summary(model_temp)
  print(glue("Likelyhood that Plot Type (Pit/Microplot) does not affect {deparse(substitute(variable))}: {as.numeric(fn_simpleF(summary_temp))}"))
  
}

fn_quickNum = function(inputVariable, numDigits = 2){
  return(
    as.numeric(round(inputVariable, digits = numDigits))
  )
}

################################################################################
#
#
#
################################################################################

fn_quickSave = function(savePlot, plotName = glue("{deparse(substitute(savePlot))}.png"), saveWidth = 16.51, saveHeight = 10){
  ggsave (
    filename = plotName,
    plot = savePlot,
    path = "./output",
    scale = 1,
    width = saveWidth,
    height = saveHeight,
    units = c ("cm"),
    dpi = 300
  )
  return(plotName)
}