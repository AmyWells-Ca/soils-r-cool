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
#   Start Date: March 3, 2026
#
################################################################################

# Libraries
# install pacman (package manager) if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(
  ragg,
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
  patchwork   # Combine charts together
)

# Set Render Engine
knitr::opts_chunk$set(dev = "ragg_png")


# Import Data set
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")

# Create Factors from Data
data$Transect.f <- as.factor(data$Transect)
data$Type.f <- as.factor(data$Type)
data$Land_Use.f <- as.factor(data$Land_Use)

data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Depth_Top == 0)

# Scatterplot Matrix

pairs(~ 
        Depth_Avg +
        Land_Use_Factor +
        perSand +
        perSilt +
        perClay +
        pH_H2O +
        pH_CaCl2 +
        pan_Al +
        pan_Ca +
        pan_Cu +
        pan_Fe +
        pan_K +
        pan_Mg +
        pan_Mn +
        pan_Na +
        pan_P +
        pan_S +
        pan_Zn 
        , data = data, 
        main = "Plant Available Nutrients Scatterplot Matrix on Kwiakah First Nation Soils"
)

pairs(~
        Depth_Avg +
        Land_Use_Factor +
        perSand +
        perSilt +
        perClay +
        pH_H2O +
        pH_CaCl2 +
        Si_CaCl2 +
        Si_Acetic +
        Oxa_Al +
        Oxa_Fe +
        Oxa_Si
        , data = data_depth, 
        main = "Silicon Concentrations Scatterplot Matrix on Kwiakah First Nation Soils"
)

################################################################################
#                                                                              #
#                         Themes for ggplot2 Figures                           #
#                                                                              #
################################################################################
system_fonts()
match_fonts("Times New Roman")
match_fonts("Arial")

## Base Theme (Replicates style of graphs Amy makes in excel)

theme_main = theme_gray() + theme(
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", linetype = "solid"),
  panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
  plot.background = element_rect(fill = "white"),
  legend.position = c(0.95, 0.95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(3, 3, 3, 3)
)

## Changes font to Arial (for Presentations)
theme_presentation = theme(
  plot.title = element_text(size = 12, family = "Arial", face = "bold", color = "black"),
  plot.subtitle = element_text(size = 11, family = "Arial", color = "black"),
  legend.title = element_text(size = 10, family = "Arial"),
  axis.title.x = element_text(size = 12, family = "Arial"),
  axis.title.y = element_text(size = 12, family = "Arial"),
  axis.text.x = element_text(size = 10, family = "Arial"),
  axis.text.y = element_text(size = 10, family = "Arial")
)

## Changes font to Times New Roman (for Papers)
theme_paper = theme(
  plot.title = element_text(size = 12, family = "Times New Roman", face = "bold", color = "black"),
  plot.subtitle = element_text(size = 11, family = "Times New Roman", color = "black"),
  legend.title = element_text(size = 10, family = "Times New Roman"),
  axis.title.x = element_text(size = 12, family = "Times New Roman"),
  axis.title.y = element_text(size = 12, family = "Times New Roman"),
  axis.text.x = element_text(size = 10, family = "Times New Roman"),
  axis.text.y = element_text(size = 10, family = "Times New Roman")
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
      device = png(),
      path = "./OUTPUT",
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
#                     END OF STATISTICAL TESTING FUNCTION                      #
#                                                                              #
################################################################################

# Testing Models

model_pH = lm(pH_CaCl2 ~ pH_H2O, data=data)
model_DSi_depth <- lm(Si_CaCl2 ~ Depth_Top, data=data_depth)
model_DSi_land <- lm(Si_CaCl2 ~ Land_Use_Factor, data=data_landUse)

fn_statTest(model_pH)
fn_statTest(model_DSi_depth, saveTest = TRUE)
fn_statTest(model_DSi_land,theme_paper,TRUE)

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

fn_fixX = function(){
  return(scale_x_continuous(expand = c(0,0)))
}

fn_fixY = function(){
  return(scale_y_continuous(expand = c(0,0)))
}

addBoxPlot = function(xValues, yValues){
  
  # t_min = min(yValues)
  # t_q1 = quantile(yValues)[2]
  # t_median = median(yValues)
  # t_mean = mean(yValues)
  # t_q3 = quantile(yValues)[4]
  # t_max = max(yValues)
  
  return(
    geom_boxplot(aes(xValues, yValues))
    )
  
  # print(t_min)
  # print(t_q1)
  # print(t_median)
  # print(t_mean)
  # print(t_q3)
  # print(t_max)
}

################################################################################

ggplot() +
  addBoxPlot(data$Land_Use.f, data$Si_Acetic) +
  fn_yLim(0,250) +
  labs(
    title = "Title",
    x = "X Values",
    y = "Y Values"
  ) + theme_presentation

################################################################################
#
# Soil Texture
#
################################################################################

# Filter data to only include data where texture was determined
data_Texture = filter(data, perSand != 0)

# Approximate Position Along Transects

### Transect B Length: 856 pixels
### P1 Pos: 70 pixels
### P2 Pos: 406 Pixels
### P3 Pos: 850 Pixels

for(i in 1:nrow(data_Texture)){
  temp = data_Texture[i,]
  temp = temp$Plot

  if(temp == 1){
    PlotPos = rbind(PlotPos, 70/856)
  } else if (temp == 2){
    PlotPos = rbind(PlotPos, (406/856))
  } else if (temp == 3){
    PlotPos = rbind(PlotPos, (850/856))
  }
}
data_Texture <- cbind(data_Texture, PlotPos)



model_Texture <- lm(perSand ~ Depth_Avg + Land_Use.f + Depth_Avg*Land_Use.f, data = data_Texture)

model_Texture.summary = summary(model_Texture)

model_Texture.summary$pValue = pf(model_Texture.summary$fstatistic[1],model_Texture.summary$fstatistic[2],model_Texture.summary$fstatistic[3], lower.tail = FALSE)

fn_statTest(model_Texture, saveTest=TRUE, theme_presentation)

summary(model_Texture)

# Percentage Sand by Depth and Land Management
p1 <- ggplot(data = data_Texture) +
  geom_point(mapping = aes(x = perSand, y = Depth_Avg, color = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_xLim(50,100) +
  fn_yLimR(0,80) +
  guides(shape = "none") +
  labs(
    # title = "Texture by Depth and Land Use",
    # subtitle = TeX(paste0("Model Significance: ","p<<0.01","; df=",model_Texture$df.residual[1],"; Adjusted-R$^{2}$=",round(model_Texture.summary$adj.r.squared, digits = 2))),
    color = TeX("$\\bf{Land\\,Use}"),
    x = TeX("$\\bf{Percent\\,Sand}"),
    y = TeX("$\\bf{Sample\\,Depth}\\,(cm)")
  ) + theme_presentation + theme(legend.position = c (0.95, 0.95), legend.justification = c("right", "top"))

# Plot percent sand by Land Management
p2 <- ggplot() +
  addBoxPlot(data_Texture$Land_Use.f, data_Texture$perSand) +
  fn_yLim(50,100) +
  labs(
    # title = "Percent Sand by Kwiakah Territory Land Management",
    x = TeX("$\\bf{Land\\,Management}"),
    y = TeX("$\\bf{Percent\\,Sand}")
  ) + theme_presentation

p3 <- (p1 + p2 + plot_layout(widths = c(2,1)))

print(p3)

ggsave (
  filename = "Fig_TextureModel.png",
  plot = p3,
  device = png(),
  path = "./OUTPUT",
  scale = 1,
  width = 23.88,
  height = 10.78,
  units = c ("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE
)
  






# Preliminary Plant Available Nutrients  
  
ggplot() +
  addBoxPlot(data$Land_Use.f, data$Si_Acetic) +
  fn_yLim(0,250) +
  labs(
    title = "Title",
    x = "X Values",
    y = "Y Values"
  ) + theme_presentation

ggplot() +
  addBoxPlot(data$Land_Use.f, data$Si_Acetic) +
  fn_yLim(0,250) +
  labs(
    title = "Title",
    x = "X Values",
    y = "Y Values"
  ) + theme_presentation

ggplot() +
  addBoxPlot(data$Land_Use.f, data$Si_Acetic) +
  fn_yLim(0,250) +
  labs(
    title = "Title",
    x = "X Values",
    y = "Y Values"
  ) + theme_presentation

ggplot() +
  addBoxPlot(data$Land_Use.f, data$Si_Acetic) +
  fn_yLim(0,250) +
  labs(
    title = "Title",
    x = "X Values",
    y = "Y Values"
  ) + theme_presentation




statTest_residuals = resid(model_pH)
statTest_fitted = fitted(model_pH)
statTest_formula = deparse1(formula(model_pH))
statTest_summary = summary(model_pH)

round(max(statTest_residuals)-min(statTest_residuals),digits=1)/5

p1 <- ggplot(data = model_pH) +
  geom_point(mapping = aes(x = statTest_fitted, y = statTest_residuals)) +
  geom_hline(aes(yintercept = 0), color="red") +
  labs(
    title = "Residuals vs. Predicted Values",
    x = TeX("$\\bf{Predicted\\,Values\\,\\hat{y}}$"),
    y = TeX("$\\bf{Residuals}$")
  ) + theme_paper


p2 <- ggplot(data = data_landUse) + geom_point(mapping = aes(x=pH_H2O, y=pH_CaCl2))

print(p1)

ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Si_CaCl2, y = Depth_Avg, color = Land_Use_Factor)) +
  scale_x_continuous(limits = c(0, 30), expand = c(0,0))
  scale_y_reverse() + theme_paper

  
  
  
  
  
## Graph Template (Depth)

d1 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Si_CaCl2, y = Depth_Avg, color = Land_Use_Factor)) +
  labs(
    # title = "Fraction of Plant Available Si (DSi)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^-1)"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    color = TeX("$\\bf{Land\\,Management}")
  ) +
  scale_x_continuous(
    limits = c(0,30),
    expand = c(0,0) ## Aligns edges to Gridlines
  ) +
  scale_y_reverse(
    limits = c(0,80),
    expand = c(0,0)
  ) +
  theme_paper

d2 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Si_Acetic, y = Depth_Avg, color = Land_Use_Factor)) +
  labs(
    # title = "Fraction of Adsorbed Si (AdSi)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^-1)"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    color = TeX("$\\bf{Land\\,Management}")
  ) +
  scale_x_continuous(
    limits = c(0,250),
    expand = c(0,0) ## Aligns edges to Gridlines
  ) +
  scale_y_reverse(
    limits = c(0,80),
    expand = c(0,0)
  ) +
  theme_paper 
  
d3 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Oxa_Si, y = Depth_Avg, color = Land_Use_Factor)) +
  labs(
    # title = "Fraction of Adsorbed Si (AdSi)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^-1)"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    color = TeX("$\\bf{Land\\,Management}")
  ) +
  scale_x_continuous(
    limits = c(0,15000),
    expand = c(0,0) ## Aligns edges to Gridlines
  ) +
  scale_y_reverse(
    limits = c(0,80),
    expand = c(0,0)
  ) +
  theme_paper  

## Graph Template (ALM)

a1 <- ggplot(data = data_landUse, aes(x = Land_Use, y=Si_CaCl2)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Pools of DSi by Land Management",
    x = TeX("$\\bf{Land\\,Management\\,Strategy}"),
    y = TeX("$\\bf{DSi\\,Concentration}\\,(mg\\,kg^-1)")
  ) +
  # scale_x_discrete(
  #   expand = c(0,0) ## Aligns edges to Gridlines
  # ) +
  scale_y_continuous(
    limits = c(0,40),
    expand = c(0,0)
  ) +
  theme_paper

a2 <- ggplot(data = data_landUse, aes(x = Land_Use, y=Si_Acetic)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Pools of AdSi by Land Management",
    x = TeX("$\\bf{Land\\,Management\\,Strategy}"),
    y = TeX("$\\bf{AdSi\\,Concentration}\\,(mg\\,kg^-1)")
  ) +
  # scale_x_discrete(
  #   expand = c(0,0) ## Aligns edges to Gridlines
  # ) +
  scale_y_continuous(
    limits = c(0,250),
    expand = c(0,0)
  ) +
  theme_paper

a3 <- ggplot(data = data_landUse, aes(x = Land_Use, y=Oxa_Si)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Pools of AdSi by Land Management",
    x = TeX("$\\bf{Land\\,Management\\,Strategy}"),
    y = TeX("$\\bf{AdSi\\,Concentration}\\,(mg\\,kg^-1)")
  ) +
  # scale_x_discrete(
  #   expand = c(0,0) ## Aligns edges to Gridlines
  # ) +
  scale_y_continuous(
    limits = c(0,15000),
    expand = c(0,0)
  ) +
  theme_paper

c1 <- ((a1 | d1)/(a2 | d2)/(a3 | d3))
print(c1)

ggsave (
  filename = "Composite 1.png",
  plot = c1,
  device = png(),
  path = "./OUTPUT",
  scale = 1,
  width = 6.5,
  height = 7,
  units = c ("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE
)

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
