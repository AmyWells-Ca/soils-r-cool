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


# Import Data set
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")

# Create Factors from Data
data$Transect <- as.factor(data$Transect)
data$Type_Factor <- as.factor(Type)
data$Land_Use_Factor <- as.factor(data$Land_Use)

# Scatterplot Matrix

pairs(~ 
        Depth_Top +
        Land_Use_Factor +
        pH_H2O +
        pH_CaCl2 +
        Si_CaCl2 +
        Si_Acetic 
        
        , data = data, 
        main = "Scatterplot Matrix for Kwiakah First Nation Soils"
)

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

# Si

model_DSi <- lm(Si_CaCl2 ~ Land_Use_Factor + Depth_Top, data=data)
summary(model_DSi)

model_DSi_depth <- lm(Si_CaCl2[data$Type == "Pit"] ~ Depth_Top[data$Type == "Pit"], data=data)
summary(model_DSi_depth)

model_DSi_land <- lm(Si_CaCl2[data$Depth_Top == 0] ~ Land_Use_Factor[data$Depth_Top == 0], data=data)
summary(model_DSi_land)

par(mfrow=c(2,2), cex=1.0, mai=c(1.0,1.0,0.6,0.6), pty="s")
plot(resid(model_DSi_land) ~ fitted(model_DSi_land), main = "Residual Plot", ylab = "Residuals", xlab = "Predicted")
abline(a=0,b=0, col="red")
qqnorm(resid(model_DSi_land))
qqline(resid(model_DSi_land),col=2)
hist(resid(model_DSi_land),density = 5, main="Residuals Distribution", col="green", border="black", xlab = "residuals")
par(mfrow=c(1,1), cex=1.0, mai=c(1.0,1.0,1.0,1.0) )

shapiro.test(resid(model_DSi_land))





plot(
  data$Si_CaCl2[data$Type == "Pit"],
  data$Depth_Top[data$Type == "Pit"]*-1,
  xlab = "Concentration (mg kg-1)",
  ylab = "Depth",
)


ggplot(data = data, mapping = aes(x = Si_CaCl2, y = Depth_Top, color = Land_Use_Factor)) +
  geom_point(
    mapping = aes(x = Si_CaCl2, y = Depth_Top)
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 5))+
  xlim(0,30) +
  ylim(0,75) +
  labs(x = "Concentration (mg kg-1)", y = "Depth", color = "Land Use") +
  theme(
    axis.title = element_text(face = "bold"),
    legent.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.background = element_rect(fill = "white"),
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
