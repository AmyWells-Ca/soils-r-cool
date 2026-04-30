################################################################################
#                                          
#   Code for the analysis of key silicon fractions in the soils of 
#   the Kwiakah First Nation
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

# Initialize Data & Functions
source("./r/KFN_initialization.R")

################################################################################
#
#   Plot perSand by Land Use and perSand by Depth
#
################################################################################


# Reference Line from Lavkulich and Rowels, 1970
# Lavkulich, L. M., & Rowles, C. A. (1970). Effect of different land use practices on a British Columbia Spodsol. Soil Science, 111(5), 323–329. https://doi.org/10.1097/00010694-197105000-00010

p_pit_Tb_001 = qp_depth(data_pB, data_pB$perSand, Lim = c(50,100,10)) +
  geom_abline(intercept = 147.639686684, slope = -2.55874673629, linetype = 2) +
  annotate("text", family = qp_font, label = "Lavkulich & Rowels, 1970  >", x = 68, y = 55) + 
  labs(
    x = TeX("$\\bf{Percent\\,Sand}\\,(\\%)")
  )
p_pit_Tb_001

fn_quickSave(p_pit_Tb_001)

################################################################################
#
# Parameters
#
################################################################################

fn_quickSummary(lm(perSand ~ Depth_Avg + Land_Use.f, data_pB))
#   R^2 = 0.8023 / 0.7281
# Depth = 0.000761
#  Land = 0.154884

fn_quickSummary(lm(perSand ~ Depth_Avg + Altitude + Latitude, data_pB))
#   R^2 = 0.8023 / 0.7281
# Depth = 0.000761
#   Alt = 0.186964
#   Lat = 0.106575

fn_quickSummary(lm(perSand ~ Depth_Avg + Altitude, data_pB))
#   R^2 = 0.7206 / 0.6585
# Depth = 0.001126
#   Alt = 0.311058
#   Lat = N/A

fn_quickSummary(lm(perSand ~ Depth_Avg + Latitude, data_pB))
#   R^2 = 0.7508 / 0.6954
# Depth = 0.0007666
#   Alt = N/A
#   Lat = 0.1570641

################################################################################
#
# Predictive Model
#
################################################################################

data_c_model = data_c
data_c_model$modeledSand = predict(lm(perSand ~ Depth_Avg + Altitude + Latitude, data_pB), data_c)

p_comp_sand_001 = qp_depth(data_c_model, data_c_model$modeledSand, Lim = c(50,100,10)) +
  labs(
    x = TeX("$\\bf{Modelled\\,Percent\\,Sand}\\,(\\%)$"),
    caption = TeX(paste0("$R^{2} = $", 0.8023, "; Model p < ", 0.001 , "; Depth p < ", 0.001, "; Altitude p = ", 0.187, "; Latitude p = ", 0.107))
  )

p_comp_sand_001
fn_quickSave(p_comp_sand_001)

################################################################################
#                                                                              #
#    Calculation of Position Along Transect (Approximate distance to beach)    #
#                                                                              #
################################################################################

# New Variable for Position Along Transect (Empty to start)
# Plot_Position <- c()

# Calculate position of each plot using values measured from Kaya's map
# for(i in 1:nrow(data)){
#   temp = data[i,]
#   temp = temp$Plot
#   
#   if(temp == 1){
#     Plot_Position = rbind(Plot_Position, (134/920))
#   } else if (temp == 2){
#     Plot_Position = rbind(Plot_Position, (470/920))
#   } else if (temp == 3){
#     Plot_Position = rbind(Plot_Position, (912/920))
#   }
# }

# Add calculated values into the data_texture data frame
# data <- cbind(data, Plot_Position)

################################################################################
#                                                                              #
#                   Model Comparison for Soil Texture                          #
#                                                                              #
################################################################################
#
# % Sand = Depth + Plot_Position + Interaction
#
# model_Texture.2 = lm(perSand ~ Depth_Avg*Plot_Position, data = data_Texture)
# 
# summary(model_Texture.2)
# Anova(model_Texture.2, type = c("III"))
#
################################################################################
#
# Model p-Value = 0.001696
# r = 0.8351
# adj-r = 0.7732
# RMSE = 3.583
#
# --------------+---< p >---+
#         Depth | 0.18578
# Plot Position | 0.52482
#   Interaction | 0.09074
#
# Conclusions:
#   1. There is no statistically significant interaction between Depth and Plot Position
#   2. None of the components of the model are significant, so a simpler model with fewer terms should be used.
#
################################################################################
#
# % Sand = Depth + Plot_Position
# 
# model_Texture.1 = lm(perSand ~ Depth_Avg + Plot_Position, data = data_Texture)
# 
# summary(model_Texture.1)
# Anova(model_Texture.1, type = c("III"))
#
################################################################################
#
# Model p-Value = 0.00166
# r = 0.7589
# adj-r = 0.7053
# RMSE = 4.085
#
# --------------+---< p >---+
#         Depth | 6.849e-4
# Plot Position | 0.1307260
#
# Observations:
#   1. Model fit decreases relative to model_Texture.2
#   2. Depth term becomes a statistically significant component of the model
#   3. Plot position is not a statistically significant component of the model
#
################################################################################
#
# Check Model Assumptions
#
# fn_statTest(model_Texture.1, saveTest = FALSE)
#
# Linearity: Model looks to be linear
# Equal Variance: Variance is a bit wonky, but seems fairly consistent
# Independence: Spatial relationships exist, but are okay? (that is what we're trying to model); Temporarl relationships don't exist, samples collected at the same time.
# Normality: Residuals are normally distributed, and Shaprio-Wilk test pas a p > 0.05
#
################################################################################
#
# % Sand = Depth
# 
# model_Texture.0 = lm(perSand ~ Depth_Avg, data = data_Texture)
# 
# summary(model_Texture.0)
# Anova(model_Texture.0, type = c("III"))
#
################################################################################
#
# Model p-Value = 0.0008926
# r = 0.6848
# adj-r = 0.6533
# RMSE = 4.431
#
# --------------+---< p >---+
#         Depth | 8.926e-4
#
# Observations:
#   1. Model fit decreases relative to model_Texture.1
#   2. Depth term becomes a statistically significant component of the model
#
################################################################################
#
# Check Model Assumptions
#
# fn_statTest(model_Texture.0, saveTest = FALSE)
#
# Linearity: Model looks to be linear
# Equal Variance: Variance is less consistent than in model_Texture.1
# Independence: Spatial relationships exist, but are okay? (that is what we're trying to model); Temporarl relationships don't exist, samples collected at the same time.
# Normality: Residuals are normally distributed, and Shaprio-Wilk test pas a p > 0.05
################################################################################


#   If you're a human reading this you should take a break to enjoy a cookie   #


################################################################################
#                                                                              #
#                              Selected Model:                                 #
#                              model_Texture.2                                 #
#                                                                              #
################################################################################
#                                                                              #
#  model_Texture.2 was selected as it is known that the position from the      #
#  beach has an important role on the texture of the soils, and this fact      #
#  would likely have been revealed with the analysis of texture in more        #
#  samples.                                                                    #
#                                                                              #
################################################################################

# # Simple linear model to predict the percent sand by depth in the soil
# model_Texture = lm(perSand ~ Depth_Avg + Altitude + Latitude, data = data_pB)
# 
# # Model Evaluation Tests (Should you choose to review them again)
# summary(model_Texture)
# Anova(model_Texture, type = c("III"))
# fn_statTest(model_Texture, theme_presentation, saveTest = TRUE)
# 
# predictSand = predict(model_Texture, data)
# data$predictSand = predictSand
# 
# # Calculating slope + intercepts for model_Texture
# 
# 
# 
# p4 <- ggplot(data = data) +
#   geom_point(mapping = aes(x=perSand, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
#   geom_point(mapping = aes(x=predictSand, y = Depth_Jitter, colour = Land_Use.f, shape = Land_Use.f), size = 1) +
#   scale_shape_manual(
#     name = "Land Use",
#     labels = c("Cutblock", "Forest Garden", "Periphery Forest"),
#     values = c(15,16,17)
#   ) +
#   scale_colour_manual(
#     name = "Land Use",
#     labels = c("Cutblock", "Forest Garden", "Periphery Forest"),
#     values = c("darkorange2","darkgreen","azure4")
#   ) +
#   fn_yLimR(0, 80) +
#   fn_xLim(50,100) +
#   labs(
#     x = TeX("$\\bf{Percent\\,Sand}\\,(\\%)"),
#     y = TeX("$\\bf{Depth}\\,(cm)")
#   )
# 
# ggsave (
#   filename = "predicted_texture.png",
#   plot = p4,
#   device = png(),
#   path = "./output",
#   scale = 1,
#   width = 14.64,
#   height = 10.78,
#   units = c ("cm"),
#   dpi = 300,
#   limitsize = TRUE,
#   bg = NULL,
#   create.dir = FALSE
# )
 