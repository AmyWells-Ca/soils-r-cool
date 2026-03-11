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

# Load Packages & Build Utilities
source("./r/functions.R")

# Import Data set
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")

# Create Factors from Data
data$Transect.f <- as.factor(data$Transect)
data$Type.f <- as.factor(data$Type)
data$Land_Use.f <- as.factor(data$Land_Use)

data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Depth_Top == 0)

################################################################################
#
# Soil Texture
#
################################################################################

# Filter data to only include data where texture was determined
data_Texture = filter(data, perSand != 0)

# Approximate Position Along Transects

### Transect B Length: 920 pixels
### P1 Pos: 134 pixels
### P2 Pos: 470 Pixels
### P3 Pos: 912 Pixels

Plot_Position <- c()
for(i in 1:nrow(data_Texture)){
  temp = data_Texture[i,]
  temp = temp$Plot

  if(temp == 1){
    Plot_Position = rbind(Plot_Position, (134/920))
  } else if (temp == 2){
    Plot_Position = rbind(Plot_Position, (470/920))
  } else if (temp == 3){
    Plot_Position = rbind(Plot_Position, (912/920))
  }
}
data_Texture <- cbind(data_Texture, Plot_Position)

# Modelling by Depth Only
model_Texture <- lm(perSand ~ Depth_Avg, data = data_Texture)

# Modelling by Position along Transect (0 to 1)
model_Texture_position <- lm(perSand ~ Depth_Avg + Plot_Position + Depth_Avg*Plot_Position, data = data_Texture)

# Modeling by "Classified" Land Use
model_Texture_landUse <- lm(perSand ~ Depth_Avg + Land_Use.f + Depth_Avg*Land_Use.f, data = data_Texture)

summary(model_Texture)
summary(model_Texture_position)
summary(model_Texture_landUse)

Anova(model_Texture, type=c("III"))
Anova(model_Texture_position, type=c("III"))
Anova(model_Texture_landUse, type=c("III"))

fn_statTest(model_Texture, saveTest=TRUE, theme_presentation)
fn_statTest(model_Texture_position, saveTest=TRUE, theme_presentation)
fn_statTest(model_Texture_landUse, saveTest=TRUE, theme_presentation)


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
  


ggplot(data = data) +
  geom_point(mapping = aes(x = Latitude, y = Longitude, color = pH_H2O, size = pH_CaCl2))+
  fn_yLimR(-125.35,-125.365) +
  fn_xLimR(50.555,50.5570)

ggplot(data = data) + 



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

  
pairs(~
        Depth_Avg +
        Land_Use.f +
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
      , data = data
)
  
  
  
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
