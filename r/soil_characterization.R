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
data$Land_Use.f <- as.factor(data$Land_Use)
data$Land_Use.f <- factor(data$Land_Use.f, levels = c("Cutblock", "Periphery", "Forest Garden"))

data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Depth_Top == 0)

################################################################################
#
# pH
#
################################################################################

# pH by Land Use

pairwise.t.test(x = data_landUse$pH_H2O,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

p1 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x=Land_Use.f, y = pH_H2O, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  fn_yLim(2.5,6) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{pH\\,\\i\\n\\,H_{2}O}")
  ) + theme_presentation + theme(axis.text.x = element_blank(), axis.title.x = element_blank())



pairwise.t.test(x = data_landUse$pH_CaCl2,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

p2 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x=Land_Use.f, y = pH_CaCl2, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  fn_yLim(2.5,6) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{pH\\,\\i\\n\\,0.1M\\,CaCl_{2}}")
  ) + theme_presentation + theme(axis.text.x = element_blank(), axis.title.x = element_blank())


pairwise.t.test(x = data_landUse$pH_Delta,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

p3 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x=Land_Use.f, y = pH_Delta, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  fn_yLim(0,2) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{\\Delta\\,pH}\\,( pH_{H_{2}O} - pH_{CaCl_{2}} )")
  ) + theme_presentation + theme(axis.text.x = element_blank(), axis.title.x = element_blank())

p4 <- (p1 + p2 + p3  + 
  plot_layout(
    guides = "collect"
    ) + 
  plot_annotation(
    tag_levels = "a",
    title = TeX("$\\bf{Pools\\,of\\,Soil\\,pH\\,by\\,Land\\,Use}"),
    subtitle = "n = 24; Microplot Samples (n=18) & 0-15cm Layer (n=6)",
    theme = theme_presentation
    )
  )

p4

ggsave (
  filename = "KwiakahCluster_pH_LandUse.png",
  plot = p4,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 10.78,
  units = c ("cm"),
  dpi = 300
)

# pH by Depth

pairwise.t.test(x = data_depth$pH_H2O,
                g = data_depth$Depth_Avg,
                p.adjust.method = "bonferroni"
)

p5 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = pH_H2O, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) + 
  fn_colourScale() +
  fn_shapeScale() +
  fn_xLim(3,6) +
  fn_yLimR(0,80) +
  labs(
    x = TeX("$\\bf{pH\\,\\i\\n\\,H_{2}O}"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  ) + theme_presentation

pairwise.t.test(x = data_depth$pH_CaCl2,
                g = data_depth$Depth_Avg,
                p.adjust.method = "bonferroni"
)

p6 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = pH_CaCl2, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) + 
  fn_colourScale() +
  fn_shapeScale() +
  fn_xLim(3,6) +
  fn_yLimR(0,80) +
  labs(
    x = TeX("$\\bf{pH\\,\\i\\n\\,0.1M\\,CaCl_{2}}"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  ) + theme_presentation + theme(axis.title.y = element_blank())

pairwise.t.test(x = data_depth$pH_Delta,
                g = data_depth$Depth_Avg,
                p.adjust.method = "bonferroni"
)

p7 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = pH_Delta, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) + 
  fn_colourScale() +
  fn_shapeScale() +
  fn_xLim(0,2) +
  fn_yLimR(0,80) +
  labs(
    x = TeX("$\\bf{\\Delta\\,pH}\\,( pH_{H_{2}O} - pH_{CaCl_{2}} )"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  ) + theme_presentation + theme(axis.title.y = element_blank())

p8 <- ((p5 + p6 + p7) + 
  plot_layout(
    guides = "collect"
    ) + 
  plot_annotation(
    tag_levels = "a",
    title = TeX("$\\bf{Pools\\,of\\,Soil\\,pH\\,by\\,Depth}"),
    subtitle = "n = 24; Soil Pit Samples (n=24)",
    theme = theme_presentation
    )
  )

p8

ggsave (
  filename = "KwiakahCluster_pH_Depth.png",
  plot = p8,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 10.78,
  units = c ("cm"),
  dpi = 300
)

#
# pH Analytical Models
#

model_pH_H2O.d <- lm(pH_H2O ~ Depth_Avg+Land_Use.f, data = data_depth)
model_pH_CaCl2.d <- lm(pH_CaCl2 ~ Depth_Avg+Land_Use.f, data = data_depth)
model_pH_Delta.d <- lm(pH_Delta ~ Depth_Avg+Land_Use.f, data = data_depth)

summary(model_pH_H2O.d)
Anova(model_pH_H2O.d, type = c("III"))
fn_statTest(model_pH_H2O.d, saveTest = FALSE)

summary(model_pH_CaCl2.d)
Anova(model_pH_CaCl2.d, type = c("III"))
fn_statTest(model_pH_CaCl2.d, saveTest = FALSE)

summary(model_pH_Delta.d)
Anova(model_pH_Delta.d, type = c("III"))
fn_statTest(model_pH_Delta.d, saveTest = FALSE)


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
