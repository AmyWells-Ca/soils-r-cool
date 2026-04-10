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
source("./r/KFN_ini")

# Import Data set
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")
data <- filter(data, Outlier < 1)

# reference <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Reference")

# Land Use as Factor
data$Land_Use.f <- as.factor(data$Land_Use)
data$Land_Use.f <- factor(data$Land_Use.f, levels = c("Cutblock", "Periphery", "Forest Garden"))

# Transect as Factor
data$Transect.f <- as.factor(data$Transect)
data$Transect.f <- factor(data$Transect.f, levels = c("B", "C"))

# Plot Type as Factor
data$Type.f <- as.factor(data$Type)
data$Type.f <- factor(data$Type.f, levels = c("Pit","Microplot"))

# Create Subsets of Data
data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Type == "Microplot")
data_topSoil <- filter(data, Depth_Top == 0)
data_pitTops <- filter(data_depth, Depth_Top == 0)

# Comparisons for Statistical Testing
comparisons <- list(
  c("Cutblock", "Periphery"),
  c("Cutblock", "Forest Garden"),
  c("Periphery", "Forest Garden")
)



################################################################################
#
# pH
#
################################################################################

fn_effectTest(data_t, data_t$pH_H2O)    # Significant Effect
fn_effectTest(data_t, data_t$pH_CaCl2)  # Significant Effect

# Topsoil Values
ref_pH_H2O = c(
  3.8, # Old Growth on Podzol; (Lavkulich & Rowles, 1970)
  4.4, # Alder on Podzol; (Lavkulich & Rowles, 1970)
  4.5  # 2nd Growth on Podzol; (Strivelli, 2010)
)

# Topsoil Values
ref_pH_CaCl2 = c(
  3.2,
  4.1,
  3.9
)

qp_landUse(data_t, data_t$pH_H2O, ref_pH_H2O, c(5.5,7)) + labs(y="pH in H2O")


ggplot(data_t, aes(x = Land_Use.f, y = pan_Ca)) +
  geom_boxplot() +
  scale_x_continuous(limits=c(0,4))


fn_effectTest(dataSource = data_topSoil, data_topSoil$pH_H2O)

# pH by Land Use

pairwise.t.test(x = data_landUse$pH_H2O,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)


p1 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pH_H2O, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (6,6.25,6)
  ) +
  fn_fillScale() +
  fn_yLim(2.5,6.5) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{pH\\,\\i\\n\\,H_{2}O}")
  ) +
  theme_noX

p2 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pH_CaCl2, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (6,6.25,6)
  ) +
  fn_fillScale() +
  fn_yLim(2.5,6.5) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{pH\\,\\i\\n\\,0.1M\\,CaCl_{2}}")
  ) +
  theme_noX

p3 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pH_Delta, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (1.4,1.6,1.4)
  ) +
  fn_fillScale() +
  fn_yLim(0,2) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{\\Delta\\,pH}\\,( pH_{H_{2}O} - pH_{CaCl_{2}} )")
  ) +
  theme_noX

p4 <- (p1 + p2 + p3  + 
  plot_layout(
    guides = "collect"
    ) + 
  plot_annotation(
    tag_levels = "a",
    # title = TeX("$\\bf{Pools\\,of\\,Soil\\,pH\\,by\\,Land\\,Use}"),
    subtitle = glue("Microplot Samples | n={nrow(data_landUse)}"),
    theme = theme_presentation
    )
  )

p4

ggsave (
  filename = "pH_LandUse.png",
  plot = p4,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.8,
  units = c ("cm"),
  dpi = 300
)

# pH by Depth

pairwise.t.test(x = data_depth$pH_H2O,
                g = data_depth$Depth_Avg,
                p.adjust.method = "bonferroni"
)

p5 <- ggplot(data = data_depth, mapping = aes(x = pH_H2O, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") + 
  fn_fillScale() +
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

p6 <- ggplot(data = data_depth, mapping = aes(x = pH_CaCl2, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") + 
  fn_fillScale() +
  fn_shapeScale() +
  fn_xLim(3,6) +
  fn_yLimR(0,80) +
  labs(
    x = TeX("$\\bf{pH\\,\\i\\n\\,0.1M\\,CaCl_{2}}"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  ) + theme_presentation + theme(axis.title.y = element_blank())

pairwise.t.test(x = data_depth$pH_Delta,
                g = data_depth$Land_Use.f,
                p.adjust.method = "bonferroni"
)

p7 <- ggplot(data = data_depth, mapping = aes(x = pH_Delta, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") + 
  fn_fillScale() +
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
    # title = TeX("$\\bf{Pools\\,of\\,Soil\\,pH\\,by\\,Depth}"),
    subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}"),
    theme = theme_presentation
    )
  )

p8

ggsave (
  filename = "pH_Depth.png",
  plot = p8,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.8,
  units = c ("cm"),
  dpi = 300
)

#
# Base Saturation
#

fn_effectTest(data_topSoil, data_topSoil$BaseSaturation)
fn_effectTest(data_topSoil, data_topSoil$Exc_Ca)
fn_effectTest(data_topSoil, data_topSoil$Exc_K)
fn_effectTest(data_topSoil, data_topSoil$Exc_Mg)
fn_effectTest(data_topSoil, data_topSoil$Exc_Mn)
fn_effectTest(data_topSoil, data_topSoil$Exc_Na)
fn_effectTest(data_topSoil, data_topSoil$Exc_Fe)
fn_effectTest(data_topSoil, data_topSoil$Exc_Al)
fn_effectTest(data_topSoil, data_topSoil$Exc_H)

p19 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=BaseSaturation, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (0.4,0.6,0.4)
  ) +
  fn_fillScale() +
  fn_yLim(0,1) +
  labs(
    subtitle = glue("Microplot Samples | n={nrow(data_landUse)}"),
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{\\Base\\,Saturation}\\,\\,( \\frac{\\sum{Ca^{2+}+K^{+}+Mg^{2+}+Mn^{2+}+Na^{+}}}{CEC})")
  ) +
  guides(fill = "none")

p20 <- ggplot(data = data_depth, mapping = aes(x = BaseSaturation, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") + 
  fn_fillScale() +
  fn_shapeScale() +
  fn_xLim(0,1) +
  fn_yLimR(0,80) +
  labs(
    subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}"),
    x = TeX("$\\bf{\\Base\\,Saturation}\\,\\,( \\frac{\\sum{Ca^{2+}+K^{+}+Mg^{2+}+Mn^{2+}+Na^{+}}}{CEC})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p21 <- ((p19 + p20) + 
         plot_layout(
           guides = "collect"
         ) + 
         plot_annotation(
           tag_levels = "a",
           # title = TeX("$\\bf{Pools\\,of\\,Soil\\,pH\\,by\\,Depth}"),
           # subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}"),
           theme = theme_presentation
         )
)

p21

ggsave (
  filename = "BaseSaturation.png",
  plot = p21,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.8,
  units = c ("cm"),
  dpi = 300
)

################################################################################
#
# Plant Available P, K, Ca, Mg
#
################################################################################

# Testing Transect/Plot Type Effects
fn_effectTest(data_topSoil, data_topSoil$pan_P)
fn_effectTest(data_topSoil, data_topSoil$pan_K)
fn_effectTest(data_topSoil, data_topSoil$pan_Ca)
fn_effectTest(data_topSoil, data_topSoil$pan_Mg)
##
## K has a strong effect from the plot types, so analysis of macronutrients should
## focus only on the microplot samples or soil pit samples
##

p9 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pan_P, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (55,65,55)
  ) +
  fn_fillScale() +
  fn_yLim(0,80) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Plant\\,Available\\,P}\\,(mg\\,kg^{-1})")
  ) +
  guides(fill = "none")

p10 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pan_K, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (110,130,110)
  ) +
  fn_fillScale() +
  fn_yLim(0,150) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Plant\\,Available\\,K}\\,(mg\\,kg^{-1})")
  ) +
  guides(fill = "none")

p11 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pan_Ca, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (210,230,210)
  ) +
  fn_fillScale() +
  fn_yLim(0,250) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Plant\\,Available\\,Ca}\\,(mg\\,kg^{-1})")
  ) +
  guides(fill = "none")

p12 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=pan_Mg, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (45,55,45)
  ) +
  fn_fillScale() +
  fn_yLim(0,60) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Plant\\,Available\\,Mg}\\,(mg\\,kg^{-1})")
  ) +
  guides(fill = "none")

p13 <- ((p9 + p10) + 
           plot_layout(
             guides = "collect",
           ) + 
           plot_annotation(
             tag_levels = "a",
             # title = TeX("$\\bf{Plant\\,Available\\,Macro-Nutrients}"),
             subtitle = glue("Microplot Samples | n={nrow(data_landUse)}"),
             theme = theme_presentation
           )
)

p13

p14 <- ((p11 + p12) + 
          plot_layout(
            guides = "collect",
          ) + 
          plot_annotation(
            tag_levels = "a",
            # title = TeX("$\\bf{Plant\\,Available\\,Macro-Nutrients}"),
            subtitle = glue("Microplot Samples | n={nrow(data_landUse)}"),
            theme = theme_presentation
          )
)

p14

ggsave (
  filename = "pan_MacroNutrients_PK.png",
  plot = p13,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

ggsave (
  filename = "pan_MacroNutrients_CaMg.png",
  plot = p14,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

#
#
#
#
#

p15 <- ggplot(data = data_depth, mapping = aes(x = pan_Ca, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,500) +
  labs(
    x = TeX("$\\bf{Plant\\,Available\\,Ca}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p16 <- ggplot(data = data_depth, mapping = aes(x = total_Ca, y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,6000) +
  labs(
    x = TeX("$\\bf{Total\\,Ca}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p17 <- ggplot(data = data_depth, mapping = aes(x = (total_Ca/pan_Ca), y = Depth_Avg, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,600) +
  labs(
    x = TeX("$\\bf{Total\\,Ca/Available\\,Ca}"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p18 <- ((p15 + p16 + p17) + 
          plot_layout(
            guides = "collect",
          ) + 
          plot_annotation(
            tag_levels = "a",
            # title = TeX("$\\bf{Plant\\,Available\\,Macro-Nutrients}"),
            subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}"),
            theme = theme_presentation
          )
)

p18

ggsave (
  filename = "available_total_Ca.png",
  plot = p18,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)







