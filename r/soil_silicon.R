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

# Load Packages & Build Utilities
source("./r/functions.R")

# Import Data set
data <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Machine Readable")
reference <- readxl::read_xlsx("./input/readable_data.xlsx", sheet = "Reference")

# Create Factors from Data
data$Land_Use.f <- as.factor(data$Land_Use)
data$Land_Use.f <- factor(data$Land_Use.f, levels = c("Cutblock", "Periphery", "Forest Garden"))

data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Type == "Microplot")

# area = c(12, 14, 19)
# 
# mean(reference$Si_CaCl2)
# 
# ggplot(data = data, aes(y = Si_CaCl2, x = Land_Use.f)) +
#   geom_boxplot(notch = FALSE) + 
#   theme_paper
#   fn_compare(reference$Si_CaCl2, compareMode = 1)
#   
# ggplot(data = data, aes(y = Land_Use.f, x=Si_CaCl2)) +
#   geom_boxplot(notch = FALSE) +
#   theme_paper
#   fn_compare(reference$Si_CaCl2, compareMode = 0, compareAxis = "x")
# 

################################################################################

#
# Dissolved Silicon
#

p101 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x = Land_Use.f, y=Si_CaCl2, fill = Land_Use.f), colour = "black") +
  fn_fillScale() +
  fn_yLim(0,30) +
  labs(
    subtitle = "Microplot Samples (n=18)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})")
  )

p102 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Si_CaCl2, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,30) +
  labs(
    subtitle = "Soil Pit Samples (n=24)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p103 <- ((p101 + p102) + 
         plot_layout(
           guides = "collect"
         ) + 
         plot_annotation(
           tag_levels = "a",
           title = TeX("$\\bf{Plant\\,Available\\,Silicon}"),
           theme = theme_presentation
         )
)

p103

ggsave (
  filename = "DSi_Fraction_Base.png",
  plot = p103,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

#
# Adsorbed Si
#

pairwise.t.test(x = data_landUse$Si_Acetic,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

model_AdSi.lm = lm(Si_Acetic ~ Land_Use.f, data = data_landUse)
summary(model_AdSi.lm)
Anova(model_AdSi.lm, type = c("III"))

p104 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x = Land_Use.f, y=Si_Acetic, fill = Land_Use.f), colour = "black") +
  fn_fillScale() +
  fn_yLim(0,100) +
  labs(
    subtitle = "Microplot Samples (n=18)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})")
  )

### Model Record Keeping

### model_AdSi.d = lm(Si_Acetic ~ Depth_Avg*Land_Use.f, data = data_depth)
### Depth * Land_Use.f insignificant interaction (p = 0.95)

model_AdSi.d = lm(Si_Acetic ~ Depth_Avg + Land_Use.f, data = data_depth)
summary(model_AdSi.d)
Anova(model_AdSi.d, type = c("III"))
fn_statTest(model_AdSi.d)

p105 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Si_Acetic, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  geom_abline(aes(intercept = (32.3314/1.69242), slope = -1/(model_AdSi.d$coefficients[2]), colour = "Cutblock"), linetype = 2) +
  geom_abline(aes(intercept = ((32.3314+39.1495)/1.69242), slope = -1/1.6924, colour = "Periphery"), linetype = 4) +
  geom_abline(aes(intercept = ((32.3314-16.2029)/1.69242), slope = -1/1.6924, colour = "Forest Garden"), linetype = 5) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,250) +
  labs(
    subtitle = "Soil Pit Samples (n=24)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p105

p106 <- ((p104 + p105) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             title = TeX("$\\bf{Adsorbed\\,Silicon}"),
             theme = theme_presentation
           )
)

p106

ggsave (
  filename = "AdSi_Fraction_Base.png",
  plot = p106,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

#
# Weakly Crystaline Si (Oxalate Extractable)
#

pairwise.t.test(x = data_landUse$Oxa_Si,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

model_WkSi.lm = lm(Oxa_Si ~ Land_Use.f, data = data_landUse)
summary(model_WkSi.lm)
Anova(model_WkSi.lm, type = c("III"))
fn_statTest(model_WkSi.lm)

p107 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x = Land_Use.f, y=Oxa_Si, fill = Land_Use.f), colour = "black") +
  fn_fillScale() +
  fn_yLim(0,4000) +
  labs(
    subtitle = "Microplot Samples (n=18)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})")
  )

p107

### Model Record Keeping

### model_WkSi.d = lm(Oxa_Si ~ Depth_Avg*Land_Use.f, data = data_depth)
### Depth * Land_Use.f insignificant interaction (p = 0.77)

model_WkSi.d = lm(log(Oxa_Si) ~ Depth_Avg+Land_Use.f, data = data_depth)
summary(model_WkSi.d)
Anova(model_WkSi.d, type = c("III"))
fn_statTest(model_WkSi.d, saveTest = TRUE)

p108 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Oxa_Si, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  # geom_abline(aes(intercept = (32.3314/1.69242), slope = -1/(model_AdSi.d$coefficients[2]), colour = "Cutblock"), linetype = 2) +
  # geom_abline(aes(intercept = ((32.3314+39.1495)/1.69242), slope = -1/1.6924, colour = "Periphery"), linetype = 4) +
  # geom_abline(aes(intercept = ((32.3314-16.2029)/1.69242), slope = -1/1.6924, colour = "Forest Garden"), linetype = 5) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,15000) +
  labs(
    subtitle = "Soil Pit Samples (n=24)",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p108

p109 <- ((p107 + p108) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             title = TeX("$\\bf{Weakly\\,Crystaline\\,Silicon}"),
             subtitle = "Allophane and Immogolite",
             theme = theme_presentation
           )
)

p109

ggsave (
  filename = "WkSi_Fraction_Base.png",
  plot = p109,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

p110 <- ggplot(data = data_depth) +
  geom_point(mapping = aes(x = Oxa_Al, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  # geom_abline(aes(intercept = (32.3314/1.69242), slope = -1/(model_AdSi.d$coefficients[2]), colour = "Cutblock"), linetype = 2) +
  # geom_abline(aes(intercept = ((32.3314+39.1495)/1.69242), slope = -1/1.6924, colour = "Periphery"), linetype = 4) +
  # geom_abline(aes(intercept = ((32.3314-16.2029)/1.69242), slope = -1/1.6924, colour = "Forest Garden"), linetype = 5) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,40000) +
  labs(
    subtitle = "Soil Pit Samples (n=24)",
    x = TeX("$\\bf{Al\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
  )

p110

p111 <- ggplot(data = data_landUse) +
  geom_jitter(mapping = aes(x = Land_Use.f, y=Oxa_AlSi, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLim(0,50) +
  labs(
    subtitle = "Microplot Samples (n=12); 6 <DL for Si",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si}$:$\\bf{Al\\,Ratio}")
  )

p111

p112 <- ggplot(data = data_depth) +
  geom_jitter(mapping = aes(x = Land_Use.f, y=Oxa_AlSi, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLim(0,20) +
  labs(
    subtitle = "Soil Pit Samples (n=24)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si}$:$\\bf{Al\\,Ratio}")
  )

p112

p113 <- ((p110 + (p112 / p111)) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             title = TeX("$\\bf{Oxalate\\,Extractable\\,Aluminum}"),
             subtitle = "Allophane and Immogolite",
             theme = theme_presentation
           )
)

p113

ggsave (
  filename = "WkSi_Aluminum.png",
  plot = p113,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

#
# Amorphous Si
#

pairwise.t.test(x = data_landUse$Si_kin,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

pairwise.t.test(x = data_landUse$SiAl_kin,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
)

p114 <- ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x = Land_Use.f, y=Si_kin, fill = Land_Use.f), colour = "black") +
  fn_fillScale() +
  fn_yLim(0,8000) +
  labs(
    subtitle = "Microplot Samples (n=18)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})")
  )

p114

p115 <- ggplot(data = data_landUse) +
  geom_jitter(mapping = aes(x = Land_Use.f, y=SiAl_kin, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLim(0,4) +
  labs(
    subtitle = "Microplot Samples (n=18)",
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si}$:$\\bf{Al\\,Ratio}")
  )

p115

p116 <- ((p114 + p115) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             title = TeX("$\\bf{Amorphous\\,Silicon}"),
             theme = theme_presentation
           )
)

p116

ggsave (
  filename = "ASi_Microplots.png",
  plot = p116,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)
### Model Record Keeping

### model_WkSi.d = lm(Oxa_Si ~ Depth_Avg*Land_Use.f, data = data_depth)
### Depth * Land_Use.f insignificant interaction (p = 0.77)

# model_WkSi.d = lm(log(Oxa_Si) ~ Depth_Avg+Land_Use.f, data = data_depth)
# summary(model_WkSi.d)
# Anova(model_WkSi.d, type = c("III"))
# fn_statTest(model_WkSi.d, saveTest = TRUE)
# 
# p111 <- ggplot(data = data_depth) +
#   geom_point(mapping = aes(x = Oxa_Si, y = Depth_Avg, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
#   # geom_abline(aes(intercept = (32.3314/1.69242), slope = -1/(model_AdSi.d$coefficients[2]), colour = "Cutblock"), linetype = 2) +
#   # geom_abline(aes(intercept = ((32.3314+39.1495)/1.69242), slope = -1/1.6924, colour = "Periphery"), linetype = 4) +
#   # geom_abline(aes(intercept = ((32.3314-16.2029)/1.69242), slope = -1/1.6924, colour = "Forest Garden"), linetype = 5) +
#   fn_colourScale() +
#   fn_shapeScale() +
#   fn_yLimR(0,80) +
#   fn_xLim(0,15000) +
#   labs(
#     subtitle = "Soil Pit Samples (n=24)",
#     x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
#     y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)")
#   )
# 
# p111



model_DSi.1 <- lm(Si_CaCl2 ~ Land_Use.f, data = data_landUse)
model_DSi.2 <- lm(Si_CaCl2 ~ Land_Use.f + model_perSand, data = data_landUse)
model_DSi.3 <- lm(Si_CaCl2 ~ Land_Use.f + model_perSand + Land_Use.f*model_perSand, data = data_landUse)


summary(model_DSi.1)
Anova(model_DSi.1, type = c("III"))
fn_statTest(model_DSi.1, saveTest = FALSE)

summary(model_DSi.2)
Anova(model_DSi.2, type = c("III"))
fn_statTest(model_DSi.2, saveTest = FALSE)

summary(model_DSi.3)
Anova(model_DSi.3, type = c("III"))
fn_statTest(model_DSi.3, saveTest = FALSE)

plots.emm <- emmeans (model_DSi.2, ~ Land_Use.f)

emmeans (model_DSi.2, ~ Land_Use.f)

# simple means
tapply(data$Si_CaCl2, data$Land_Use.f, mean) # simple means

# Are they different? Test this.
pairs(plots.emm, adjust="bonferroni", side="two-sided") 

ggplot(data = data_landUse) +
  geom_boxplot(mapping = aes(x = Land_Use.f, y = Si_kin))

pairwise.t.test(x = data_landUse$Si_kin,
                g = data_landUse$Land_Use.f,
                p.adjust.method = "bonferroni"
                )

ggplot(data = data) +
  geom_point(mapping = aes(x = Depth_Avg, y = Si_CaCl2, colour = Land_Use.f, shape = Land_Use.f), size = 3)

model_DSi.4 <- lm(Si_CaCl2 ~ Depth_Avg, data = data)

summary(model_DSi.4)
Anova(model_DSi.4, type = c("III"))

################################################################################

ggplot(data = data) +
  geom_point(mapping = aes(x=Si_kin, y = SiAl_kin, colour = Land_Use.f, shape = Land_Use.f), size = 3) +
  fn_colourScale() +
  fn_shapeScale() +
  fn_yLim(0,4) +
  fn_xLim(0,5000) + labs(
    x = TeX("$\\bf{Amorphous\\,Si}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Amorphous\\,Si:Al\\,Ratio}")
  )

ggplot(data = data) +
  geom_point(mapping = aes(x=Al_kin, y = pH_CaCl2, colour = Land_Use.f))

ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y = Si_kin, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  fn_yLim(0,7500) +
  labs(
    title = TeX("$\\frac{A}{B}\\;asdf^{1235}_{1234}$")
  )

ggplot(data = data) +
  geom_boxplot(mapping = aes(x=Land_Use.f, y = SiAl_kin, fill = Land_Use.f), color = "black")+
  fn_fillScale() +
  fn_yLim(0,4)

ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Si_CaCl2, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  guides(fill = "none") +
  fn_yLim(0,25) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{CaCl_{2}\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

p2 <- ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Si_Acetic, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  guides(fill = "none") +
  fn_yLim(0,250) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Acetic\\Acid\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Oxa_Si, fill = Land_Use.f), color = "black") +
  fn_fillScale() +
  guides(fill = "none") +
  fn_yLim(0,15000) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Oxalate\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

p5 <- (p1 + p2 + p3)

print(p4)

ggsave (
  filename = "figure_n_DescribedSi.png",
  plot = p4,
  device = agg_png(),
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 10.78,
  units = c ("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE
)
