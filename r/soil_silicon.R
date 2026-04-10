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
source("./r/KFN_ini")


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
statComparisons <- list(
  c("Cutblock", "Periphery"),
  c("Periphery", "Forest Garden"),
  c("Cutblock", "Forest Garden")
)

################################################################################
#
# Dissolved Silicon
#
################################################################################


p_t_DSi_001 <- qp_landUse(data_t, data_t$Si_CaCl2)

p_t_DSi_001

# Testing Transect/Plot Type Effects
fn_effectTest(data_topSoil, data_topSoil$Si_CaCl2)
## Transects & Plot Type do not have a statistically significant effect (p>0.1)
## Therefore, data$topSoil samples can be used for Si_CaCl2 to compare potential land use effects

p101 <- ggplot(data = data_topSoil, mapping = aes(x = Land_Use.f, y=Si_CaCl2, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (29,31,29)
  ) +
  fn_fillScale() +
  fn_yLim(0,35) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    subtitle = glue("Top Soil Samples | n={nrow(data_topSoil)}"),
    caption = glue("Soil Pit Samples: {nrow(filter(data_topSoil, Type == 'Pit'))}; Microplot Samples: {nrow(filter(data_topSoil, Type == 'Microplot'))}")
  ) +
  guides(fill = "none")

p101

model_DSi.d <- lm(Si_CaCl2 ~ Depth_Avg+Land_Use.f, data = data_depth)
m_DSi.d.summary <- summary(model_DSi.d)
m_DSi.d.Anova <- Anova(model_DSi.d, type = c("III"))

fn_statTest(model_DSi.d)

m_DSi.d.summary
m_DSi.d.Anova

m_DSi.d.tTest <- pairwise.t.test(x = data_depth$Si_CaCl2,
                g = data_depth$Land_Use.f,
                p.adjust.method = "bonferroni"
)

m_DSi.d <- c()
m_DSi.d$b <- as.numeric(model_DSi.d$coefficients[1])
m_DSi.d$m <- as.numeric(model_DSi.d$coefficients[2])
m_DSi.d$a1 <- as.numeric(model_DSi.d$coefficients[3])
m_DSi.d$a2 <- as.numeric(model_DSi.d$coefficients[4])
m_DSi.d$F <- fn_quickNum(fn_simpleF(m_DSi.d.summary),4)
m_DSi.d$Fd <- fn_quickNum(m_DSi.d.Anova$`Pr(>F)`[2], 4)
m_DSi.d$Fl <- fn_quickNum(m_DSi.d.Anova$`Pr(>F)`[3], 4)

p102 <- ggplot(data = data_depth, mapping = aes(x = Si_CaCl2, y = Depth_Jitter, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  geom_abline(aes(intercept = (m_DSi.d$b/m_DSi.d$m), slope = -1/(m_DSi.d$m), colour = "0"), linetype = 2) +
  geom_abline(aes(intercept = ((m_DSi.d$b+m_DSi.d$a1)/m_DSi.d$m), slope = -1/m_DSi.d$m, colour = "1"), linetype = 4) +
  geom_abline(aes(intercept = ((m_DSi.d$b+m_DSi.d$a2)/m_DSi.d$m), slope = -1/m_DSi.d$m, colour = "2"), linetype = 5) +
  fn_slrScale() +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,30) +
  labs(
    subtitle = "Soil Pit Samples | n=24",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    caption = glue("Model p: {m_DSi.d$F}, Depth p: {m_DSi.d$Fd}, Land Use p: {m_DSi.d$Fl} \n \n CB-PF: {fn_quickNum(m_DSi.d.tTest$p.value[1],4)} | PF-FG: {fn_quickNum(m_DSi.d.tTest$p.value[4],4)} | CB-FG: {fn_quickNum(m_DSi.d.tTest$p.value[2],4)}")
  )

p102
  
p103 <- ((p101 + p102) + 
         plot_layout(
           guides = "collect"
         ) + 
         plot_annotation(
           tag_levels = "a",
           # title = TeX("$\\bf{Plant\\,Available\\,Silicon}"),
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


################################################################################
#
# Adsorbed Si
#
################################################################################

# Testing Transect/Plot Type Effects
fn_effectTest(data_topSoil, data_topSoil$Si_Acetic)
## Transects & Plot Type do not have a statistically significant effect (p>0.1)
## Therefore, data$topSoil samples can be used for Si_Acetic to compare potential land use effects

# Top Soil Effects
p104 <- ggplot(data = data_topSoil, mapping = aes(x = Land_Use.f, y=Si_Acetic, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (90,95,90)
  ) +
  fn_fillScale() +
  fn_yLim(0,100) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    subtitle = glue("Top Soil Samples | n={nrow(data_topSoil)}"),
    caption = glue("Soil Pit Samples: {nrow(filter(data_topSoil, Type == 'Pit'))}; Microplot Samples: {nrow(filter(data_topSoil, Type == 'Microplot'))}")
  ) +
  guides(fill = "none")

p104

model_AdSi.d <- lm(Si_Acetic ~ Depth_Avg+Land_Use.f, data = data_depth)
m_AdSi.d.summary <- summary(model_AdSi.d)
m_AdSi.d.Anova <- Anova(model_AdSi.d, type = c("III"))

m_AdSi.d.summary
m_AdSi.d.Anova

fn_statTest(model_AdSi.d)

m_AdSi.d.tTest <- pairwise.t.test(x = data_depth$Si_Acetic,
                                 g = data_depth$Land_Use.f,
                                 p.adjust.method = "bonferroni"
)

m_AdSi.d.b <- as.numeric(model_AdSi.d$coefficients[1])
m_AdSi.d.m <- as.numeric(model_AdSi.d$coefficients[2])
m_AdSi.d.a1 <- as.numeric(model_AdSi.d$coefficients[3])
m_AdSi.d.a2 <- as.numeric(model_AdSi.d$coefficients[4])
m_AdSi.d.F <- fn_quickNum(fn_simpleF(m_AdSi.d.summary),4)
m_AdSi.d.Fd <- fn_quickNum(m_AdSi.d.Anova$`Pr(>F)`[2], 4)
m_AdSi.d.Fl <- fn_quickNum(m_AdSi.d.Anova$`Pr(>F)`[3], 4)

p105 <- ggplot(data = data_depth, mapping = aes(x = Si_Acetic, y = Depth_Jitter, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  geom_abline(aes(intercept = (m_AdSi.d.b/m_AdSi.d.m), slope = -1/(m_AdSi.d.m), colour = "0"), linetype = 2) +
  geom_abline(aes(intercept = ((m_AdSi.d.b+m_AdSi.d.a1)/m_AdSi.d.m), slope = -1/m_AdSi.d.m, colour = "1"), linetype = 4) +
  geom_abline(aes(intercept = ((m_AdSi.d.b+m_AdSi.d.a2)/m_AdSi.d.m), slope = -1/m_AdSi.d.m, colour = "2"), linetype = 5) +
  fn_slrScale() +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,250) +
  labs(
    subtitle = "Soil Pit Samples | n=24",
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    caption = glue("Model p: {m_AdSi.d.F}, Depth p: {m_AdSi.d.Fd}, Land Use p: {m_AdSi.d.Fl} \n \n CB-PF: {fn_quickNum(m_AdSi.d.tTest$p.value[1],4)} | PF-FG: {fn_quickNum(m_AdSi.d.tTest$p.value[4],4)} | CB-FG: {fn_quickNum(m_AdSi.d.tTest$p.value[2],4)}")
  )

p105

p106 <- ((p104 + p105) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             # title = TeX("$\\bf{Adsorbed\\,Silicon}"),
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

################################################################################
#
# Weakly Crystaline Si (Oxalate Extractable)
#
################################################################################

# Testing Transect/Plot Type Effects
fn_effectTest(data_topSoil, data_topSoil$Oxa_Si)
fn_effectTest(data_topSoil, data_topSoil$Oxa_Al)
fn_effectTest(data_topSoil, data_topSoil$Oxa_AlSi)
## Transects & Plot Type do not have a statistically significant effect (p>0.1)
## Therefore, data$topSoil samples can be used for Oxalate Extractable Al and Si to compare potential land use effects

# Top Soil Effects
p107 <- ggplot(data = data_topSoil, mapping = aes(x = Land_Use.f, y=Oxa_Si, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (4000,4250,4000)
  ) +
  fn_fillScale() +
  fn_yLim(0,4500) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    subtitle = glue("Top Soil Samples | n={nrow(data_topSoil)}"),
    caption = glue("Soil Pit Samples: {nrow(filter(data_topSoil, Type == 'Pit'))}; Microplot Samples: {nrow(filter(data_topSoil, Type == 'Microplot'))}")
  ) +
  guides(fill = "none")

p107

model_WkSi.d <- lm(Oxa_Si ~ Depth_Avg+Land_Use.f, data = data_depth)
m_WkSi.d.summary <- summary(model_WkSi.d)
m_WkSi.d.Anova <- Anova(model_WkSi.d, type = c("III"))

m_WkSi.d.summary
m_WkSi.d.Anova

fn_statTest(model_WkSi.d)

m_WkSi.d.tTest <- pairwise.t.test(x = data_depth$Oxa_Si,
                                  g = data_depth$Land_Use.f,
                                  p.adjust.method = "bonferroni"
)

m_WkSi.d.tTest

m_WkSi.d.b <- as.numeric(model_WkSi.d$coefficients[1])
m_WkSi.d.m <- as.numeric(model_WkSi.d$coefficients[2])
m_WkSi.d.a1 <- as.numeric(model_WkSi.d$coefficients[3])
m_WkSi.d.a2 <- as.numeric(model_WkSi.d$coefficients[4])
m_WkSi.d.F <- fn_quickNum(fn_simpleF(m_WkSi.d.summary),4)
m_WkSi.d.Fd <- fn_quickNum(m_WkSi.d.Anova$`Pr(>F)`[2], 4)
m_WkSi.d.Fl <- fn_quickNum(m_WkSi.d.Anova$`Pr(>F)`[3], 4)

p108 <- ggplot(data = data_depth, mapping = aes(x = Oxa_Si, y = Depth_Jitter, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  geom_abline(aes(intercept = (m_WkSi.d.b/m_WkSi.d.m), slope = -1/(m_WkSi.d.m), colour = "1"), linetype = 2) +
  geom_abline(aes(intercept = ((m_WkSi.d.b+m_WkSi.d.a1)/m_WkSi.d.m), slope = -1/m_WkSi.d.m, colour = "2"), linetype = 4) +
  geom_abline(aes(intercept = ((m_WkSi.d.b+m_WkSi.d.a2)/m_WkSi.d.m), slope = -1/m_WkSi.d.m, colour = "3"), linetype = 5) +
  fn_slrScale() +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,15000) +
  labs(
    subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}"),
    x = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    caption = glue("Model p: {m_WkSi.d.m}, Depth p: {m_WkSi.d.Fd}, Land Use p: {m_WkSi.d.Fl} \n \n CB-PF: {fn_quickNum(m_WkSi.d.tTest$p.value[1],4)} | PF-FG: {fn_quickNum(m_WkSi.d.tTest$p.value[4],4)} | CB-FG: {fn_quickNum(m_WkSi.d.tTest$p.value[2],4)}")
  )

p108

p109 <- ((p107 + p108) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             # title = TeX("$\\bf{Weakly\\,Crystaline\\,Silicon}"),
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

#
# Amorphous Aluminum
#

model_WkAl.d <- lm(Oxa_Al ~ Depth_Avg+Land_Use.f, data = data_depth)
m_WkAl.d <- c()
m_WkAl.d.summary <- summary(model_WkAl.d)
m_WkAl.d.Anova <- Anova(model_WkAl.d, type = c("III"))

m_WkAl.d.summary
m_WkAl.d.Anova

m_WkAl.d$tTest <- pairwise.t.test(x = data_depth$Oxa_Si,
                                  g = data_depth$Land_Use.f,
                                  p.adjust.method = "bonferroni"
)

m_WkAl.d$tTest

m_WkAl.d$b <- as.numeric(model_WkAl.d$coefficients[1])
m_WkAl.d$m <- as.numeric(model_WkAl.d$coefficients[2])
m_WkAl.d$a1 <- as.numeric(model_WkAl.d$coefficients[3])
m_WkAl.d$a2 <- as.numeric(model_WkAl.d$coefficients[4])
m_WkAl.d$F <- fn_quickNum(fn_simpleF(m_WkAl.d.summary),4)
m_WkAl.d$Fd <- fn_quickNum(m_WkAl.d.Anova$`Pr(>F)`[2], 4)
m_WkAl.d$Fl <- fn_quickNum(m_WkAl.d.Anova$`Pr(>F)`[3], 4)

p110 <- ggplot(data = data_depth, mapping = aes(x = Oxa_Al, y = Depth_Jitter, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(size = 3, colour = "black") +
  geom_abline(aes(intercept = (m_WkAl.d$b/m_WkAl.d$m), slope = -1/(m_WkAl.d$m), colour = "1"), linetype = 2) +
  geom_abline(aes(intercept = ((m_WkAl.d$b+m_WkAl.d$a1)/m_WkAl.d$m), slope = -1/m_WkAl.d$m, colour = "2"), linetype = 4) +
  geom_abline(aes(intercept = ((m_WkAl.d$b+m_WkAl.d$a2)/m_WkAl.d$m), slope = -1/m_WkAl.d$m, colour = "3"), linetype = 5) +
  fn_slrScale() +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLimR(0,80) +
  fn_xLim(0,40000) +
  labs(
    subtitle = "Soil Pit Samples | n=24",
    x = TeX("$\\bf{Al\\,Concentration}\\,(mg\\,kg^{-1})"),
    y = TeX("$\\bf{Sampling\\,Depth}\\,(cm)"),
    caption = glue("Model p: {m_WkAl.d$F}, Depth p: {m_WkAl.d$Fd}, Land Use p: {m_WkAl.d$Fl} \n \n CB-PF: {fn_quickNum(m_WkAl.d$tTest$p.value[1],4)} | PF-FG: {fn_quickNum(m_WkAl.d$tTest$p.value[4],4)} | CB-FG: {fn_quickNum(m_WkAl.d$tTest$p.value[2],4)}")
  )

p110

p111 <- ggplot(data = data_topSoil, mapping = aes(x = Land_Use.f, y=Oxa_AlSi, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_jitter(colour = "black", size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (35,37.5,35)
  ) +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLim(0,50) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Al}$:$\\bf{Si\\,Ratio}"),
    subtitle = glue("Top Soil Samples | n={nrow(data_topSoil)}"),
    caption = glue("Soil Pit Samples: {nrow(filter(data_topSoil, Type == 'Pit'))}; Microplot Samples: {nrow(filter(data_topSoil, Type == 'Microplot'))}")
  )

p111

p112 <- ggplot(data = data_depth, mapping = aes(x = Land_Use.f, y=Oxa_AlSi, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_jitter(colour = "black", size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (22.5,25,22.5)
  ) +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLim(0,30) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Al}$:$\\bf{Si\\,Ratio}"),
    subtitle = glue("Soil Pit Samples | n={nrow(data_depth)}")
  )

p112

p113 <- ((p110 + (p112 / p111)) + 
           plot_layout(
             guides = "collect"
           ) + 
           plot_annotation(
             tag_levels = "a",
             # title = TeX("$\\bf{Oxalate\\,Extractable\\,Aluminum}"),
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

################################################################################
#
# Amorphous Si
#
################################################################################

# Testing Transect/Plot Type Effects
fn_effectTest(data_topSoil, data_topSoil$Si_kin)
fn_effectTest(data_topSoil, data_topSoil$Al_Kin)
fn_effectTest(data_topSoil, data_topSoil$SiAl_kin)
## Transects & Plot Type do not have a statistically significant effect (p>0.1)
## Therefore, data$topSoil samples can be used for Oxalate Extractable Al and Si to compare potential land use effects


p114 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=Si_kin, fill = Land_Use.f)) +
  geom_boxplot(colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (4250,4500,4250)
  ) +
  fn_fillScale() +
  fn_yLim(0,5000) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si\\,Concentration}\\,(mg\\,kg^{-1})"),
    subtitle = glue("Microplot Samples | n={nrow(data_landUse)}")
  ) +
  guides(fill = "none")

p114

p115 <- ggplot(data = data_landUse, mapping = aes(x = Land_Use.f, y=SiAl_kin, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_jitter(colour = "black", size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  stat_pwc(
    method = "t.test",
    p.adjust.method = "bonferroni",
    label = "p.adj.format",
    tip.length = 0,
    bracket.shorten = 0.1,
    y.position = c (3.25,3.5,3.25)
  ) +
  fn_fillScale() +
  fn_shapeScale() +
  fn_yLim(0,4) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Si}$:$\\bf{Al\\,Ratio}"),
    subtitle = glue("Microplot Samples | n={nrow(data_landUse)}")
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
