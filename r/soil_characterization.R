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
rm(list=ls(all=TRUE))  # Remove all objects left from previous runs of R
source("./r/KFN_initialization.R") # Loads Functions & Datasets

################################################################################
#
# Q: Can microplot samples be grouped with top soil samples for 
# statistical analysis of pH?
#
################################################################################
#
# NOTE: Last run on April 24, 2026
#
# fn_effectTest(data_t, data_t$pH_H2O)    #  Transect P: 0.498
#                                           # Plot Type P: 0.002
# 
# fn_effectTest(data_t, data_t$pH_CaCl2)   #  Transect P: 0.395
#                                           # Plot Type P: 0.017
# CONCLUSION:
# Top Soil Samples (Depth 0-15) and Microplot Samples can not be grouped together
# during the statistical analysis of pH in soils
#
################################################################################
#
# Microplot pH in H2O, 0.1M CaCl2, and Delta pH
#
################################################################################

aggregate(pH_H2O ~ Land_Use.f, data_m, mean)
aggregate(pH_H2O ~ Land_Use.f, data_m, sd)

aggregate(pH_CaCl2 ~ Land_Use.f, data_m, mean)
aggregate(pH_CaCl2 ~ Land_Use.f, data_m, sd)

aggregate(pH_Delta ~ Land_Use.f, data_m, mean)
aggregate(pH_Delta ~ Land_Use.f, data_m, sd)

p_micro_pH_001 = qp_landUse(data_m, data_m$pH_H2O, baselineValue = c(3.8,4.4,4.5,5.5,4.5,4.5,4.8), Lim = c(2.5,7.0,0.5), labelBaseline = FALSE) + 
  fn_statCompare(c(6,6.25,6)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{pH\\,\\i\\n\\,H_{2}O}$")
  )
p_micro_pH_002 = qp_landUse(data_m, data_m$pH_CaCl2, baselineValue = c(3.5,4.1,3.9,4.6), Lim = c(2.5,7.0,0.5), labelBaseline = FALSE) + 
  fn_statCompare(c(4.25,4.5,4.25)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{pH\\,\\i\\n\\,0.01M\\,CaCl_{2}}$")
  )
p_micro_pH_003 = qp_landUse(data_m, data_m$pH_Delta, Lim = c(0,2.5,0.5), labelBaseline = FALSE) + 
  fn_statCompare(c(2,2.25,2)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{\\Delta pH}\\,(pH_{H_{2}O}-pH_{CaCl_{2}})$")
  )

p_micro_pH = (p_micro_pH_001 + p_micro_pH_002 + p_micro_pH_003) + 
  plot_annotation(
    tag_levels = "A",
    subtitle = qp_nMicroplot,
    theme = qp_theme
  )
p_micro_pH
fn_quickSave(p_micro_pH)

################################################################################
#
# Soil Pit pH in H2O, 0.1M CaCl2, and Delta pH
#
################################################################################

model_pH_H2O.p = lm(pH_H2O ~ Depth_Avg, data = data_p) # No Interaction / No Depth or Land Use Effect
fn_quickSummary(model_pH_H2O.p)

model_pH_CaCl2.p = lm(pH_CaCl2 ~ Depth_Avg, data_p) # No Interaction / Depth Effect but no Land Use Effect
fn_quickSummary(model_pH_CaCl2.p)

model_pH_Delta.m = lm(pH_Delta ~ Depth_Avg, data_p) # No interaction, but subjectively there is an interaction...
fn_quickSummary(model_pH_Delta.m)

pairwise.t.test(
  x = data_p$pH_H2O,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = data_p$pH_CaCl2,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = data_p$pH_Delta,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)

p_pit_pH_001 = qp_depth(data_p, data_p$pH_H2O, Lim = c(3,6,0.5)) +
  qp_lmANOVA(model_pH_H2O.p)$lines +
  labs(
    x = TeX("$\\bf{pH\\,\\i\\n\\,H_{2}O}$"),
    caption = qp_lmANOVA(model_pH_H2O.p)$caption
  )

p_pit_pH_002 = qp_depth(data_p, data_p$pH_CaCl2, Lim = c(3,6,0.5)) +
  qp_lmANOVA(model_pH_CaCl2.p)$lines +
  labs(
    x = TeX("$\\bf{pH\\,\\i\\n\\,0.01M\\,CaCl_{2}}$"),
    caption = qp_lmANOVA(model_pH_CaCl2.p)$caption
  )

p_pit_pH_003 = qp_depth(data_p, data_p$pH_Delta, Lim = c(0,2,0.5)) +
  qp_lmANOVA(model_pH_Delta.m)$lines +
  labs(
    x = TeX("$\\bf{\\Delta pH}\\,(pH_{H_{2}O}-pH_{CaCl_{2}})$"),
    caption = qp_lmANOVA(model_pH_Delta.m)$caption
  )


p_pit_pH = ((p_pit_pH_001 + p_pit_pH_002 + p_pit_pH_003)/(guide_area())) + 
  plot_layout(guides = "collect", heights = c(4,1)) & 
  theme(legend.position = 'bottom', legend.direction = "horizontal") & 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) & 
  plot_annotation(
    tag_levels = "A",
    subtitle = qp_nPits,
    theme = qp_theme
  )

p_pit_pH

fn_quickSave(p_pit_pH)

################################################################################
#
# Q: Can microplot samples be grouped with top soil samples for 
# statistical analysis of CEC, exchangeable ions, and base saturation?
#
################################################################################
#
# NOTE: Last run on April 27, 2026
#
# fn_effectTest(data_t, data_t$CEC)                 #  Transect P: 0.408
#                                                   # Plot Type P: 0.089 --> Possibly significant
#
# fn_effectTest(data_t, data_t$BaseSaturation)      #  Transect P: 0.244
#                                                   # Plot Type P: 0.472 --> Not significant
# 
# fn_effectTest(data_t, data_t$Exc_Ca)              #  Transect P: 0.211
#                                                   # Plot Type P: 0.540 
# 
# fn_effectTest(data_t, data_t$Exc_K)               #  Transect P: 0.384
#                                                   # Plot Type P: 0.001 *** --> Many of the soil pit samples do not have Exc_K in them, while the micro plots do
# 
# fn_effectTest(data_t, data_t$Exc_Mg)              #  Transect P: 0.772
#                                                   # Plot Type P: 0.134
# 
# fn_effectTest(data_t, data_t$Exc_Mn)              #  Transect P: 0.240
#                                                   # Plot Type P: 0.226
# 
# fn_effectTest(data_t, data_t$Exc_Na)              #  Transect P: NaN --> <DL
#                                                   # Plot Type P: NaN --> <DL
# 
# fn_effectTest(data_t, data_t$Exc_Fe)              #  Transect P: 0.775
#                                                   # Plot Type P: 0.002
# 
# fn_effectTest(data_t, data_t$Exc_Al)              #  Transect P: 0.165
#                                                   # Plot Type P: 0.155
# 
# fn_effectTest(data_t, data_t$Exc_H)               #  Transect P: 0.532
#                                                   # Plot Type P: 0.116
# 
# CONCLUSION:
# Top Soil Samples (Depth 0-15) and Microplot Samples can not be grouped together
# during the statistical analysis of base saturation in soils
#
################################################################################
#
# Cation Exchange Capacity
#
################################################################################


tempLabel = TeX("$\\bf{CEC}\\,(cmol_{c}\\,kg^{-1})$")

p_micro_CEC = qp_landUse(data_m, data_m$CEC, baselineValue = c(5.3,13), Lim = c(0,16,2)) +
  fn_statCompare(c(13,14,13)) +
  labs(
    subtitle = qp_nMicroplot,
    y = tempLabel
  )

p_micro_CEC
fn_quickSave(p_micro_CEC)

aggregate(CEC ~ Land_Use.f, data_m, mean)
aggregate(CEC ~ Land_Use.f, data_m, sd)

aggregate(CEC ~ Depth_Avg, data_p, mean)
aggregate(CEC ~ Depth_Avg, data_p, sd)

pairwise.t.test(
  x = data_p$CEC,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)

# model_CEC.d <- lm(CEC ~ Depth_Avg*Land_Use.f, data = data_p)
# 
# fn_statTest(model_CEC.d,                    #      Linearity: No curvilinear relationship
#             testTheme = qp_theme,           # Equal Variance: Equal variance!
#             saveTest = TRUE)                #      Normality: Normally distributed! 
# 
# summary(model_CEC.d)                        # Regression is almost statistically significant --> p = 0.6439 
#                                             # R^2 = 0.4155 --> Okayy
#                                             # Adj_R^2 = 0.2531 (bad)
# 
# Anova(model_CEC.d, type = c("III"))         #    Depth p = 0.205 --> Not Significant
#                                             # Land_Use p = 0.974 --> Very insignificant
#                                             # Interact p = 0.769 --> Insignificant! --> Can simplify model
# 
# model_CEC.d <- lm(CEC ~ Depth_Avg+Land_Use.f, data = data_p)
# 
# fn_statTest(model_CEC.d,                    #      Linearity: No curvilinear relationship
#             testTheme = qp_theme,           # Equal Variance: Equal variance!
#             saveTest = TRUE)                #      Normality: Normally distributed! 
# 
# summary(model_CEC.d)                        # Regression is almost statistically significant --> p = 0.0155 
#                                             # R^2 = 0.3982 --> Okayy
#                                             # Adj_R^2 = 0.3079 (bad)
# 
# Anova(model_CEC.d, type = c("III"))         #    Depth p = 0.002 --> Significant!
#                                             # Land_Use p = 0.487 --> Insignificant --> Simplify Model

model_CEC.d <- lm(CEC ~ Depth_Avg, data = data_p)

fn_statTest(model_CEC.d,                    #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Equal variance!
            saveTest = TRUE)                #      Normality: Normally distributed! 

summary(model_CEC.d)                        # Regression is almost statistically significant --> p = 0.00219
                                            # R^2 = 0.3534 --> Okayy
                                            # Adj_R^2 = 0.324 (okayish)

p_pit_CEC = qp_depth(data_p, data_p$CEC, Lim = c(0,10,1)) +
  qp_lmANOVA(model_CEC.d)$lines +
  labs(
    subtitle = qp_nPits,
    x = tempLabel,
    caption = qp_lmANOVA(model_CEC.d)$caption
  )

p_pit_CEC
fn_quickSave(p_pit_CEC)

model_CEC_driver = lm(CEC ~ model_perSand+pH_CaCl2, data_p)
fn_quickSummary(model_CEC_driver)
# 
# axis_x = seq(min(data_p$model_perSand), max(data_p$model_perSand), length.out = 50)
# axis_y = seq(min(data_p$pH_CaCl2), max(data_p$pH_CaCl2), length.out = 50)
# 
# z_matrix = t(outer(axis_x, axis_y, function(x,y){
#   predict(model_CEC_driver, data.frame(model_perSand = x, pH_CaCl2 = y))
# }))
# 
# testFig = plot_ly(x = ~axis_x, y = ~axis_y, z = ~z_matrix) %>% 
#   add_surface() %>%
#   layout(scene = list(
#     xaxis = list(title = "Sand"),
#     yaxis = list(title = "pH in 0.01M CaCl2"),
#     zaxis = list(title = "CEC")
#   ))
# testFig

rm(tempLabel)

################################################################################
#
# Base Saturation
#
################################################################################

tempLabel = TeX("$\\bf{Base\\,Saturation}\\,(\\%\\,Base\\,Forming\\,Ions)$")

p_micro_BaseSaturation = qp_landUse(data_m, data_m$BaseSaturation*100, Lim = c(0,50,10)) +
  fn_statCompare(c(32.5,37.5,32.5)) +
  labs(
    subtitle = qp_nMicroplot,
    y = tempLabel
  )

p_micro_BaseSaturation
fn_quickSave(p_micro_BaseSaturation)

aggregate(BaseSaturation ~ Land_Use.f, data_m, mean)
aggregate(BaseSaturation ~ Land_Use.f, data_m, sd)
aggregate(BaseSaturation ~ Land_Use.f, data_m, max)

aggregate(BaseSaturation ~ Depth_Avg, data_p, mean)
aggregate(BaseSaturation ~ Depth_Avg, data_p, sd)

pairwise.t.test(
  x = data_p$BaseSaturation,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)


model_BS.m <- lm(BaseSaturation*100 ~ Depth_Avg*Land_Use.f, data = data_p)

fn_statTest(model_BS.m,                     #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Equal variance!
            saveTest = TRUE)                #      Normality: Normally distributed!

summary(model_BS.m)                         # Regression is almost statistically significant --> p = 0.0892
                                            # R^2 = 0.3887 --> Okayy
                                            # Adj_R^2 = 0.2189 (bad)

Anova(model_BS.m, type = c("III"))          #    Depth p = 0.718 --> Not Significant
                                            # Land_Use p = 0.453 --> Very insignificant
                                            # Interact p = 0.886 --> Insignificant! --> Can simplify model

model_BS.p <- lm(log10(BaseSaturation) ~ Depth_Avg+Land_Use.f, data = data_p)

fn_statTest(model_BS.p,                     #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Weird variance --> Equal at high and low, but lower variance at intermediate values
            saveTest = TRUE)                #      Normality: Normally distributed!

summary(model_BS.p)                         # Regression is statistically significant p = 0.02034
                                            # R^2 = 0.3804 --> Okayy
                                            # Adj_R^2 = 0.2875 (bad)

Anova(model_BS.p, type = c("III"))          #    Depth p = 0.389 --> Insignificant --> Simplify Model
                                            # Land_Use p = 0.010 --> Significant 

model_BS.s <- lm(BaseSaturation ~ Depth_Avg, data = data_p)

fn_statTest(model_BS.s,                     #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Equal variance!
            saveTest = TRUE)                #      Normality: Normally distributed! 

summary(model_BS.s)                         # Regression is almost statistically significant --> p = 0.00219
                                            # R^2 = 0.3534 --> Okayy
                                            # Adj_R^2 = 0.324 (okayish)

p_pit_BaseSaturation = qp_depth(data_p, data_p$BaseSaturation*100, Lim = c(0,70,10)) +
  qp_lmANOVA(model_BS.m)$lines + 
  labs(
    subtitle = qp_nPits,
    x = tempLabel,
    caption = qp_lmANOVA(model_BS.m)$caption
  )

p_pit_BaseSaturation
fn_quickSave(p_pit_BaseSaturation)

rm(tempLabel)

################################################################################
#
# Exchangeable Ions
#
################################################################################

################################################################################
#
# Q: Can microplot samples be grouped with top soil samples for 
# statistical analysis of plant available nutrients?
#
################################################################################
#
# NOTE: Last run on April 27, 2026
#
# fn_effectTest(data_t, data_t$pan_P)               #  Transect P: 0.916
#                                                   # Plot Type P: 0.253
# 
# fn_effectTest(data_t, data_t$pan_Ca)              #  Transect P: 0.217
#                                                   # Plot Type P: 0.609
# 
# fn_effectTest(data_t, data_t$pan_Mg)              #  Transect P: 0.857
#                                                   # Plot Type P: 0.104 ** FLAG FOR FUTURE SAMPLES
# 
# fn_effectTest(data_t, data_t$pan_K)               #  Transect P: 0.397
#                                                   # Plot Type P: 0.010 ** SIGNIFICANT
#
# CONCLUSION:
# Top Soil Samples (Depth 0-15) and Microplot Samples can not be grouped together
# during the statistical analysis of plant available nutrients as Mg and K may
# have statistically significant effects of sampling type on PAN
#
################################################################################
#
# Plant Available P, K, Ca, Mg in Microplot Samples
#
################################################################################

p_micro_P = qp_landUse(data_m, data_m$pan_P, baselineValue = c(2.5,5.8), Lim = c(0,100,10), labelBaseline = FALSE) +
  fn_statCompare(c(50,55,50)) +
  qp_shortName() +
  labs(
    # subtitle = qp_nMicroplot,
    y = TeX("$\\bf{PAN\\,P}\\,(mg\\,kg^{-1})$")
  )
p_micro_P


p_micro_Ca = qp_landUse(data_m, data_m$pan_Ca, Lim = c(0,700,100), labelBaseline = FALSE) +
  fn_statCompare(c(200,250,200)) +
  qp_shortName() +
  labs(
    # subtitle = qp_nMicroplot,
    y = TeX("$\\bf{PAN\\,Ca}\\,(mg\\,kg^{-1})$")
  )
p_micro_Ca


p_micro_Mg = qp_landUse(data_m, data_m$pan_Mg, Lim = c(0,150,25), labelBaseline = FALSE) +
  fn_statCompare(c(115,130,115)) +
  qp_shortName() +
  labs(
    # subtitle = qp_nMicroplot,
    y = TeX("$\\bf{PAN\\,Mg}\\,(mg\\,kg^{-1})$")
  )
p_micro_Mg


p_micro_K = qp_landUse(data_m, data_m$pan_K, Lim = c(0,150,25), labelBaseline = FALSE) +
  fn_statCompare(c(125,135,125)) +
  qp_shortName() +
  labs(
    # subtitle = qp_nMicroplot,
    y = TeX("$\\bf{PAN\\,K}\\,(mg\\,kg^{-1})$")
  )
p_micro_K


p_micro_pan_macroNutrients = ((p_micro_P + p_micro_K)/(p_micro_Ca + p_micro_Mg)) +
  plot_annotation(
    tag_levels = "A",
    subtitle = qp_nMicroplot,
    theme = qp_theme
  )
p_micro_pan_macroNutrients
fn_quickSave(p_micro_pan_macroNutrients, saveHeight = 12)

################################################################################
#
# Plant Available Cu, Mn, Na, S and Zn in Microplot Samples
#
################################################################################

p_micro_Mn = qp_landUse(data_m, data_m$pan_Mn, Lim = c(0,100,10), labelBaseline = FALSE) +
  fn_statCompare(c(75,85,75)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{PAN\\,Mn}\\,(mg\\,kg^{-1})$")
  )
  guides(fill = guide_legend())
p_micro_Mn


p_micro_Na = qp_landUse(data_m, data_m$pan_Na, Lim = c(0,50,10), labelBaseline = FALSE) +
  fn_statCompare(c(35,40,35)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{PAN\\,Na}\\,(mg\\,kg^{-1})$")
  )
p_micro_Na


p_micro_S = qp_landUse(data_m, data_m$pan_S, Lim = c(0,50,10), labelBaseline = FALSE) +
  fn_statCompare(c(35,40,35)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{PAN\\,S}\\,(mg\\,kg^{-1})$")
  )
p_micro_S


p_micro_Zn = qp_landUse(data_m, data_m$pan_Zn, Lim = c(0,10,1), labelBaseline = FALSE) +
  fn_statCompare(c(4,5,4)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{PAN\\,Zn}\\,(mg\\,kg^{-1})$")
  )
p_micro_Zn


p_micro_Cu = qp_landUse(data_m, data_m$pan_Cu, baselineValue = c(0.5,1), Lim = c(0,10,1), labelBaseline = FALSE) +
  fn_statCompare(c(2.5,3.5,2.5)) +
  qp_shortName() +
  labs(
    y = TeX("$\\bf{PAN\\,Cu}\\,(mg\\,kg^{-1})$")
  )
p_micro_Cu



p_micro_pan_microNutrients = (p_micro_Mn + guide_area() + p_micro_Na + p_micro_S + p_micro_Zn + p_micro_Cu) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) + 
  plot_annotation(
    tag_levels = "A",
    subtitle = qp_nMicroplot,
    theme = qp_theme
  )
p_micro_pan_microNutrients
fn_quickSave(p_micro_pan_microNutrients, saveHeight = 18)


################################################################################


################################################################################
#
# Plant Available P, K, Ca, Mg in Soil Pit Samples
#
################################################################################

fn_Element.d = function(element, type = 2){
  model = c()
  
  if(type == 3){
    model = lm(element ~ Depth_Avg*Land_Use.f, data_p)
  } else if(type == 2){
    model = lm(element ~ Depth_Avg+Land_Use.f, data_p)
  } else if(type == 1){
    model = lm(element ~ Depth_Avg, data_p)
  }
  
  print(fn_quickSummary(model))
  print("<> Paired T-Tests <>")
  print("<> <> <> <> <> <> <>")
  print(
  pairwise.t.test(
    x = element,
    g = data_p$Depth_Avg,
    p.adjust.method = "bonferroni"
  ))
  print(
  pairwise.t.test(
    x = element,
    g = data_p$Land_Use.f,
    p.adjust.method = "bonferroni"
  ))
}

fn_Element.d(log(data_p$pan_P+1), 2)
fn_Element.d(data_p$pan_K, 2)
fn_Element.d(log(data_p$pan_Ca+1), 2)
fn_Element.d(data_p$pan_Mg, 2)
fn_Element.d(data_p$pan_Mn, 2)
fn_Element.d(data_p$pan_Na, 2)
fn_Element.d(data_p$pan_S, 2)
fn_Element.d(data_p$pan_Zn, 2)
fn_Element.d(data_p$pan_Cu, 2)

#         |           |      |     Model      |
# Element | Transform | Int. |   p   |   R2   | Depth | Land Use | T-Test Differences
# --------+-----------+------+-------+--------+-------+----------+------------------------
# pan_P   | Log10     | No   | 0.030 | 0.3547 | 0.090 | 0.037    | No differences, PF-FG 0.51   
# pan_K   | None      | No   | 0.001 | 0.4272 | 0.006 | 0.083    | TopSoil != Lower Layers      [ DEPTH ]
# pan_Ca  | Log10     | No   | 0.005 | 0.4632 | 0.001 | 0.019    | No differences, PF-FG = 0.53 
# pan_Mg  | None      | No   | 0.008 | 0.4356 | 0.002 | 0.355    | TopSoil != Lower Layers      [ DEPTH ]
# pan_Mn  | None      | No   | ~0    | 0.6536 | 0.041 | ~0       | PF & FG != CB                [ LANDM ] LOWER
# pan_Na  | None      | No   | 0.002 | 0.5202 | 0.004 | 0.011    | PF != FG                     [ LANDM ] LOWER
# pan_S   | None      | No   | ~0    | 0.6258 | 0.009 | ~0       | CB & FG != PF                [ LANDM ] HIGHER
# Pan_Zn  | None      | No   | 0.022 | 0.3734 | 0.006 | 0.297    | Topsoil != 55-75             [ DEPTH ]
# pan_Cu  | None      | No   | ~0    | 0.6531 | 0.124 | ~0       | FG != CB & PF                [ LANDM ] LOWER (0)


################################################################################
#
# Total Elements
#
# Note: Only analyzed on soil pit samples, do do not need to test for
# the effect of sampling on concentrations
#
################################################################################

data_p_longer = data_p %>%
  pivot_longer(
    cols = c(
      total_Al, 
      total_Ca,
      total_Cu,
      total_Fe,
      total_K,
      total_Mg,
      total_Mn,
      total_Na,
      total_P,
      total_Zn
    ),
    names_to = "element",
    values_to = "concentration"
  )

tempLabelX = scale_x_discrete(
  TeX("$\\bf{Element}$"),
  labels = c(
    "total_Al" = "Al", 
    "total_Ca" = "Ca",
    "total_Cu" = "Cu",
    "total_Fe" = "Fe",
    "total_K" = "K",
    "total_Mg" = "Mg",
    "total_Mn" = "Mn",
    "total_Na" = "Na",
    "total_P" = "P",
    "total_Zn" = "Zn"
  )
)

lm(concentration ~ Depth_Avg, data = data_p_longer)

p_pit_totalElements_000 = ggplot(data_p_longer, aes(x = element, y = concentration, shape = Land_Use.f, fill = Land_Use.f)) +
  geom_point(size = 3) +
  fn_shapeScale() +
  fn_fillScale() +
  scale_y_log10(limits = c(1,100000), breaks = 10**seq(0,5, length.out = 6), expand = c(0,0)) +
  tempLabelX +
  labs(
    subtitle = qp_nPits,
    y = TeX("$\\bf{Tota\\,Concentration}\\,(mg\\,kg^{-1})$")
  )

p_pit_totalElements_000

p_pit_totalElements_001 = ggplot(filter(data_p_longer, Depth_Avg == 7.5), aes(x = element, y = concentration, shape = Land_Use.f, fill = Land_Use.f)) +
  geom_point(size = 3) +
  fn_shapeScale() +
  fn_fillScale() +
  scale_y_log10(limits = c(1,50000), breaks = c(10**seq(0,5, length.out = 6)/2,10**seq(0,5, length.out = 6)), expand = c(0,0)) +
  tempLabelX +
  labs(
    subtitle = "0-15cm",
    y = TeX("$\\bf{Tota\\,Concentration}\\,(mg\\,kg^{-1})$")
  )

p_pit_totalElements_002 = ggplot(filter(data_p_longer, Depth_Avg == 25), aes(x = element, y = concentration, shape = Land_Use.f, fill = Land_Use.f)) +
  geom_point(size = 3) +
  fn_shapeScale() +
  fn_fillScale() +
  scale_y_log10(limits = c(1,50000), breaks = c(10**seq(0,5, length.out = 6)/2,10**seq(0,5, length.out = 6)), expand = c(0,0)) +
  tempLabelX +
  labs(
    subtitle = "15-35cm",
    y = TeX("$\\bf{Tota\\,Concentration}\\,(mg\\,kg^{-1})$")
  )


p_pit_totalElements_003 = ggplot(filter(data_p_longer, Depth_Avg == 45), aes(x = element, y = concentration, shape = Land_Use.f, fill = Land_Use.f)) +
  geom_point(size = 3) +
  fn_shapeScale() +
  fn_fillScale() +
  scale_y_log10(limits = c(1,50000), breaks = c(10**seq(0,5, length.out = 6)/2,10**seq(0,5, length.out = 6)), expand = c(0,0)) +
  tempLabelX +
  labs(
    subtitle = "35-55cm",
    y = TeX("$\\bf{Tota\\,Concentration}\\,(mg\\,kg^{-1})$")
  )


p_pit_totalElements_004 = ggplot(filter(data_p_longer, Depth_Avg == 65), aes(x = element, y = concentration, shape = Land_Use.f, fill = Land_Use.f)) +
  geom_point(size = 3) +
  fn_shapeScale() +
  fn_fillScale() +
  scale_y_log10(limits = c(1,50000), breaks = c(10**seq(0,5, length.out = 6)/2,10**seq(0,5, length.out = 6)), expand = c(0,0)) +
  tempLabelX +
  labs(
    subtitle = "55-75cm",
    y = TeX("$\\bf{Tota\\,Concentration}\\,(mg\\,kg^{-1})$")
  )

p_pit_totalElements_005 = (p_pit_totalElements_001 + p_pit_totalElements_002 + p_pit_totalElements_003 + p_pit_totalElements_004) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) + 
  plot_annotation(
    tag_levels = "A",
    subtitle = qp_nPits,
    theme = qp_theme
  )
p_pit_totalElements_005


fn_Element.d(data_p$total_Al, 2)
fn_Element.d(data_p$total_Ca, 2)
fn_Element.d(data_p$total_Cu, 2)
fn_Element.d(data_p$total_Fe, 2)
fn_Element.d(data_p$total_K, 2)
fn_Element.d(data_p$total_Mg, 2)
fn_Element.d(data_p$total_Mn, 2)
fn_Element.d(data_p$total_Na, 2)
fn_Element.d(data_p$total_P, 2)
fn_Element.d(data_p$total_Zn, 2)

#         |              Model                |
# Element | Int. | Transform |   p   |   R2   | Depth | Land Use | T-Test Differences
# --------+------+-----------+-------+--------+-------+----------+------------------------
# Al      | No   | None      | 0.034 | 0.3445 | 0.019 | 0.161    | None
# Ca      | No   | None      | ~0    | 0.8260 | ~0    | ~0       | PF & FG != CB
# Cu      | No   | None      | 0.032 | 0.3496 | 0.029 | 0.100    | None
# Fe      | No   | None      | 0.281 | 0.1705 | 0.629 | 0.171    | None
# K       | No   | None      | ~0    | 0.7709 | ~0    | ~0       | PF & FG != CB
# Mg      | No   | None      | 0.003 | 0.4994 | 0.005 | 0.170    | None
# Mn      | No   | None      | 0.300 | 0.1646 | 0.177 | 0.389    | None
# Na      | No   | None      | ~0    | 0.6161 | ~0    | 0.004    | Top Soil != Bottom & PF != CB
# P       | No   | None      | 0.035 | 0.3441 | 0.036 | 0.091    | None
# Zn      | No   | None      | 0.004 | 0.4722 | 0.002 | 0.082    | Top Soil != Bottom

