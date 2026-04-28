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
# Q: Can microplot samples be grouped with top soil samples for 
# statistical analysis of Si?
#
################################################################################
# 
# NOTE: Last run on April 13, 2026
# 
# fn_effectTest(data_t, data_t$Si_CaCl2)    #  Transect P: 0.554
#                                           # Plot Type P: 0.573
# 
# fn_effectTest(data_t, data_t$Si_Acetic)   #  Transect P: 0.939
#                                           # Plot Type P: 0.316
# 
# fn_effectTest(data_t, data_t$Oxa_Si)      #  Transect P: 0.932
#                                           # Plot Type P: 0.276
# 
# fn_effectTest(data_t, data_t$Oxa_Al)      #  Transect P: 0.807
#                                           # Plot Type P: 0.331
# 
# fn_effectTest(data_t, data_t$Oxa_AlSi)    #  Transect P: 0.387
#                                           # Plot Type P: 0.783
# 
# fn_effectTest(data_t, data_t$Si_kin)      #  Transect P: 0.172
#                                           # Plot Type P: 0.483
# 
# fn_effectTest(data_t, data_t$Al_kin)      #  Transect P: 0.821
#                                           # Plot Type P: 0.358
# 
# fn_effectTest(data_t, data_t$SiAl_kin)    #  Transect P: 0.997
#                                           # Plot Type P: 0.266
# 
# CONCLUSION:
# Top Soil Samples (Depth 0-15) and Microplot Samples can be grouped together
# during the statistical analysis of Si in soils

################################################################################
#
# Dissolved Silicon
#
################################################################################

# Land Use (Spatial Variation)
p_t_DSi_001 = qp_landUse(data_t, data_t$Si_CaCl2, baselineValue = 5, Lim = c(0,40)) +
  fn_statCompare(c(32.5,35,32.5)) +
  labs(
    subtitle = qp_nTopSoil,
    y = qp_SiCon
  )

p_t_DSi_001
fn_quickSave(p_t_DSi_001)

fn_quickSummary(lm(Si_CaCl2 ~ Land_Use.f, data_t))

aggregate(Si_CaCl2 ~ Land_Use.f, data_t, mean)
aggregate(Si_CaCl2 ~ Land_Use.f, data_t, sd)

aTest = function(variable){
  aggregate(variable ~ Land_Use.f, data_t, mean)
}

quickStat_LandUseMean(Si_CaCl2, data_t)

# model_DSi_perSand = lm(Si_CaCl2 ~ model_perSand, data = data_t)
# 
# data_t_ANCOVA = data_t
# data_t_ANCOVA$DSi = as.numeric(predict(model_DSi_perSand, data_t))
# 
# p_t_DSi_002 = qp_landUse(data_t_ANCOVA, data_t_ANCOVA$DSi)
# p_t_DSi_002
# 
# rm(model_DSi_perSand)
# rm(data_t_ANCOVA)

# Depth (Effect of Depth)

# Depth + Land Use Model
model_DSi.d <- lm(Si_CaCl2 ~ Depth_Avg+Land_Use.f, data = data_p)

summary(model_DSi.d)                       # Regression is statistically significant --> p = 0.001248
                                           # R^2 = 0.538 --> Not great, not terrible
                                           # Adj_R^2 = 0.4687

Anova(model_DSi.d, type = c("III"))        #    Depth p = 0.020490 --> Significant!
                                           # Land_Use p = 0.002156 --> Significant!

fn_statTest(model_DSi.d,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values
            saveTest = TRUE)               #      Normality: Normally distributed

pairwise.t.test(
  x = data_p$Si_CaCl2,
  g = data_p$Land_Use.f,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = data_p$Si_CaCl2,
  g = data_p$Depth_Avg,
  p.adjust.method = "bonferroni"
)

p_p_DSi_002 = qp_depth(data_p, data_p$Si_CaCl2) +
  qp_lmANOVA(model_DSi.d)$lines +
  labs(
    subtitle = qp_nPits,
    x = qp_SiCon,
    caption = qp_lmANOVA(model_DSi.d)$caption
  )

p_p_DSi_002
fn_quickSave(p_p_DSi_002)

################################################################################
#
# Adsorbed Si
#
################################################################################

# Land Use (Spatial Variation)
p_top_AdSi_001 = qp_landUse(data_t, data_t$Si_Acetic, Lim = c(0,125,25)) +
  fn_statCompare(c(100,112.5,100)) +
  labs(
    subtitle = qp_nTopSoil,
    y = qp_SiCon
  )

p_top_AdSi_001
fn_quickSave(p_top_AdSi_001)

# Depth (Effect of Depth)

# Depth + Land Use Model
model_AdSi.d <- lm(Si_Acetic ~ Depth_Avg+Land_Use.f, data = data_p)

summary(model_AdSi.d)                       # Regression is statistically significant --> p = 0.0003994
                                            # R^2 = 0.4784 --> Not great, not terrible
                                            # Adj_R^2 = 0.4002

Anova(model_AdSi.d, type = c("III"))        #    Depth p = 0.001741 --> Significant!
                                            # Land_Use p = 0.095206 --> Possibly Significant!

fn_statTest(model_AdSi.d,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Equal variance across predicted values
            saveTest = TRUE)                #      Normality: Normally distributed, slightly faavours higher end

p_pit_AdSi_002 = qp_depth(data_p, data_p$Si_Acetic, Lim = c(0,250,50)) +
  qp_lmANOVA(model_AdSi.d)$lines +
  labs(
    subtitle = qp_nPits,
    x = qp_SiCon,
    caption = qp_lmANOVA(model_AdSi.d)$caption
  )

p_pit_AdSi_002
fn_quickSave(p_pit_AdSi_002)

################################################################################
#
# Weakly Crystaline Si (Oxalate Extractable)
#
################################################################################

# Land Use (Spatial Variation)
p_top_WkSi_001 = qp_landUse(data_t, data_t$Oxa_Si, baselineValue = 800, Lim = c(0,4500,500)) +
  fn_statCompare(c(4000,4250,4000)) +
  labs(
    subtitle = qp_nTopSoil,
    y = qp_SiCon
  )

p_top_WkSi_001
fn_quickSave(p_top_WkSi_001)

# Depth (Effect of Depth)

# Depth + Land Use Model
# model_WkSi.d <- lm(Oxa_Si ~ Depth_Avg+Land_Use.f, data = data_p)
# 
# summary(model_WkSi.d)                       # Regression is statistically significant --> p = 0.01621
#                                             # R^2 = 0.3953 --> Not great
#                                             # Adj_R^2 = 0.3046
# 
# Anova(model_WkSi.d, type = c("III"))        #    Depth p = 0.02318 --> Significant!
#                                             # Land_Use p = 0.04924 --> Barely Significant!
# 
# fn_statTest(model_WkSi.d,                   #      Linearity: No curvilinear relationship
#             testTheme = qp_theme,           # Equal Variance: Unequal variance across predicted values --> Log or sqrt transform needed
#             saveTest = TRUE)                #      Normality: Normally distributed, but peaks around 0

model_WkSi.d <- lm(log10(Oxa_Si) ~ Depth_Avg+Land_Use.f, data = data_p)

summary(model_WkSi.d)                       # Regression is statistically significant --> p = 0.0005371
                                            # R^2 = 0.5766 --> Solid
                                            # Adj_R^2 = 0.5131

Anova(model_WkSi.d, type = c("III"))        #    Depth p = 0.001118 --> Significant!
                                            # Land_Use p = 0.007141 --> Significant!

fn_statTest(model_WkSi.d,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,           # Equal Variance: Equal variance!
            saveTest = TRUE)                #      Normality: As normally distributed according to Shapiro-Wilks, but looks to be a bimodal distribution 

p_pit_WkSi_002 = qp_depth(data_p, log10(data_p$Oxa_Si), Lim = c(2,5)) +
  qp_lmANOVA(model_WkSi.d)$lines +
  labs(
    subtitle = qp_nPits,
    x = TeX("$\\bf{Log_{10}\\,Si\\,Concentration}\\,\\,log_{10}(mg\\,kg^{-1})$"),
    caption = qp_lmANOVA(model_WkSi.d)$caption
  )

p_pit_WkSi_002
fn_quickSave(p_pit_WkSi_002)

################################################################################
#
# Amorphous Silicon (ASi)
#
################################################################################

# Land Use (Spatial Variation)
p_top_ASi_001 = qp_landUse(data_t, data_t$Si_kin) +
  fn_statCompare(c(6000,7000,6000)) +
  labs(
    subtitle = qp_nTopSoil,
    y = qp_SiCon
  )

p_top_ASi_001
fn_quickSave(p_top_ASi_001)

# Depth (Effect of Depth)

# Depth + Land Use Model
model_ASi.d <- lm(Si_kin ~ Depth_Avg+Land_Use.f, data = data_p)

summary(model_ASi.d)                       # Regression is statistically significant --> p = 0.02545
                                           # R^2 = 0.3654 --> Not great
                                           # Adj_R^2 = 0.2702

Anova(model_ASi.d, type = c("III"))        #    Depth p = 0.03565 --> Significant!
                                           # Land_Use p = 0.06132 --> Possibly Significant!

fn_statTest(model_ASi.d,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values
            saveTest = TRUE)               #      Normality: Not normally distributed

p_pit_ASi_002 = qp_depth(data_p, data_p$Si_kin) +
  qp_lmANOVA(model_ASi.d)$lines +
  labs(
    subtitle = qp_nPits,
    x = qp_SiCon,
    caption = qp_lmANOVA(model_ASi.d)$caption
  )

p_pit_ASi_002
fn_quickSave(p_pit_ASi_002)

################################################################################
#
# Ca & Si
#
################################################################################

# Plant Available Calcium & Silicon
tempLab_x = TeX("$\\bf{Plant\\,Available\\,Ca}\\,(mg\\,kg^{-1})$")

# Calcium and Dissolved Silicon
model_CaDSi = lm(Si_CaCl2 ~ pan_Ca, data = data_c)

fn_statTest(model_CaDSi,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Normally distributed, but slightly skewed left

summary(model_CaDSi)                       # Regression is not statistically significant --> p = 0.5332
                                           # R^2 = 0.00978 --> Awful
                                           # Adj_R^2 = -0.1498

# Calcium and Adsorbed Silicon
model_CaAdSi = lm(Si_Acetic ~ pan_Ca, data = data_c)

fn_statTest(model_CaAdSi,                  #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Not normally distributed, looks bimodal

summary(model_CaAdSi)                      # Regression is not statistically significant --> p = 0.3844
                                           # R^2 = 0.01897 --> Awful
                                           # Adj_R^2 = -0.005559

# Calcium and Weakly Crystalline Silicon
model_CaWkSi = lm(Oxa_Si ~ pan_Ca, data = data_c)

fn_statTest(model_CaWkSi,                  #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Not normally distributed

summary(model_CaWkSi)                      # Regression is not statistically significant --> p = 0.5147
                                           # R^2 = 0.01069 --> Awful
                                           # Adj_R^2 = -0.01404

# Calcium and Amorphous Silicon
model_CaASi = lm(Si_kin ~ pan_Ca, data = data_c)

fn_statTest(model_CaASi,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values
            saveTest = TRUE)               #      Normality: Normally distributed

summary(model_CaASi)                       # Regression is not statistically significant --> p = 0.02171
                                           # R^2 = 0.1248 --> Awful
                                           # Adj_R^2 = 0.103



p_comp_CaSi_001 = qp_Ratio(data_c, data_c$pan_Ca, data_c$Si_CaCl2, yLim = c(0,30,5)) + labs(y = TeX("$\\bf{Dissolved\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_CaDSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_CaDSi, plotOrientation = "x")$lines
p_comp_CaSi_001
p_comp_CaSi_002 = qp_Ratio(data_c, data_c$pan_Ca, data_c$Si_Acetic, yLim = c(0,250,50)) + labs(y = TeX("$\\bf{Adsorbed\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_CaAdSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_CaAdSi, plotOrientation = "x")$lines
p_comp_CaSi_002
p_comp_CaSi_003 = qp_Ratio(data_c, data_c$pan_Ca, data_c$Oxa_Si, yLim = c(0,15000,2500)) + labs(y = TeX("$\\bf{Weakly Crystaline\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_CaWkSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_CaWkSi, plotOrientation = "x")$lines
p_comp_CaSi_003
p_comp_CaSi_004 = qp_Ratio(data_c, data_c$pan_Ca, data_c$Si_kin, yLim = c(0,10000,2500)) + labs(y = TeX("$\\bf{Amorphous\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_CaASi, plotOrientation = "x")$caption) + qp_lmANOVA(model_CaASi, plotOrientation = "x")$lines
p_comp_CaSi_004

p_comp_CaSi_005 = (p_comp_CaSi_001 + p_comp_CaSi_002)/(p_comp_CaSi_003 + p_comp_CaSi_004)/(guide_area()) + plot_layout(guides = "collect", heights = c(4,4,1)) & theme(legend.position = 'bottom', legend.direction = "horizontal") & guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) & plot_annotation(tag_levels = "A")
p_comp_CaSi_005

fn_quickSave(p_comp_CaSi_005, saveHeight = 16)

rm(tempLab_x)


################################################################################


# Total Calcium & Silicon
tempLab_x = TeX("$\\bf{Microwave\\,Digestible\\,Ca}\\,(mg\\,kg^{-1})$")

# Calcium and Dissolved Silicon
model_Total_CaDSi = lm(Si_CaCl2 ~ total_Ca, data = data_p)

fn_statTest(model_Total_CaDSi,             #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Normally distributed

summary(model_Total_CaDSi)                 # Regression is not statistically significant --> p = 0.1265
                                           # R^2 = 0.1029 --> Awful
                                           # Adj_R^2 = -0.06208

# Calcium and Adsorbed Silicon
model_Total_CaAdSi = lm(Si_Acetic ~ total_Ca, data = data_p)

fn_statTest(model_Total_CaAdSi,                  #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Not normally distributed, looks bimodal

summary(model_Total_CaAdSi)                      # Regression is not statistically significant --> p = 0.3844
                                           # R^2 = 0.01897 --> Awful
                                           # Adj_R^2 = -0.005559

# Calcium and Weakly Crystalline Silicon
model_Total_CaWkSi = lm(Oxa_Si ~ total_Ca, data = data_p)

fn_statTest(model_Total_CaWkSi,                  #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values?
            saveTest = TRUE)               #      Normality: Not normally distributed

summary(model_Total_CaWkSi)                      # Regression is not statistically significant --> p = 0.5147
                                           # R^2 = 0.01069 --> Awful
                                           # Adj_R^2 = -0.01404

# Calcium and Amorphous Silicon
model_Total_CaASi = lm(Si_kin ~ total_Ca, data = data_p)

fn_statTest(model_Total_CaASi,                   #      Linearity: No curvilinear relationship
            testTheme = qp_theme,          # Equal Variance: Equal variance across predicted values
            saveTest = TRUE)               #      Normality: Normally distributed

summary(model_Total_CaASi)                       # Regression is not statistically significant --> p = 0.02171
                                           # R^2 = 0.1248 --> Awful
                                           # Adj_R^2 = 0.103



p_comp_CaSi_006 = qp_Ratio(data_p, data_p$total_Ca, data_p$Si_CaCl2, yLim = c(0,30,5), xLim = c(0,6000,1000)) + labs(y = TeX("$\\bf{Dissolved\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_Total_CaDSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_Total_CaDSi, plotOrientation = "x")$lines
p_comp_CaSi_006
p_comp_CaSi_007 = qp_Ratio(data_p, data_p$total_Ca, data_p$Si_Acetic, yLim = c(0,250,50), xLim = c(0,6000,1000)) + labs(y = TeX("$\\bf{Adsorbed\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_Total_CaAdSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_Total_CaAdSi, plotOrientation = "x")$lines
p_comp_CaSi_007
p_comp_CaSi_008 = qp_Ratio(data_p, data_p$total_Ca, data_p$Oxa_Si, yLim = c(0,15000,2500), xLim = c(0,6000,1000)) + labs(y = TeX("$\\bf{Weakly Crystaline\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_Total_CaWkSi, plotOrientation = "x")$caption) + qp_lmANOVA(model_Total_CaWkSi, plotOrientation = "x")$lines
p_comp_CaSi_008
p_comp_CaSi_009 = qp_Ratio(data_p, data_p$total_Ca, data_p$Si_kin, yLim = c(0,10000,2500), xLim = c(0,6000,1000)) + labs(y = TeX("$\\bf{Amorphous\\,Si}\\,(mg\\,kg^{-1})$"), x = tempLab_x, caption = qp_lmANOVA(model_Total_CaASi, plotOrientation = "x")$caption) + qp_lmANOVA(model_Total_CaASi, plotOrientation = "x")$lines
p_comp_CaSi_009

p_comp_CaSi_010 = (p_comp_CaSi_006 + p_comp_CaSi_007)/(p_comp_CaSi_008 + p_comp_CaSi_009)/(guide_area()) + plot_layout(guides = "collect", heights = c(4,4,1)) & theme(legend.position = 'bottom', legend.direction = "horizontal") & guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) & plot_annotation(tag_levels = "A", theme = qp_theme)
p_comp_CaSi_010

fn_quickSave(p_comp_CaSi_010, saveHeight = 16)

rm(tempLab_x)