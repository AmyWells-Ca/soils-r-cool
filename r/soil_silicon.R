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
data$Transect.f <- as.factor(data$Transect)
data$Type.f <- as.factor(data$Type)
data$Land_Use.f <- as.factor(data$Land_Use)

data_depth <- filter(data, Type == "Pit")
data_landUse <- filter(data, Depth_Top == 0)

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

p1 <- ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Si_CaCl2, fill = Land_Use.f), color = "black") +
  scale_fill_manual(
    name = "Land Use",
    labels = c("Cutblock", "Forest Garden", "Periphery Forest"),
    values = c("darkorange2","darkgreen","azure4")
  ) +
  guides(fill = "none") +
  fn_yLim(0,25) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{CaCl_{2}\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

p2 <- ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Si_Acetic, fill = Land_Use.f), color = "black") +
  scale_fill_manual(
    name = "Land Use",
    labels = c("Cutblock", "Forest Garden", "Periphery Forest"),
    values = c("darkorange2","darkgreen","azure4")
  ) +
  guides(fill = "none") +
  fn_yLim(0,250) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Acetic\\Acid\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

p3 <- ggplot(data = data) +
  geom_boxplot(mapping=aes(x=Land_Use.f, y=Oxa_Si, fill = Land_Use.f), color = "black") +
  scale_fill_manual(
    name = "Land Use",
    labels = c("Cutblock", "Forest Garden", "Periphery Forest"),
    values = c("darkorange2","darkgreen","azure4")
  ) +
  guides(fill = "none") +
  fn_yLim(0,15000) +
  labs(
    x = TeX("$\\bf{Land\\,Use}"),
    y = TeX("$\\bf{Oxalate\\,Extractable\\,Si}\\,(mg\\,kg^{-1})")
  ) + theme_presentation

p4 <- (p1 + p2 + p3)

print(p4)

ggsave (
  filename = "figure_n_DescribedSi.png",
  plot = p4,
  device = png(),
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
