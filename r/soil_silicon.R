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

area = c(12, 14, 19)

mean(reference$Si_CaCl2)

ggplot(data = data, aes(y = Si_CaCl2, x = Land_Use.f)) +
  geom_boxplot(notch = FALSE) + 
  theme_paper
  fn_compare(reference$Si_CaCl2, compareMode = 1)
  
ggplot(data = data, aes(y = Land_Use.f, x=Si_CaCl2)) +
  geom_boxplot(notch = FALSE) +
  theme_paper
  fn_compare(reference$Si_CaCl2, compareMode = 0, compareAxis = "x")

