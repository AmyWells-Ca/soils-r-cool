source("./r/functions.R")

# Randomly generated data from Excel
df <- data.frame(
  LandUse = c("Cutblock", "Periphery", "Forest Garden", "Cutblock", "Periphery", "Forest Garden", "Cutblock", "Periphery", "Forest Garden"),
  pH_H2O = c(5.8,5.5,5.6,5.1,6,3.8,3.7,5.6,3.8),
  pH_CaCl2 = c(3.5,3.1,2.6,4,4.5,4,4,3.6,3.4),
  SOM = c(1.92,2.21,3.22,4.89,5.76,1.56,5.25,2.84,8.06)
)

# Converts the "class" variable into a factor for statistical analysis
df$Land_Use.f <- as.factor(df$LandUse)
# Sets the order of the factor
df$Land_Use.f <- factor(df$Land_Use.f, levels = c("Cutblock", "Periphery", "Forest Garden"))

p_b1 <- ggplot(data = df, mapping = aes(x = pH_H2O, y = pH_CaCl2, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(colour = "black", size = 3) +
  labs(
    x = "pH in H2O",
    y = "pH in CaCl2"
  )

p_b1

# Example Save for Plot that is 6.5" wide and 3.5" tall (For a Paper)
ggsave (
  filename = "p_b1.png",
  plot = p_b1,
  path = "./output",
  scale = 1,
  width = 6.5,
  height = 3.5,
  units = c ("in"),
  dpi = 300
)

p_b2 <- ggplot(data = df, mapping = aes(x = Land_Use.f, y = SOM, fill = Land_Use.f)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  labs(
    x = "Land Use",
    y = "Percent SOM"
  )

p_b2

# Example Save for Plot that is 23.88 cm wide and 12.80 cm tall (For a PowerPoint)
ggsave (
  filename = "p_b2.png",
  plot = p_b2,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)

# Now With Tweaks

p_a1 <- ggplot(data = df, mapping = aes(x = pH_H2O, y = pH_CaCl2, fill = Land_Use.f, shape = Land_Use.f)) +
  geom_point(colour = "black", size = 3) +
  fn_fillScale() +
  fn_shapeScale() +
  fn_xLim(3.5, 6.5) +   # Set the bounds of the X Axis!
  fn_yLim(2.5, 5) +     # Set the bounds of the Y Axis! 
  labs(
    x = "pH in H2O",
    y = "pH in CaCl2"
  ) + theme_paper # Sets the plot theme with the paper style!

p_a1

# Example Save for Plot that is 6.5" wide and 3.5" tall (For a Paper)
ggsave (
  filename = "p_a1.png",
  plot = p_a1,
  path = "./output",
  scale = 1,
  width = 6.5,
  height = 3.5,
  units = c ("in"),
  dpi = 300
)

p_a2 <- ggplot(data = df, mapping = aes(x = Land_Use.f, y = SOM, fill = Land_Use.f)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, colour = "black") +
  fn_fillScale() +
  fn_yLim(0,10) +      # Set the bounds of the Y Axis! We don't have to worry about X-Axis bounds as it is a boxplot!
  labs(
    x = "Land Use",
    y = "Percent SOM"
  ) + 
  theme_presentation # Sets the plot theme with the presentation style!

p_a2

# Example Save for Plot that is 23.88 cm wide and 12.80 cm tall (For a PowerPoint)
ggsave (
  filename = "p_a2.png",
  plot = p_a2,
  path = "./output",
  scale = 1,
  width = 23.88,
  height = 12.80,
  units = c ("cm"),
  dpi = 300
)
