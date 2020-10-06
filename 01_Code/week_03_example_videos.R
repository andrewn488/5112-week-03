# Practice with Week 3 Videos
# load libraries and datasets to use
# "gg" = "Grammar of Graphics"
library(tidyverse)
data(mpg)
View(mpg)

# what's the relationship between Engine Size ('displ') and mpg ('hwy')?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# this yields a 'negative' relationship: bigger engine = less mpg

# practice creating a ggplot with different mappings
# color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# practice with Set vs. Mapping
# Set goes OUTSIDE the aesthetic when dealing with variables
# Set goes INSIDE the aesthetic to make a specific group, called what you call it
# INSIDE = mapping to something
# OUTSIDE = setting characteristics for the entire layer
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# histogram
ggplot(data = mpg) + 
  geom_histogram(mapping = aes(x = hwy))

# often not where we want to be. binwidth is VERY important for histogram
# R chooses binwidth = 30 by default
ggplot(data = mpg) + 
  geom_histogram(mapping = aes(x = hwy, binwidth = 2))

# color vs. fill
ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill = class))
# density
ggplot(data = mpg) + 
  geom_density(mapping = aes(x = hwy))

# add color to density
ggplot(data = mpg) + 
  geom_density(mapping = aes(x = hwy, color = class))
# add group to density
ggplot(data = mpg) + 
  geom_density(mapping = aes(x = hwy, group = class))

# --------------------------------------
# Global vs. Local Variables
# Local characteristics are added per layer
# layers of geoms - point and smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# add characteristics per layer - color to point: 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth()

# add characteristics per layer - filter to smooth: 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth(data = filter(mpg, drv == "f"))

# grammar of graphics



