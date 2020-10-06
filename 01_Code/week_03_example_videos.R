# Practice with Week 3 Videos
# load libraries and datasets to use
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
