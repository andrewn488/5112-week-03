# Practice with Week 3 Videos
# load libraries and datasets to use
# "gg" = "Grammar of Graphics"
library(tidyverse)
library(gridExtra)
library(GGally)
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

# --------------------------------------
# EDA Process
library(tidyverse)
library(haven)
nfhs <- read_dta("02_Raw_data/IAHR52FL.dta")
View(nfhs)

# tidy data from week 02 exercise: 
question_2 <- nfhs %>% 
  select(1:47, "hv270")
View(question_2)
# setup education:
question_3 <- nfhs %>% 
  select("hhid", starts_with("hvidx"), contains("hv108"))
# setup female: 
question_4 <- nfhs %>% 
  select("hhid", starts_with(c("ha0_", "ha1_", "ha2_", "ha3_", "ha4_", "ha5_", "ha6_")))
View(question_4)

# setup male: 
question_5 <- select(nfhs, hhid,
                     num_range("hb0_", 0:20, width = 2),
                     num_range("hb1_", 0:20, width = 2),
                     num_range("hb2_", 0:20, width = 2),
                     num_range("hb3_", 0:20, width = 2),
                     num_range("hb4_", 0:20, width = 2),
                     num_range("hb5_", 0:20, width = 2),
                     num_range("hb6_", 0:20, width = 2)
)
View(question_5)

# edu tidy:
edu_tidy <- question_3 %>% 
  gather(variable_name, var_value, -hhid) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  select(-number) %>%               # de-select 'number' column
  filter(!is.na(hvidx)) %>%         # filter out na's
  rename(roster_id = hvidx, edu_yrs = hv108)
View(edu_tidy)

# female tidy: 
female_tidy <- question_4 %>% 
  gather(variable_name, var_value, -hhid) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  select(-number, -ha0, -ha4, -ha5, -ha6) %>%      # de-select unused variables
  rename(female_age = ha1, weight = ha2, height = ha3) %>%     # rename columns to something meaningful
  subset(!is.na(female_age)) %>%    # filter out na's
  mutate(female = TRUE)
View(female_tidy)

# male tidy:
male_tidy <- question_5 %>% 
  gather(variable_name, var_value, 2:127) %>% 
  separate(variable_name, c("var", "number"), sep = "_") %>% 
  spread(key = var, value = var_value) %>% 
  select(-number, -hb0, -hb4, -hb5, -hb6) %>%    # de-select unused variables
  rename(male_age = hb1, weight = hb2, height = hb3) %>%      # rename columns to something meaningful
  subset(!is.na(male_age)) %>%      # filter out na's
  mutate(female = FALSE) 
View(male_tidy)

# bind male and female
male_and_female <- bind_rows(female_tidy, male_tidy)

# join
week02_tidy <- left_join(male_and_female, question_2, by = "hhid")
View(week02_tidy)
summary(week02_tidy)

#----------------------------------------
# EDA Process
week02_tidy <- week02_tidy %>% 
  mutate(
    female_weight = case_when(
      female_weight <= 9000 ~ female_weight / 10, 
      female_weight > 9000 ~ NA_real_
    ),
    male_weight = case_when(
      male_weight <= 9000 ~ male_weight / 10, 
      male_weight > 9000 ~ NA_real_
    ),
    female_height = case_when(
      female_height <= 9000 ~ female_height / 10,
      female_height > 9000 ~ NA_real_
    ),
    male_height = case_when(
      male_height <= 9000 ~ male_height / 10,
      male_height > 9000 ~ NA_real_
    )
  )
summary(week02_tidy)

# Univariate non-graphical 
# categorical data - tabulation
week02_tidy %>% 
  rename(type_place = hv025) %>% 
  group_by(type_place, female) %>%            # organize data by female
  summarise(count = n(),
            percent = (sum(count) / nrow(week02_tidy)) * 100, 
            mean_age = mean(female_age, na.rm = TRUE),
            mean_female_height = mean(female_height, na.rm = TRUE),
            mean_female_weight = mean(female_weight, na.rm = TRUE)
            )

# univariate graphing
grid.arrange(
  ggplot(week02_tidy, aes(x = 1, y = female_age)) +  
  geom_boxplot(),
  ggplot(week02_tidy, aes(x = 2, y = edu_yrs)) + 
  geom_boxplot(),
  ncol = 2
)

# multivariate EDA
cor(week02_tidy$female_weight, week02_tidy$female_height, use = "complete.obs")
ggpairs(week02_tidy, columns = c("female_age", "female_weight", "female_height", "edu_yrs"))


#-------------------------------------------
# stats review - Central Limit Theorem


