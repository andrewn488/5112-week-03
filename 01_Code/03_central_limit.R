# Central limit theorem

library(tidyverse)
library(here)

base <- read_rds(here("02_Raw_data/educ.rds"))

View(base)
# Years of education is distinctly NOT normally distributed
ggplot(base, aes(x = educ)) +
  geom_histogram(binwidth = 1)

# could also just have used summary
base %>% 
  summarise(avg_edu = mean(educ, na.rm = TRUE))


# Central Limit Theorem in action
# You should try to change some of the values and compare

# keep this one the same, so you can see what happens when you change sample
# size and number of replications
set.seed(2) 

sample_size <- 5

get_mean <- function(df, sample_size) {
  df <- sample_n(df, sample_size) %>% 
    summarise(avg_educ = mean(educ, na.rm = TRUE))
  df$avg_educ
}

sample_of_avg <- as.tibble(replicate(1000, get_mean(base, sample_size)))

ggplot(sample_of_avg, aes(x = value)) +
  geom_histogram()

mean(sample_of_avg$value)

