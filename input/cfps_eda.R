library(tidyverse)
library(e1071)

source("./input/clean_datasets.R")



famecon_family_df %>% 
  ggplot(aes(x = fincome1)) +
  geom_histogram() +
  facet_wrap(~provcd)