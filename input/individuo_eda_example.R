library(tidyverse)

source("./input/load_souce_data.R")



famconf2020 %>% filter(fid20 == 100051) %>% select(contains('fid'), co_a20_p)

# Un pid a boleo:  100051501

# Para saber el hukou que tenía esta persona con 12 años:
adult2020 %>% filter(pid == 100051501) %>% select(pid, qa603)

# Para conocer su residencia y hukou presentes
famconf2020 %>%
  filter(pid == 100051501) %>%
  select(pid, hukou_a20_p, fid_urban_20)