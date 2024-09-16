require(dplyr)
library(readr)
library(tidyr)
library(stringr)


df <- read_csv("rvu_data.csv")%>% 
  mutate(min_minutes = as.numeric(str_extract(description, '[0-9].'))) %>% 
  mutate(min_minutes = ifelse(grepl('hour', description), 60, min_minutes)) %>% 
  mutate(type = case_when(
    grepl('new', description, ignore.case=T) ~ "new",
    grepl('established', description, ignore.case=T) ~ "fu",
    grepl('consult', description, ignore.case=T) ~ "cs",
    grepl('prolonged', description, ignore.case=T) ~ "extra",
  ))
df

find_base <- function(visit_type, minutes){
  df <- df %>% filter(type == visit_type)
  extra_minutes <- minutes - max(df$min_minutes)
  n_prolonged <- extra_minutes %/% 15
  print(sprintf('Extra: %s', extra_minutes))
  print(sprintf('Number of prolonged : %s', n_prolonged))
}

find_base('new', 100)