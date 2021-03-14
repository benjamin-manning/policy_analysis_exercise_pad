library(tidyverse)
library(dplyr)
library(stringr)
library(svMisc)
library(ggplot2)
library(gtable)
library("ggpubr")
library(data.table)
library(lfe)
library(haven)


per_call <- read_csv('~/Desktop/PAE/github/data/per_call_df.csv')
per_call_saver <- per_call

per_call_time_sort <- per_call %>% 
  arrange(last_year)


only_last_calls <- per_call_time_sort[!duplicated(per_call_time_sort$callerId), ]


drop_last_calls <- per_call_time_sort[duplicated(per_call_time_sort$callerId), ]
class(drop_last_calls$callerId)

mean_depth <- drop_last_calls %>%
  group_by(callerId) %>%
  summarise(mean_depth = mean(n.x))

mean_call_time <- drop_last_calls  %>%
  group_by(callerId) %>%
  summarise(mean_call = mean(call_time))

combined_means <- left_join(mean_call_time, mean_depth,by="callerId")

last_calls_only <- only_last_calls %>% 
  select(callerId, content)

prediction_df <- left_join(combined_means , last_calls_only, by="callerId")
write.csv(prediction_df, '~/Desktop/PAE/github/data/time_fix.csv', row.names = FALSE)

