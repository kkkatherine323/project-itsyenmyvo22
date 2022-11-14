# 1) number gun deaths where sales 18+
# 2) number gun deaths where sales 21+
# 3) state with highest # of gun deaths
# 4) state with highest rate of poverty
# 5) highest # of gun deaths in one day

library("dplyr")
library("tidyr")

data_summary <- read.csv("https://raw.githubusercontent.com/kkkatherine323/project-itsyenmyvo22/main/data/all_sum_df.csv")

summary_info <- list()
summary_info$deaths_sales_18 <- sum(data_summary$age18longgunsale, na.rm = T)
summary_info$deaths_sales_18 <- sum(data_summary$age18longgunsale, na.rm = T)
summary_info$max_deaths_state <- data_summary %>%
  filter(n_killed == max(n_killed, na.rm = T)) %>%
  select(state)
summary_info$highest_rate_state <- data_summary %>%
  filter(Percent.Above.Poverty.Rate == max(Percent.Above.Poverty.Rate, na.rm = T)) %>%
  select(state) %>%
  distinct(state, .keep_all = TRUE)
summary_info$highest_deaths_one_day <- data_summary %>%
  filter(n_killed == max(n_killed, na.rm = T)) %>%
  select(n_killed)
