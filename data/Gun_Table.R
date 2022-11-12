library("dplyr")

#Data wrangling 

# gun_violence <- read.csv("/Users/yenmy/Documents/info201/project/project-itsyenmyvo22/data/gun-violence-data_01-2013_03-2018.csv", stringsAsFactors = FALSE)
# View(gun_violence)

firearm_provision <- read.csv("/Users/yenmy/Documents/info201/project/project-itsyenmyvo22/data/Firearm-provisions.csv", stringsAsFactors = FALSE)
View(firearm_provision)

sociological_metrics <- read.csv("/Users/yenmy/Documents/info201/project/project-itsyenmyvo22/data/Sociological-Metrics.csv", stringsAsFactors = FALSE)
View(sociological_metrics)

summary_gun_violence <- gun_violence %>%
  select(state, date, n_killed, n_injured)
View(summary_gun_violence)

summary_firearm_provision <- firearm_provision %>%
  filter(year == 2017) %>%
  select(state, age18longgunsale, age21longgunsale)
View(summary_firearm_provision)

summary_sociological_metrics <- sociological_metrics %>%
  select(State, Percent.Above.Poverty.Rate)
View(summary_sociological_metrics)

names(summary_sociological_metrics)[names(summary_sociological_metrics) == "State"] <- "state"
View(summary_sociological_metrics)

all_sum_df <- left_join(summary_gun_violence, summary_firearm_provision, by = "state")
View(all_sum_df)

all_sum_df <- left_join(all_sum_df, summary_sociological_metrics, by = "state")
View(all_sum_df)

#The final data frame with everything we should be using is `all_sum_df`

write.csv(all_sum_df,"/Users/yenmy/Documents/info201/project/project-itsyenmyvo22/data/all_sum_df.csv", row.names = FALSE)

