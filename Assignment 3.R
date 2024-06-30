# getwd()

if (!require(dplyr)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(dplyr)) install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1
data <- read_csv('StormEvents_details-ftp_v1.0_d1984_c20220425.csv')

# head(data,7)

# 2
data <- data[c("BEGIN_YEARMONTH","EPISODE_ID","STATE","STATE_FIPS","CZ_NAME","CZ_TYPE","CZ_FIPS","EVENT_TYPE")] %>%
  #3
  arrange(STATE) %>%
  
  #4
  mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME)) %>%

  #5
  filter(CZ_TYPE=="C") %>%
  select(-c("CZ_TYPE")) %>%
  
  #6
  mutate(STATE_FIPS = str_pad(data$STATE_FIPS, width = 3, side = "left", pad = "0"), CZ_FIPS = str_pad(data$CZ_FIPS, width = 3, side = "left", pad = "0")) %>%
  unite("FIPS", c("STATE_FIPS", "CZ_FIPS")) %>%
  
  #7
  rename_all(tolower)

#8
us_states <- data.frame(state = state.name, region = state.region, area = state.area)

#9
count <- data.frame(table(data$state)) %>% rename(c("state"="Var1"))
merged <- merge(count,us_states,"state")

#10
plot <- ggplot(merged,
               aes(x = area, y = Freq)) +
        geom_point(aes(color= region)) +
        labs(x = "Land area (sq mi)",
             y = "# of storm events in 1984") +
        ggtitle("Storms events in 1984") +
        scale_x_continuous(name = "",labels = scales::comma)
plot