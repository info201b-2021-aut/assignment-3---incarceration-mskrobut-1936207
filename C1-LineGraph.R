library(tidyverse)
library(ggplot2)
library(hrbrthemes)

## CREATING SMALLER CSV W ONLY THE APPLICAPABLE DATA
incarceration_trends <- read_csv("incarceration_trends.csv")

King_df <- select(incarceration_trends, state, county_name, year, total_pop_15to64, black_pop_15to64, total_jail_pop, black_jail_pop, aapi_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  filter(state == "WA", county_name == "King County", year == 2018 |year == 2016 | year == 2014 | year == 2012 | year == 2010 | year == 2008 | year == 2006 | year == 2004 | year == 2002 | year == 2000) %>%
  mutate(PercentofTotalPop_Black = (black_pop_15to64 / total_pop_15to64) * 100) %>%
  mutate(PercentofJailPop_Black = (black_jail_pop / total_jail_pop) * 100) %>%
  mutate(PercentofTotalPopinJail = (total_jail_pop / total_pop_15to64) * 100)

write.csv(King_df,"King_df.csv", row.names = FALSE)

King_df <- read_csv("King_df.csv")


### CHART 1 TREND OVER TIME What % of total Incarceration do Black People make up in King County from 2000 - 2018


Chart1PercentJailPopBlack <- subset(King_df, select = c("year", "PercentofJailPop_Black"))

ggplot(Chart1PercentJailPopBlack, aes(x=year, y=PercentofJailPop_Black)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9, linetype=2) +
  theme_ipsum() +
  labs(title = "Black People's Percent of Total Incarceration \nin King County from 2000 to 2018") + ylab("Black Peoples % Total Incarceration") + xlab("Years")
theme_bw(base_size = 200)