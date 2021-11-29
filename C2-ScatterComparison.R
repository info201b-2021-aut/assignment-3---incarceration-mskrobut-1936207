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


### CHART 2 COMPARISON RATIO percentage of population that is Black vs the percentage of pop in jail that is black in King County
King_df <- read_csv("King_df.csv")
ComparScat <- subset(King_df, select = c("year", "PercentofTotalPop_Black", "PercentofJailPop_Black")) %>%
  filter(year == 2018 | year == 2016 | year == 2014 | year == 2012 | year == 2010 | year == 2008 | year == 2006 | year == 2004 | year == 2002 | year == 2000)

ggplot(ComparScat, aes(x=PercentofJailPop_Black, y=PercentofTotalPop_Black)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6, alpha=0.6) +
  theme_ipsum() +
  labs(title = "The Effect of King County's rising population of \nBlack People on the amount of incarcerated \nBlack People in King County") + ylab("Percent of Total Black Population in Jail") + xlab("Percent of Total Black Population in King County")
theme_bw(base_size = 200)
