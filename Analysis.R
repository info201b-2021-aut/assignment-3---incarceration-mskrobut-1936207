incarceration_trends <- read_csv("Desktop/assignment-3---incarceration-mskrobut-1936207/incarceration_trends.csv")

library(tidyverse)
incarcerationtrends_df <- read.csv("Desktop/assignment-3---incarceration-mskrobut-1936207/incarceration_trends.csv")

# SUMMARY INFO
SummaryInfo <- select(incarceration_trends, year, state, county_name, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  filter(year == 2018, state == "WA")

CountyHighestJailPop <- filter(SummaryInfo, year == 2018, total_jail_pop == max(total_jail_pop))
print(CountyHighestJailPop)
# Answer: King County with 3060

CountyLowestJailPop <- filter(SummaryInfo, year == 2018, total_jail_pop == min(total_jail_pop))
print(CountyLowestJailPop)
# Answer: Garfield and Columbia with 7

CountyHighestBlackPop2018 <- filter(SummaryInfo, year == 2018, black_jail_pop == max(black_jail_pop))
print(CountyHighestBlackPop2018)
# Answer: King County 3060.00

CountyLowestBlackPop2018 <- filter(SummaryInfo, year == 2018, black_jail_pop == min(black_jail_pop))
print(CountyLowestBlackPop2018)
# Answer: Adams, Garfeild, Pend Oreille, Wahkiakum all had 0

# RaceMostIncarKing2018 <- filter(SummaryInfo, year == 2018, county_name == "King County", county_name == max(county_name))
# Answer: White People with 1565.32

# RaceMostIncarKing1991 <- filter(KingTotalStats, year == 1991, county_name == "King County", county_name == max(county_name))
# Answer: White people with 1137.79


# SMALLER CSV FILE FOR WHAT I AM ANALYZING IN FRIST 2 CHARTS
King_df <- select(incarceration_trends, state, county_name, year, total_pop_15to64, black_pop_15to64, total_jail_pop, black_jail_pop, aapi_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  filter(state == "WA", county_name == "King County", year == 2018 |year == 2016 | year == 2014 | year == 2012 | year == 2010 | year == 2008 | year == 2006 | year == 2004 | year == 2002 | year == 2000) %>%
  mutate(PercentofTotalPop_Black = (black_pop_15to64 / total_pop_15to64) * 100) %>% 
  mutate(PercentofJailPop_Black = (black_jail_pop / total_jail_pop) * 100) %>%
  mutate(PercentofTotalPopinJail = (total_jail_pop / total_pop_15to64) * 100)

write.csv(King_df,"Desktop/assignment-3---incarceration-mskrobut-1936207/King_df.csv", row.names = FALSE)     



# CHART 1 TREND OVER TIME What % of total Incarceration do Black People make up in King County from 2000 - 2018
King_df <- read_csv("Desktop/assignment-3---incarceration-mskrobut-1936207/King_df.csv")
Chart1PercentJailPopBlack <- subset(King_df, select = c("year", "PercentofJailPop_Black"))

library(ggplot2)
library(hrbrthemes)

ggplot(Chart1PercentJailPopBlack, aes(x=year, y=PercentofJailPop_Black)) +
  geom_line( color="#69b3a2", size=1, alpha=0.9, linetype=2) +
  theme_ipsum() +
  labs(title = "Black People's Percent of Total Incarceration \nin King County from 2000 to 2018") + ylab("Black Peoples % Total Incarceration") + xlab("Years")
  theme_bw(base_size = 200)



# CHART 2 COMPARISON RATIO percentage of population that is Black vs the percentage of pop in jail that is black in King County
King_df <- read_csv("Desktop/assignment-3---incarceration-mskrobut-1936207/King_df.csv")
ComparScat <- subset(King_df, select = c("year", "PercentofTotalPop_Black", "PercentofJailPop_Black")) %>%
  filter(year == 2018 | year == 2016 | year == 2014 | year == 2012 | year == 2010 | year == 2008 | year == 2006 | year == 2004 | year == 2002 | year == 2000)

library(ggplot2)
library(hrbrthemes)

ggplot(ComparScat, aes(x=PercentofJailPop_Black, y=PercentofTotalPop_Black)) + 
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6, alpha=0.6) +
  theme_ipsum() +
  labs(title = "The Effect of King County's rising population of \nBlack People on the amount of incarcerated \nBlack People in King County") + ylab("Percent of Total Black Population in Jail") + xlab("Percent of Total Black Population in King County")
  theme_bw(base_size = 200)
  

# CHART 3 MAP WA counties 2018 % total black people incarcerated from total jail population
  Map_df <- subset(incarceration_trends, select = c("state", "county_name", "year", "total_jail_pop", "black_jail_pop")) %>%
    filter(state=="WA", year == 2018) %>%
    mutate(PercentofJailPop_Black = (black_jail_pop / total_jail_pop) * 100)
  
  write.csv(Map_df,"Desktop/assignment-3---incarceration-mskrobut-1936207/Map_df.csv", row.names = FALSE)            
  Map_df <- read_csv("Desktop/assignment-3---incarceration-mskrobut-1936207/Map_df.csv")
  
  map_data <- subset(Map_df, select = c("county_name", "PercentofJailPop_Black"))
  rename(map_data) -> CountyWAtable
  
  