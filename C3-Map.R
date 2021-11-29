### CHART 3 MAP OF ALL COUNTIES IN US % OF BLACK PEOPLE OF TOTAL INCARCERATED POP IN 2018
library(mapproj)
library(ggplot2)
library(dplyr)
require(maps)

statenameabr <- read_csv("statenameabr.csv")
ALLcounties2018 <- read_csv("ALLcounties2018.csv")

map_shape <- map_data("county")

map_shapewabr <- merge(x=map_shape,y=statenameabr,by="region",all.x=TRUE)

dataALLcounty <- rename(ALLcounties2018, abr = state)

mapdataUSstate <- merge(x=dataALLcounty,y=map_shapewabr,by="abr",all.x=TRUE)

dataforcornermap <- subset(mapdataUSstate, select = c("abr", "PercentofJailPop_Black", "long", "lat", "group")) %>%
  filter (abr == "WA" | abr == "OR" | abr == "CA" | abr == "AL" | abr == "GA" | abr == "FL")

map_theme <- theme_bw() +
  theme(
    plot.background = element_rect(),
  )

UScornermap <- ggplot(dataforcornermap) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = PercentofJailPop_Black),
    color = "black",
    size = .1
  ) +
  coord_map() +
  scale_fill_gradient2(
    low = "yellow",
    mid = "orange",
    high = "red",
    midpoint = 50,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",
    breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels=c("Minimum", 10, 20, 30, 40, 50, 60, 70, 80, 90, "Maximum"),
    limits=c(0, 100)
  ) +
  labs(
    fill = "% of Incarcerated Population that is Black",
    title = "Percentage of Black People of the total Incarcerated Population in 2018",
  ) +
  map_theme

print(UScornermap)

