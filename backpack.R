# we try again -----
# Assignment 3
# Load the data into a variable `incarceration`
library("stringr")
urlfile = ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incar <- read.csv(url(urlfile))

library("ggplot2")
library("dplyr")

# Introduction + Summary -------------------------------------------------------

# In 2018, which state has had the highest total jail population in the
# dataset, and what is that number? Store the state and population number in
# the variable `jail_highest_state`

jail_highest_state <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(state, total_jail_pop)

# What year had the highest total jail population?

race_highest_jail <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull() 

# How many races are included in the dataset and have their own column?



# What state has the highest total latinx jail population? DONE

incar %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
  pull(state, latinx_jail_pop)

# What is the total latinx jail population for the state of Washington?


# What is the difference between the total latinx jail population and the adult
# latinx jail population in the most recent year? 

cali_counties <- incar %>%
  group_by(county_name) %>%
  filter(state == "CA") %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  summarise(jail_cali = sum(latinx_jail_pop))



# What is the ratio between the Latinx population in California and the Latinx
# jail population in California?

# MAP TAKE 2 real one this time-----

library(tidyverse)
library(mapproj)
library(maps)
library(patchwork)

california_count <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(state == "CA") %>%
  select(year, fips, state, county_name, latinx_jail_pop) 

california_count$county_name = toupper(california_count$county_name)
stopwords = c("COUNTY")
california_count$county_name <- gsub(paste0(stopwords,collapse = "|"),"", california_count$county_name)

county_shapes <- map_data("county") %>%
  filter(region == "california") %>%
  filter(subregion == unique(subregion)) %>%
  rename(county_name = "subregion") 

news <-left_join(california_count, county_shapes)

county_data <- map_data("county") %>% # load state shapefile
  rename(county_name = subregion) %>% 
  left_join(california_count, by="county_name")
county_data$county_name = toupper(county_data$county_name)

the_map <- county_shapes %>%
  full_join(california_count, by = "county_name")

the_map$county_name = toupper(the_map$county_name)

counties <- map_data("county")
ca_county <- subset(counties, region == "california")

ggplot(data = ca_county, mapping = aes(x = long, y = lat, group = group, fill = subregion)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray") +
  theme_nothing() +
  geom_polygon(data = ca_county, fill = NA, color = "black") +
  geom_polygon(color = "white", fill = NA) +
  
  ca_base + 
  geom_polygon(data = cacopa, aes(fill = latinx), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes
scale_fill_gradientn(colours = rev(rainbow(7)),
                     breaks = c(2, 4, 10, 100, 1000, 10000),
                     trans = "log10")


# Map "percentage of total pop is latinx jail pop"-----------------------------

library(usmap) 
library(ggplot2)
library(dplyr)

latinx_state <- incar %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop/latinx_jail_pop) %>% 
  summarize(pop = sum(latinx_jail_pop), total = max(total_pop), mutate = sum(total_pop/latinx_jail_pop))

plot_usmap(data = latinx_state, values = "pop", color = "black") +
  coord_fixed(1) +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(10, 100, 1000, 10000),
                       trans = "log10") +
  labs(title = "The United States", subtitle = "Latinx Jail Population in 2014") +
  theme(legend.position = "right")


scale_fill_continuous(low = "chartreuse", high = "magenta", name = "Latinx Jail Population", label = scales::comma)
memory.limit()
memory.limit(size=10000)


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Scatterplot Chart------------------------------------------------------------

ggplot(data = latinx_state) +
  geom_point(mapping = aes(x = pop, y = total, fill = pop))

# Line Chart: Trends over Time-------------------------------------------------
jail_pop <- incar %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(native_jail_pop == native_jail_pop) %>%
  summarize(jail_total = sum(total_jail_pop), total_pop_15_64 = sum(total_pop_15to64),
            latinx_total = sum(latinx_jail_pop), white_total = sum(white_jail_pop),
            aapi_total = sum(aapi_jail_pop), black_total = sum(black_jail_pop),
            native_total = sum(native_jail_pop))

latinx_jail <- incar %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  summarise(latinx_total = sum(latinx_jail_pop))
library(ggplot2)

ggplot(data = jail_pop) +
  geom_line(mapping = aes(x = year, y = latinx_total, color = "black")) +
  geom_line(mapping = aes(x = year, y = aapi_total, color = "chartreuse")) +
  geom_line(mapping = aes(x = year, y = native_total, color = "peachpuff3")) +
  geom_line(mapping = aes(x = year, y = black_total, color = "red")) +
  geom_line(mapping = aes(x = year, y = white_total, color = "turquoise4")) +
  scale_color_manual(name="Races:",
                     values = c("black", "chartreuse", "peachpuff3", "red", "turquoise4"),
                     labels = c("Latinx", "AAPI", "Native", "Black", "White")) +
  scale_fill_manual(name="Races:",
                    values = c("black", "chartreuse", "peachpuff3", "red", "turquoise4"),
                    labels = c("Latinx", "AAPI", "Native", "Black", "White")) +
  ggtitle("Yearly Total Jail Population for each Race") +
  labs(x = "Year", y = "Jail Population")

library("stringr")
urlfile = ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incar <- read.csv(url(urlfile))


jail_pop <- incar %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(native_jail_pop == native_jail_pop) %>%
  summarize(jail_total = sum(total_jail_pop), total_pop_15_64 = sum(total_pop_15to64),
            latinx_total = sum(latinx_jail_pop), white_total = sum(white_jail_pop),
            aapi_total = sum(aapi_jail_pop), black_total = sum(black_jail_pop),
            native_total = sum(native_jail_pop))

latinx_jail <- incar %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  summarise(latinx_total = sum(latinx_jail_pop))
library(ggplot2)

ggplot(data = jail_pop) +
  geom_line(mapping = aes(x = year, y = latinx_total, color = "black")) +
  geom_line(mapping = aes(x = year, y = aapi_total, color = "chartreuse")) +
  geom_line(mapping = aes(x = year, y = native_total, color = "peachpuff3")) +
  geom_line(mapping = aes(x = year, y = black_total, color = "red")) +
  geom_line(mapping = aes(x = year, y = white_total, color = "turquoise4")) +
  scale_color_manual(name="Races:",
                     values = c("black", "chartreuse", "peachpuff3", "red", "turquoise4"),
                     labels = c("Latinx", "AAPI", "Native", "Black", "White")) +
  scale_fill_manual(name="Races:",
                    values = c("black", "chartreuse", "peachpuff3", "red", "turquoise4"),
                    labels = c("Latinx", "AAPI", "Native", "Black", "White")) +
  ggtitle("Yearly Total Jail Population for each Race") +
  labs(x = "Year", y = "Jail Population")
