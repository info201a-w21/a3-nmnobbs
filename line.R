
library("stringr")
urlfile = ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incar <- read.csv(url(urlfile))

library("ggplot2")
library("dplyr")


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

line_chart <- ggplot(data = jail_pop) +
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



# Assignment 3
# Load the data into a variable `incarceration`
library("stringr")
urlfile = ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incar <- read.csv(url(urlfile))

library("ggplot2")
library("dplyr")

# Introduction + Summary ------------------------------------------------------

# In 2018, which state has had the highest total jail population in the
# dataset? Store the state and population number in
# the variable `jail_highest_state`

jail_highest_state <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(state, total_jail_pop)

# What year in a single row had the highest total jail population? Store in
# the variable `year_highest_jail`

year_highest_jail <- incar %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(year) 

# What state has the highest total Latinx jail population in the most recent
# year? Store the state and number in the variable `high_state_latinx`

high_state_latinx <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
  pull(state, latinx_jail_pop)

# What is the total Latinx jail population in the state of California?
# Combine the amount from each year and store in the variable `ca_latinx_jail`

ca_latinx_jail <- incar %>%
  group_by(year) %>%
  filter(state == "CA") %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  summarize(amount = sum(latinx_jail_pop)) %>%
  sum(ca_latinx_jail[, 'amount'], na.rm = TRUE)

# What is the ratio between the Latinx population in California and the Latinx
# jail population in California in the most recent year of the dataset?
# Store in the variable `latinx_ratio`

latinx_ratio <- incar %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop/latinx_jail_pop) %>% 
  summarize(latinx_jail = sum(latinx_jail_pop), total = sum(total_pop),
            ratio = sum(latinx_jail_pop/total_pop)) %>% 
  filter(state == "CA") %>% 
  pull(ratio)

# Map -------------------------------------------------------------------------

library(usmap) 
library(ggplot2)
library(dplyr)

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

latinx_state <- incar %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop/latinx_jail_pop) %>% 
  summarize(pop = sum(latinx_jail_pop), total = max(total_pop), mutate = sum(total_pop/latinx_jail_pop))


map <- plot_usmap(data = latinx_state, values = "pop", color = "black", name = "Latinx Jail Population") +
  coord_fixed(1) +
  blank_theme +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(10, 100, 1000, 10000),
                       trans = "log10", name = "Latinx Jail Population") +
  labs(title = "The United States", subtitle = "Latinx Jail Population in 2014",
       name = "Latinx Jail Population") +
  theme(legend.position = "right")


# Scatterplot Chart------------------------------------------------------------


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

line_chart <- ggplot(data = jail_pop) +
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
