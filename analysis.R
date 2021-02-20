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

# What race accounted for the majority of the highest total jail population in
# 2018? Store in the variable `race_highest_jail`

race_highest_jail <- incar %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(aapi_jail_pop, white_jail_pop) 

# Which race has the highest total jail population in the most recent year?



# what percentage of the total jail population does the race in question 3
# account for in the state that has the highest jail population?



# What is the ratio between the Latinx population in California and the Latinx
# jail population in California?


# Map--------------------------------------------------------------------------




# Scatterplot Chart------------------------------------------------------------

ggplot(data = jail_pop) +
  geom_point(mapping = aes(x = latinx_total, y = aapi_total, color = latinx_total)) +
  geom_point(mapping = aes(x = aapi_total, y = region))

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
  summarize(jail_total = sum(total_jail_pop), latinx_total = sum(latinx_jail_pop),
            white_total = sum(white_jail_pop), aapi_total = sum(aapi_jail_pop),
            black_total = sum(black_jail_pop), native_total = sum(native_jail_pop))

latinx_jail <- incar %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  summarise(latinx_total = sum(latinx_jail_pop))

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
  xlab("Year") +
  ylab("Jail Population")


