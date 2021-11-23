#analysis for a3


incarceration_trends <- read.csv("~/Desktop/UW_1YEAR/INFO201/assignment-3---incarceration-rrenusree/incarceration_trends.csv")

library("knitr")
library("tidyverse")
library("maps")
library("mapproj")
library("patchwork")
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gcookbook)
  


selected_incarceration_trends <- na.omit(select(incarceration_trends, c('year','total_pop','black_pop_15to64', 'male_pop_15to64', 'total_jail_pop', 'black_jail_pop', 'male_adult_jail_pop', 'county_name', 'state', 'aapi_jail_pop', 'aapi_pop_15to64', 'latinx_pop_15to64', 'latinx_jail_pop', 'commuting_zone')))
wa_incarceration_trends <- na.omit(selected_incarceration_trends[selected_incarceration_trends$state == "WA",])
wa_1990_incarceration_trends <- na.omit(wa_incarceration_trends[wa_incarceration_trends$year == "1990",])

wa_2018_incarceration_trends <- na.omit(wa_incarceration_trends[wa_incarceration_trends$year == "2018",])

sum_1990_jailpop_incarceration_wa <- sum(wa_1990_incarceration_trends$total_jail_pop)
sum_2018_jailpop_incarceration_wa <- sum(wa_2018_incarceration_trends$total_jail_pop)
difference_jailpop_incarceration <- sum_2018_jailpop_incarceration_wa - sum_1990_jailpop_incarceration_wa
percent_increase_incarceration_wa <- (difference_jailpop_incarceration/sum_1990_jailpop_incarceration_wa)*100


sum_jailmales_1990_wa <- sum(wa_1990_incarceration_trends$male_adult_jail_pop)
sum_jailmales_2018_wa <- sum(wa_2018_incarceration_trends$male_adult_jail_pop)
difference_males <- sum_jailmales_2018_wa - sum_jailmales_1990_wa ##

percent_jail_males_1990 <- (sum_jailmales_1990_wa/sum_1990_jailpop_incarceration_wa) * 100 ##
percent_jail_males_2018 <- (sum_jailmales_2018_wa/sum_2018_jailpop_incarceration_wa) * 100 ##

county_highest_blackjailpop <- selected_incarceration_trends[selected_incarceration_trends$black_jail_pop == max(selected_incarceration_trends$black_jail_pop), "county_name"] ##
year_highest_blackjailpop <- selected_incarceration_trends[selected_incarceration_trends$black_jail_pop == max(selected_incarceration_trends$black_jail_pop), "year"] ##


wa_incarceration_trends <-mutate(wa_incarceration_trends,
                                 black_jail_pop_by_totalpop = (black_jail_pop/total_jail_pop)*100)

wa_incarceration_trends <-mutate(wa_incarceration_trends,
                                 latinx_jail_pop_by_totalpop = (latinx_jail_pop/total_jail_pop)*100)

wa_incarceration_trends <-mutate(wa_incarceration_trends,
                                 latinx_pop_percentage = (latinx_pop_15to64/total_pop)*100)
                                 

grouped_blackpopjail_everyyear <- wa_incarceration_trends %>%
  group_by(year) %>%
  summarize(percentage_in_jail = (sum(black_jail_pop)/sum(total_jail_pop))*100, na.rm = TRUE)

grouped_asianpopjail_everyyear <- wa_incarceration_trends %>%
  group_by(year) %>%
  summarize(sum_asianjail = (sum(aapi_jail_pop)/sum(total_jail_pop))*100, na.rm = TRUE)

grouped_latinxpopjail_everyyear <- wa_incarceration_trends %>%
  group_by(year) %>%
  summarize(sum_latinojail = (sum(latinx_jail_pop)/sum(total_jail_pop))*100, na.rm = TRUE)

grouped_races <- data.frame(grouped_blackpopjail_everyyear, grouped_asianpopjail_everyyear, grouped_latinxpopjail_everyyear)

#line chart 

linechart <- ggplot(data = grouped_races, mapping = aes(x=year))+
  geom_line(mapping = aes(y=percentage_in_jail, color = "Black")) +
  geom_line(mapping = aes(y=sum_asianjail, color = "Asian")) +
  geom_line(mapping = aes(y=sum_latinojail, color = "Latino")) +
  labs(x= "year", y = "Percentage in Jail", title = "Percentage in Jail vs Year") 
  

#more calculations:

black_pop_1990_wa <- sum(wa_1990_incarceration_trends$black_pop_15to64)
total_pop_1990_wa <- sum(wa_1990_incarceration_trends$total_pop)
percent_black_pop_1990_wa <- (black_pop_1990_wa/total_pop_1990_wa)*100 #

male_pop_1990_wa <- sum(wa_1990_incarceration_trends$male_pop_15to64)
percent_male_pop_1990_wa <- (male_pop_1990_wa/total_pop_1990_wa)*100 #

black_jailpop_1990_wa <- na.omit(sum(wa_1990_incarceration_trends$black_jail_pop))
total_jailpop_1990_wa <- na.omit(sum(wa_1990_incarceration_trends$total_jail_pop))
percent_black_jailpop_1990_wa <- (black_jailpop_1990_wa/total_jailpop_1990_wa)*100 #

male_jailpop_1990_wa <- sum(wa_1990_incarceration_trends$male_adult_jail_pop)
percent_male_jailpop_1990_wa <- (male_jailpop_1990_wa/total_jailpop_1990_wa)*100 #

black_pop_2018_wa <- sum(wa_2018_incarceration_trends$black_pop_15to64)
total_pop_2018_wa <- sum(wa_2018_incarceration_trends$total_pop)
percent_black_pop_2018_wa <- (black_pop_2018_wa/total_pop_2018_wa)*100 #

male_pop_2018_wa <- sum(wa_2018_incarceration_trends$male_pop_15to64)
percent_male_pop_2018_wa <- (male_pop_2018_wa/total_pop_2018_wa)*100

black_jailpop_2018_wa <- sum(wa_2018_incarceration_trends$black_jail_pop)
total_jailpop_2018_wa <- sum(wa_2018_incarceration_trends$total_jail_pop)
percent_black_jailpop_2018_wa <- (black_jailpop_2018_wa/total_jailpop_2018_wa)*100 #

male_jailpop_2018_wa <- sum(wa_2018_incarceration_trends$male_adult_jail_pop)
percent_male_jailpop_2018_wa <- (male_jailpop_2018_wa/total_jailpop_2018_wa)*100 #

#scatterplot 

scatterplot <- ggplot(data = wa_1990_incarceration_trends) +
  geom_point(
    mapping = aes(x = wa_1990_incarceration_trends$latinx_pop_percentage1990, y = wa_1990_incarceration_trends$latinx_jail_pop_by_totalpop),
    alpha = .6
  ) +
  
  # Add title and axis labels
  labs(
    title = "Percentage of Latino population in jail vs Percentage of Latino population", # plot title
    x = "Percentage of Latino population", # x-axis label
    y = "Percentage of Latino population in jail", # y-axis label
    
  )

wa_1990_incarceration_trends <-mutate(wa_1990_incarceration_trends,
                                 latinx_jail_pop_by_totalpop = (latinx_jail_pop/total_jail_pop)*100)

wa_1990_incarceration_trends <-mutate(wa_1990_incarceration_trends,
                                 latinx_pop_percentage1990 = (latinx_pop_15to64/total_pop)*100)


# Create a map of Washington state and the prison population in 2018

max_date <- incarceration_trends %>%
  filter(year == max(year))
         
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by="polyname")

map_data <- county_shapes %>%
  left_join(max_date, by="fips") %>%
  filter(state == "WA") 

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

incarceration_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x= long, y=lat, group= group, fill = total_jail_pop),
    color = "gray", size = 0.3
    
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$total_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Jail population count across Washington")




