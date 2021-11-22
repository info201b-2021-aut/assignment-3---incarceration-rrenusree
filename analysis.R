#analysis for a3

incarceration_trends <- read.csv("/Users/renusree/Desktop/UW_1YEAR/INFO201/assignment-3---incarceration-rrenusree/incarceration-trends/incarceration_trends.csv")

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

#The variables I am choosing to analyze are the total population counts for every county
#the black population in every county, total male population in each county, total
#population in jail, black population in jail, and male population in jail.

sum_1990_jailpop_incarceration_wa <- sum(wa_1990_incarceration_trends$total_jail_pop)
sum_2018_jailpop_incarceration_wa <- sum(wa_2018_incarceration_trends$total_jail_pop)
difference_jailpop_incarceration <- sum_2018_jailpop_incarceration_wa - sum_1990_jailpop_incarceration_wa
percent_increase_incarceration_wa <- (difference_incarceration/sum_1990_incarceration_wa)*100


sum_jailmales_1990_wa <- sum(wa_1990_incarceration_trends$male_adult_jail_pop)
sum_jailmales_2018_wa <- sum(wa_2018_incarceration_trends$male_adult_jail_pop)
difference_males <- sum_jailmales_2018_wa - sum_jailmales_1990_wa ##

percent_jail_males_1990 <- (sum_jailmales_1990_wa/sum_1990_jailpop_incarceration_wa) * 100 ##
percent_jail_males_2018 <- (sum_jailmales_2018_wa/sum_2018_jailpop_incarceration_wa) * 100 ##

county_highest_blackjailpop <- selected_incarceration_trends[selected_incarceration_trends$black_jail_pop == max(selected_incarceration_trends$black_jail_pop), "county_name"] ##
year_highest_blackjailpop <- selected_incarceration_trends[selected_incarceration_trends$black_jail_pop == max(selected_incarceration_trends$black_jail_pop), "year"] ##


#Trends over time chart


#line chart - graph 3 years : 1990, , 2018 for different groups
#groups - percent of black population in general
        #- percent of black population in jail
        #- percent of males in general
        #- percent of males in jail 

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

#line chart 

linechart <- ggplot()+
  geom_line(data = grouped_blackpopjail_everyyear, mapping = aes(x=year, y=percentage_in_jail), color = "red") +
  geom_line(data = grouped_asianpopjail_everyyear, mapping = aes(x=year, y=sum_asianjail), color = "blue") +
  ggtitle("Racial Percentage in jail vs year")


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
    mapping = aes(x = latinx_pop_percentage, y = latinx_jail_pop_by_totalpop, color = commuting_zone),
    alpha = .6
  ) +
  
  # Add title and axis labels
  labs(
    title = "dk", # plot title
    x = "hii", # x-axis label
    y = "byee", # y-axis label
    color = "commuting" # legend label for the "color" property
  )

wa_1990_incarceration_trends <-mutate(wa_1990_incarceration_trends,
                                 latinx_jail_pop_by_totalpop = (latinx_jail_pop/total_jail_pop)*100)

wa_1990_incarceration_trends <-mutate(wa_1990_incarceration_trends,
                                 latinx_pop_percentage = (latinx_pop_15to64/total_pop)*100)

#1970 - 2018 for each county 



state_shape <- map_data("state")

# Create a blank map of U.S. states

mean_date <- incarceration_trends %>%
  filter(year == mean(year))
         
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by="polyname")

map_data <- county_shapes %>%
  left_join(mean_date, by="fips") %>%
  filter(state == "TX") 

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
  ggtitle("Jail")




