########################################################
## Setup section
#######################################################
library(tidyverse)
library(ggplot2)

######################################################
## Day 1 work: Intro to R and Plotting
######################################################

gapminder_1997 <- read_csv("data/gapminder_1997.csv")

add_two <- 2+2
name <- "Rose"
name <- "Milo"

Sys.Date()
getwd()
?read_csv()

sum(5,6)
?round()
round(3.1415)
round(x=3.1415,digits=3)

# using ggplot to make a plot
ggplot(data=gapminder_1997) + 
  aes(x = gdpPercap , y = lifeExp , color = continent , size = pop/1000000) +
  labs(x = "GDP Per Capita" , y = "Life Expectancy" , 
       title = "Do people in wealthy countries live longer?" , 
       size = "populations (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
  

RColorBrewer::display.brewer.all()

#load in full gapminder dataset
gapminder_data <- read_csv("data/gapminder_data.csv")

dim(gapminder_data)
head(gapminder_data)
glimpse(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp) +
  geom_boxplot()

ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp) +
  geom_violin() + 
  geom_jitter(width = 0.15, alpha = 0.2)

ggsave("gdpPercap_lifeExp.png")

#####################################################
## Day 2 work: Data manipulation and Cleaning
#####################################################

gapminder_data <- read_csv("data/gapminder_data.csv")
glimpse(gapminder_data)
View(gapminder_data)
summarize(gapminder_data, avglifeExp = mean(lifeExp))
summarize(gapminder_data, MaxlifeExp = max(lifeExp))

#Pipes
#%>%
#means "and then do this thing"

#option 1 without a pipe
summarize(gapminder_data, avglifeExp = mean(lifeExp)

#option 2 with a pipe
gapminder_data %>% summarize(avglifeExp = mean(lifeExp))

#save the summary table
gapminder_data_summary <- gapminder_data %>% summarize(avglifeExp = mean(lifeExp))

gapminder_data %>% filter(year == 2007) %>% 
  summarize(avglifeExp07 = mean(lifeExp))
#find the earliest year in the dataset using summarize and min
gapminder_data %>% summarize(startpoint = min(year))

#filter the data to 1952 only and find avg GDP per capita
gapminder_data %>% filter(year == 1952) %>%
  summarize(avgGDPperCap = mean(gdpPercap))

#calculate life expectancy by year
gapminder_data %>% group_by(year) %>% 
  summarize(avg = mean(lifeExp))

#calculate life expectancy by continent
gapminder_data %>% group_by(continent) %>% 
  summarize(avg = mean(lifeExp), min = min(lifeExp), max = max(lifeExp))


#adding columns to the dataset using mutate function
gapminder_data %>% mutate(gdp = gdpPercap * pop),

#we have a column called pop 
#use mutate to create a column for popInMillions
gapminder_data %>% mutate(popInMillions = pop/1000000),

#filter to select particular rows 
#select to select column
gapminder_data %>% select(pop, year),

gapminder_data %>% select (-continent ),

#print a dataframe with the country, continent, year and lifeExp
gapminder_data %>% select(country, continent, year, lifeExp),
gapminder_data %>% select(year, starts_with("c")),

#print dataframe of all cols that end in the letter "p"
gapminder_data %>% select(ends_with("p")),

gapminder_data %>% select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp),

# save a dataframe that contains only the Americas in 2007
#| is for OR
#& is for AND
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% select (-year, -continent),

#cleaning messy data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, col_names = c("region_number", "country", "year", "series", "values", "footnotes", "source")),

# select the country, year, series, and value columns
co2_emissions_dirty %>% select(country, year, values) %>%
  print (n= 50),
co2_emissions_dirty %>% select(country, year, series, values) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = values),

view (co2_emissions_dirty),


#how to join two data frames using tidyverse
inner_join(gapminder_data, co2_emissions_dirty, by ="country"),
anti_join(gapminder_data, co2_emissions_dirty, by="country"),

co2_emissions <- read_csv("data/co2-un-data.csv", skip=2, col_names = c("region", "country", "year", "series", "values", "footnotes", "source")) %<% select(country, year, series, values) %<% mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                                                                                                                                                                                                    "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, value_fill = values) %<% filter (year = 2005) %<% select(-year) %<% mutate(country = recode(country, "Bolivia(Plurin. State of)" = "Bolivia", "United States of America" = "United States", "Venezuela (Boliv. Rep. of)" = "Venezuela")),

anti_join(gapminder_data, co2_emissions, by ="country"),

#address Puerto Rico
gapminder_data <-gapminder_data %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")),

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by ="country"),

gapminder_co2 %>% group_by(continent) %>% summarize(avglifeExp = mean(lifeExp)),

gapminder_co2 %>% filter(continent =="Americas") %>% 
  mutate(region= ifelse(country == "United States" | country == "Canada" | country == "Mexico", "north", "south")),

#I want the Americas in 2007
gapminder_co2 %>% filter(continent =="Americas" & year.x==2007) %>% 
  mutate(region= ifelse(country == "United States" | country == "Canada" | country == "Mexico", "north", "south"))





































































