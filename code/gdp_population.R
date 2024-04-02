library(tidyverse)
library(tidyverse)
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











