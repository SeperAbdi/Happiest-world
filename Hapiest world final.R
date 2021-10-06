
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(kableExtra)
library(plotly)
library(gridExtra)
library(ggrepel)

#show data 

data2015 <- read.csv(file = '2015.csv')
data_2015 <- subset(data2015,select=c(Country, Region, HappyScore, lifeExp, Freedom))

#Show a bubble plot

data_2015 %>%
  mutate(lifeExp=lifeExp*100) %>% 
  mutate(Freedom=Freedom*100) %>%
  mutate(HappyScore) %>%
  arrange(desc(HappyScore)) %>%
  mutate(Country = factor(Country, Country)) %>%
  ggplot( aes(x=Freedom, y=lifeExp))+
  geom_point(aes(color = Region, size =HappyScore))
    theme(legend.position="bottom")+
    geom_point(alpha=0.7) +
    scale_size(range = c(1.4, 10), name="Happiest Score") +
    scale_color_viridis(discrete=TRUE, guide=FALSE) +
    theme_ipsum() +
    theme(legend.position="bottom")


#other combination 
d2 <-ggplot(data = data_2015, aes(x=Freedom, y=HappyScore))+
  geom_point(aes(color = Region, size =lifeExp))+
  scale_color_viridis(discrete=TRUE) +
  scale_y_log10() +
  theme_ipsum() +
  theme(legend.position="bottom") 

d3 <-ggplot(data = data_2015, aes(x=lifeExp, y=HappyScore))+
  geom_point(aes(color = Region, size =lifeExp)) +
  guides(colour = guide_colourbar(order = 1))+
       
  scale_color_viridis(discrete=TRUE) +
  scale_y_log10() +
  theme_ipsum() +
 theme(legend.position="none") 

grid.arrange(d2,d3, ncol=1)
#in these 2 plot I put one scale to show better how much freedom and happiness 
#are related to each other in a same countries
#######################################
#Simple Bubble Chart with name of countries
library(plotly)

data <- read.csv(file = '2015.csv')

d4 <- plot_ly(data, x = ~lifeExp, y = ~Freedom, text = ~Country, type = 'scatter', mode = 'markers',
               color = ~HappyScore, colors = 'Blues',
               marker = list(size = ~HappyScore, opacity = 0.5))
d4 <- d4 %>% layout(title = 'Happiest Country in the World',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))

d4
