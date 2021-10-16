library(tidyverse)
library(janitor)
library(httr)
library(XML)
library(extrafont)
library(showtext)
#create the data


Countries <- c("Belgium","United Kingdom","Japan","Italy","China","Germany","France","India","Spain","Philippines","Russia","Cuba","United States","Mexico",'Nigeria',"South Africa","Brazil","Argentina","Canada","Australia")

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population_density"

r <- GET(url)

doc <- readHTMLTable(
  doc=content(r, "text"))

density_table <- doc[[1]]

density_table <- density_table %>% row_to_names(row_number = 1)

density_table <- density_table[2:nrow(density_table),]

names(density_table) <- c("country","population","area_mi","area_km","pop_km","pop_mi","source_year")

density_table<- density_table %>% mutate(country = gsub('\\*.*',"",country))

density_table <-  density_table %>% mutate(source_year = str_match(source_year,"\\d{4}"))

density_table <- density_table %>% mutate(country = str_trim(country))

final_table <- density_table %>% filter(country %in% Countries) %>% select(country,pop_mi)

#Create points

data <- final_table %>%
  mutate(
    pop_mi = as.numeric(gsub(",","",pop_mi)),
    sqrt = sqrt(pop_mi)
    
    ) %>% 
  mutate(
    r1 = ceiling(sqrt),
    r2 = floor(sqrt),
    total = r1*r2,
    r2 = ifelse(total<pop_mi,r2+1,r2),
    total = r1*r2
  ) %>%
  rowwise() %>% 
  mutate(
    points = list(expand.grid(1:r1,1:r2))
  ) %>%
  ungroup() %>% 
  mutate(
    points = map2(pop_mi,points,~.y[(nrow(.y)-.x+1):nrow(.y),])
  ) %>% 
  select(country,pop_mi,r1,r2,points) %>% 
  unnest(points) %>% 
  mutate(country = paste(country,pop_mi),
         country = fct_reorder(country,- pop_mi),
         Var1 = (Var1-1)/(r1-1),
         Var2 = (Var2-1)/(r2-1)) 

#create plot


p <- data %>% ggplot()+
  geom_point(aes(x=Var1,y=Var2),size = 0.8,position = position_jitter(0.001,0.001))+
  facet_wrap(~country)+
  ggtitle("Density of Populations of Some of the Countries.")+
  theme(strip.placement = "inside",
        strip.text = element_text(size=30),
        legend.title=element_blank(),
        legend.text=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        plot.background = element_rect(fill="#f9e9c8"), 
        panel.background=element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.3),
        strip.background = element_blank(),
        plot.title = element_text(hjust =0.5,size = 50),
        plot.margin = unit(c(0.5, 1.5, 1.5, 1.5), "cm"),
        text=element_text(family="Special Elite")
        )

#save plot
ggsave("Population.png",p,width = 8,height=8)
