library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)
library(dplyr)

library(extrafont)
library(showtext)
#font_import()
loadfonts(device = "win")

our_theme <- theme_minimal() + 
theme(text=element_text(family="ETBembo"))

countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czech Republic")
#download.file("https://github.com/owid/covid-19-data/raw/master/public/data/jhu/full_data.csv", "data/jhu.csv")
df <- read_csv("data/jhu.csv", 
                    col_types = cols_only( 
                      date = col_date(),
                      new_cases = col_double(),
                      location = col_character())) %>% 
  filter(location %in% countries)

df%>% 
  rename(Country = location)-> df

g <- ggplot(df,# %>% filter(Country == "Czech Republic") , 
            aes(x = date, 
                y = rollmean(new_cases, 14, na.pad=TRUE, align = "right"), 
                color = Country)) + 
  geom_line() + 
  our_theme 
ggplotly(g)

n = 7

df$new_cases_mean = rollmean(df$new_cases, n, na.pad=TRUE, align = "right")
df %>% 
  group_by(Country) %>% 
  arrange(Country, date) %>% 
  mutate(rate = 100 * (new_cases_mean - lag(new_cases_mean, n))/lag(new_cases_mean, n)) %>%
  mutate_if(is.numeric, list(~na_if(., -Inf)))%>%
  ungroup() -> df2

g<-ggplot(df2 %>% filter(Country %in% c("Sweden", "Germany", "Denmark")), #g<-ggplot(df2,
       aes(x = date, 
           #           y = rollmean(rate, n, na.pad=TRUE, align = "right"), 
            y = rate, 
           color = Country)) + 
  geom_smooth(method="auto" , se=TRUE, span = 0.3, aes(y=rate), level = 0.75) +
  #ylim(-50, 110) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2020-10-01','2020-12-02'))) +
  our_theme
ggplotly(g)
g

df3 = transform(df2,Country=factor(Country,levels=countries))

aes_BEL = aes(xmin = as.Date('2020-11-02'), xmax = as.Date('2020-12-01'), ymin = -Inf, ymax = Inf)
aes_GER = aes(xmin = as.Date('2020-11-02'), xmax = as.Date('2020-12-01'), ymin = -Inf, ymax = Inf)
aes_SWE = aes(xmin = as.Date('2020-11-08'), xmax = as.Date('2020-12-01'), ymin = -Inf, ymax = Inf)
aes_FRA = aes(xmin = as.Date('2020-10-30'), xmax = as.Date('2020-12-01'), ymin = -Inf, ymax = Inf)
#library(plyr)  
g<-ggplot(df3 ,#%>% filter(Country  %in% c("Belgium", "France", "Czech Republic")), #g<-ggplot(df2,
          aes(x = date, 
              #           y = rollmean(rate, n, na.pad=TRUE, align = "right"), 
              y = rate, 
              color = Country)) +
  geom_rect(data=df3[df3["Country"]=="Belgium",], aes_BEL, fill = '#F0F0F0') + 
  geom_rect(data=df3[df3["Country"]=="Germany",], aes_GER, fill = '#F0F0F0') + 
  geom_rect(data=df3[df3["Country"]=="Sweden",], aes_SWE, fill = '#F0F0F0') + 
  geom_rect(data=df3[df3["Country"]=="France",], aes_FRA, fill = '#909090') + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_smooth(method="auto" , se=TRUE, span = 0.3, aes(y=rate), level = 0.95) +
  ylab("Rate of change") +
  scale_y_continuous(labels = scales::percent_format( scale = 1.0, accuracy = 1), limits = c(-60, 125)) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2020-10-01','2020-12-02'))) +
  our_theme +
  facet_wrap(~ Country)
g 
ggplotly(g)
