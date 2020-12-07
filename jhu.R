library(tidyverse)
library(ggplot2)
library(plotly)
library(zoo)
library(dplyr)
library(scales)

library(extrafont)
# this line is not very portable. You might need to remove the references to ETBembo below
font_import(paths="~/Library/Fonts/", pattern = "*et*", prompt = FALSE)

our_theme <- theme_minimal() + 
  theme(text=element_text(family="ETBembo-RomanLF"))

countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czech Republic", "Czechia", "Austria", "United Kingdom", "Netherlands")
countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czech Republic", "Czechia", "Austria", "United Kingdom", "Slovakia")

#download.file("https://github.com/owid/covid-19-data/raw/master/public/data/jhu/full_data.csv", "data/jhu.csv")
df <- read_csv("data/jhu.csv", 
                    col_types = cols_only( 
                      date = col_date(),
                      new_cases = col_double(),
                      location = col_character())) %>% 
  filter(location %in% countries) %>% 
  rename(Country = location, Date = date) -> df

# normalize by population
#download.file("https://github.com/owid/covid-19-data/raw/master/public/data/jhu/locations.csv", "data/jhu_pop.csv")
df_pop <- read_csv("data/jhu_pop.csv")
df_pop%>% 
  rename(Country = location)-> df_pop
df$new_cases_per_cap <- df$new_cases*100000 / df_pop$population[match(df$Country, df_pop$Country)]


n = 7
df$new_cases_mean <- rollmean(df$new_cases, n, na.pad = TRUE, align = "right")
df$mean_cases_per_cap <- rollmean(df$new_cases_per_cap, 14, na.pad = TRUE, align = "right")

df %>% 
  group_by(Country) %>% 
  arrange(Country, Date) %>% 
  mutate(rate = 100 * (new_cases_mean - lag(new_cases_mean, n))/lag(new_cases_mean, n)) %>%
  mutate_if(is.numeric, list(~na_if(., -Inf)))%>%
  ungroup() -> df_jhu

write_csv(df_jhu, "data/subset_jhu.csv")

df_jhu <- read_csv("data/subset_jhu.csv")
df_jhu <- transform(df_jhu, Country=factor(Country,levels=countries))

g <- ggplot(df_jhu,
            aes(x = Date, 
                y = mean_cases_per_cap, 
                color = Country)) + 
  geom_line() +
  ggtitle("Covid-19 Second wave - Cases per capita") +
  ylab("Cases per 100,000") +
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-10-01','2020-12-04')),
               labels = date_format("%b")) +
  our_theme +
  theme(legend.title = element_blank(), legend.position = "none") +
  facet_wrap(~ Country)

ggsave(scale = 0.6, paste("img/", "jhu_cases", ".png"), plot = g)
ggplotly(g)
g

g <- ggplot(df_jhu,
          aes(x = Date, 
              y = rate, 
              color = Country)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "auto" , se = TRUE, span = 0.3, aes(y=rate), level = 0.999, size = 0.5) +
  ylab("Cases - weekly RoC") +
  ggtitle("Covid-19 Second wave - Rate of Change") +
  scale_y_continuous(labels = scales::percent_format( scale = 1.0, accuracy = 1), limits = c(-75, 125)) +
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-10-01','2020-12-04')),
               labels = date_format("%b")) +
  our_theme +
  theme(legend.title = element_blank(), legend.position = "none") +
  facet_wrap(~ Country)
g 
ggsave(scale = 0.6, paste("img/", "jhu_roc", ".png"), plot = g)
ggplotly(g)

