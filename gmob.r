library(tidyverse)
library(readr)
library(plotly)
library(zoo)
#library(extrafont)
#loadfonts(device = "win")

#download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", "data/Global_Mobility_Report.csv")

Global_Mobility_Report <- read_csv("data/Global_Mobility_Report.csv", 
                                   col_types = cols(census_fips_code = col_skip(), 
                                                    country_region_code = col_skip(), 
                                                    iso_3166_2_code = col_skip()))

countries <- c("Sweden", "Belgium", "France", "Germany", "Czechia", "Denmark")

cols <- c("country", "Date", "Retail & recreation",
             "Grocery & pharmacy",
             "Parks", 
             "Transit stations", 
             "Workplaces",
             "Residential_change")

colors <- c("#AF707070", "#7070AF70", "#AF70AF70", "#AFAF7070", "#70AFAF70", "#00AF00")

All_Mobility_Data <- Global_Mobility_Report %>% 
  dplyr::select(- sub_region_2) %>%
  dplyr::select(- metro_area) %>% 
  filter( is.na(sub_region_1)) %>% 
  select(-sub_region_1)

colnames(All_Mobility_Data) <- cols

All_Mobility_Data_long <- All_Mobility_Data %>% 
  gather(Mobility_Type, Percent_Change, -c(Date, country))


Subset_Mobility_Data_long <- All_Mobility_Data_long %>% 
  filter(country %in% countries) 

g <- ggplot(Subset_Mobility_Data_long %>% filter(country == "Sweden") ,
            aes(x = Date, 
                y = rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"), 
                color = Mobility_Type)) + 
  geom_line() + 
  theme_light() +
  ggtitle("Google mobility report - Sweden") +
  ylab("Change from baseline") +
  ylim(-75, 25) 
#  geom_vline(xintercept=as.numeric(black_friday), linetype="dashed", color = "black") +
#  annotate("text", x=black_friday, y=-75, label="Black friday ", hjust = "right")

ggplotly(g)%>% style(textposition = "left")

black_friday <- as.Date("2020-11-27")

g <- ggplot(All_Mobility_Data_long %>% filter(country == "Germany") , 
            aes(x = Date, 
                y = rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"), 
                color = Mobility_Type)) + 
  geom_line() + 
  theme_light() +
  theme(text=element_text(family="ETBembo")) +
  ggtitle("Google mobility report - Germany") +
  ylab("Change from baseline") +
  ylim(-75, 25) 
#  geom_vline(xintercept=as.numeric(black_friday), linetype="dashed", color = "black") +
#  annotate("text", x=black_friday, y=-75, label="Black friday ", hjust = "right")
  
ggplotly(g)%>% style(textposition = "left")
g










#library(showtext)
#font_paths()    
#font_add("et-book", "et-book-roman-line-figures.ttf")
#font_families()
#windowsFonts()

#library(extrafont)
#font_import()
#loadfonts(device = "win")


#  R n R
g <- ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == "Retail & recreation"),
          aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
              color = country)) + 
  geom_line() +
  theme_light() +
  theme(text=element_text(family="ETBembo")) +
  ggtitle("Google mobility report - Retail & recreation") +
  ylab("Change from baseline") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(name = "Country", values = colors)

ggplotly(g)
g


#  Work
g <- ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == "Workplaces"),
            aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
                color = country)) + 
  geom_line() +
  theme_light() +
  theme(text=element_text(family="ETBembo")) +
  ggtitle("Google mobility report - Workplaces") +
  ylab("Change from baseline") +
  scale_color_manual(name = "Country", values = colors)

ggplotly(g)
g

#  Parks
g <- ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == "Parks"),
            aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
                color = country)) + 
  geom_line() +
  theme_light() +
  theme(text=element_text(family="ETBembo")) +
  ggtitle("Google mobility report - Parks") +
  ylab("Change from baseline") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(name = "Country", values = colors)

ggplotly(g)
g

#  Transit stations
g <- ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == "Transit stations"),
            aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
                color = country)) + 
  geom_line() +
#  geom_smooth(method="auto" , se=TRUE, span = 0.3) +
#geom_smooth(method="auto" , se=TRUE, span = 0.1, aes(y=Percent_Change, linetype="None")) +
  theme_light() +
  theme(text=element_text(family="ETBembo")) +
  ggtitle("Google mobility report - Transit stations") +
  ylab("Change from baseline") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(name = "Country", values = colors)

ggplotly(g)
g
