library(tidyverse)
library(readr)
library(plotly)
library(zoo)
library(extrafont)
#loadfonts(device = "win")
library(tidyverse)
countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czechia")

cols <- c("country", "Date", "Retail & recreation",
          "Grocery & pharmacy",
          "Parks", 
          "Transit stations", 
          "Workplaces",
          "Residential_change")

#download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", "data/Global_Mobility_Report.csv")
Global_Mobility_Report <- read_csv("data/Global_Mobility_Report.csv", 
                                   col_types = cols(census_fips_code = col_skip(), 
                                                    country_region_code = col_skip(), 
                                                    iso_3166_2_code = col_skip()))



All_Mobility_Data <- Global_Mobility_Report %>% 
  dplyr::select(- sub_region_2) %>%
  dplyr::select(- metro_area) %>% 
  filter( is.na(sub_region_1)) %>% 
  select(-sub_region_1)

colnames(All_Mobility_Data) <- cols

Subset_Mobility_Data_long <- All_Mobility_Data %>% 
  gather(Mobility_Type, Percent_Change, -c(Date, country)) %>% 
  filter(country %in% countries) 

write_csv(Subset_Mobility_Data_long, "data/Subset_Mobility_Data_long.csv")

Subset_Mobility_Data_long <- read_csv("data/Subset_Mobility_Data_long.csv")
  
g <- ggplot(Subset_Mobility_Data_long %>% 
              filter(country == "Sweden")%>%
              filter(Mobility_Type != "Parks") ,
            aes(x = Date, 
                y = rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"), 
                color = Mobility_Type)) + 
  geom_line() + 
  theme_light() +
  ggtitle("Google mobility report - Sweden") +
  ylab("Change from baseline") 

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
  #geom_vline(xintercept=as.numeric(black_friday), linetype="dashed", color = "black") +
  #annotate("text", x=black_friday, y=-75, label="Black friday ", hjust = "right")
  
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
our_theme <- theme_minimal() + 
  theme(text=element_text(family="ETBembo"))

colors <- c("#AF707070", "#7070AF70", "#AF70AF70", "#AFAF7070", "#70AFAF70", "#00AF00")

plot_mob_cat <- function (Mob_Type)  
{
  ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == Mob_Type),
         aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
             color = country)) + 
    geom_line() +
    theme_light() +
    theme(text=element_text(family="ETBembo")) +
    ggtitle(paste("Google mobility report - ", Mob_Type)) +
    ylab("Change from baseline") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_color_manual(name = "Country", values = colors)+
    geom_hline(yintercept=0, linetype="dashed") 
}


ggplotly(plot_mob_cat("Residential_change"))
plot_mob_cat("Residential_change")
ggplotly(plot_mob_cat("Retail & recreation"))
ggplotly(plot_mob_cat("Workplaces"))
ggplotly(plot_mob_cat("Parks"))
ggplotly(plot_mob_cat("Transit stations"))

plot_country <- function (Country)  
{
  g <- ggplot(All_Mobility_Data_long %>% 
                filter(country == Country)%>%
                filter(Mobility_Type != "Parks") , 
              aes(x = Date, 
                  y = rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"), 
                  color = Mobility_Type)) + 
    geom_line() + 
    theme_light() +
    theme(text=element_text(family="ETBembo")) +
    ggtitle(paste("Google mobility report - ", Country)) +
    ylab("Change from baseline") 
}

countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czechia")
ggplotly(plot_country("Sweden"))
ggplotly(plot_country("Germany"))
ggplotly(plot_country("Denmark"))
ggplotly(plot_country("Belgium"))
ggplotly(plot_country("France"))
ggplotly(plot_country("Czechia"))

plot_mob_facet <- function (Mob_Type)  
{
  g<-ggplot(transform(Subset_Mobility_Data_long,country=factor(country,levels=countries)) %>% 
              filter(Mobility_Type == Mob_Type),
            aes(Date, rollmean(Percent_Change, 7, na.pad=TRUE, align = "right"),
                color = country)) + 
    geom_line() +
    our_theme +
    ggtitle(paste("Google mobility report - ", Mob_Type)) +
    ylab("Change from baseline") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    geom_hline(yintercept=0, linetype="dashed")  +
    scale_x_date(date_breaks = "1 month", limits = as.Date(c('2020-10-01','2020-12-02'))) +
    facet_wrap(~ country)
}

ggplotly(plot_mob_facet("Residential_change"))  %>% style(hoverinfo = "none")
ggplotly(plot_mob_facet("Retail & recreation"))
ggplotly(plot_mob_facet("Workplaces"))
ggplotly(plot_mob_facet("Parks"))
ggplotly(plot_mob_facet("Transit stations"))
