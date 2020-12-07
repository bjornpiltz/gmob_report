library(tidyverse)
library(readr)
library(plotly)
library(zoo)

library(extrafont)
# this line is not very portable. You might need to remove the references to ETBembo below
font_import(paths="~/Library/Fonts/", pattern = "*et*", prompt = FALSE)

countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czech Republic", "Czechia", "Austria", "United Kingdom", "Slovakia")
colors <- c("#F8766D", "#D3920070", "#93AA0070", "#00BA3870", "#00C19F70", "#00B9E370", "#619CFF70", "#DB72FB70", "#FF61C370")

cols <- c("Country", "Date", "Retail & recreation",
          "Grocery & pharmacy",
          "Parks", 
          "Transit stations", 
          "Workplaces",
          "Residential")

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
  gather(Mobility_Type, Percent_Change, -c(Date, Country)) %>% 
  filter(Country %in% countries) 

# Name Czech Republic consistently
Subset_Mobility_Data_long$Country <- gsub("Czechia", "Czech Republic", Subset_Mobility_Data_long$Country)
Subset_Mobility_Data_long$Change <- rollmean(Subset_Mobility_Data_long$Percent_Change, 7, na.pad=TRUE, align = "right")
write_csv(Subset_Mobility_Data_long, "data/Subset_Mobility_Data_long.csv")

Subset_Mobility_Data_long <- read_csv("data/Subset_Mobility_Data_long.csv")
Subset_Mobility_Data_long <-transform(Subset_Mobility_Data_long, Country = factor(Country,levels=countries) ) 
                         
our_theme <- theme_minimal() + 
  theme(text=element_text(family="ETBembo-RomanLF"))

plot_mob_cat <- function (Mob_Type)  
{
  g <- ggplot(Subset_Mobility_Data_long %>% filter(Mobility_Type == Mob_Type),
         aes(Date, Change,
             color = Country)) + 
    geom_line() +
    our_theme +
    ggtitle(paste("Google mobility report - ", Mob_Type)) +
    ylab("Change from baseline") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_color_manual(name = "Country", values = colors) +
    geom_hline(yintercept = 0, linetype="dashed")
    ggsave(scale = 0.6, paste("img/", Mob_Type, ".png"), plot = g)
    return(g)
}

ggplotly(plot_mob_cat("Residential"))
ggplotly(plot_mob_cat("Retail & recreation"))
ggplotly(plot_mob_cat("Workplaces"))
ggplotly(plot_mob_cat("Transit stations"))
ggplotly(plot_mob_cat("Parks"))

plot_country <- function (cntry)  
{
  g <- ggplot(Subset_Mobility_Data_long %>% 
                filter(Country == cntry)%>%
                filter(Mobility_Type != "Parks") , 
              aes(x = Date, 
                  y = Change, 
                  color = Mobility_Type)) + 
    geom_line() + 
    our_theme +
    ggtitle(paste("Google mobility report - ", cntry)) +
    ylab("Change from baseline") 
  return(g) 
}

ggplotly(plot_country("Sweden"))
ggplotly(plot_country("Germany"))
ggplotly(plot_country("Denmark"))
ggplotly(plot_country("Belgium"))
ggplotly(plot_country("France"))
ggplotly(plot_country("Czech Republic"))

plot_mob_facet <- function (Mob_Type)  
{
  g<-ggplot(Subset_Mobility_Data_long %>% 
              filter(Mobility_Type == Mob_Type), aes(Date, Change, color = Country)) + 
    geom_line() +
    our_theme +
    theme(legend.title = element_blank(), legend.position = "none") +
    ggtitle(paste("Google mobility - ", Mob_Type)) +
    ylab("Change from baseline") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    geom_hline(yintercept=0, linetype="dashed")  +
    scale_x_date(date_breaks = "1 month", 
                 limits = as.Date(c('2020-10-01','2020-12-04')),
                 labels = date_format("%b")) +
    facet_wrap(~ Country)
    ggsave(scale = 0.6, paste("img/", Mob_Type, "_facet.png"), plot = g)
    return(g)
}

ggplotly(plot_mob_facet("Residential"))
ggplotly(plot_mob_facet("Retail & recreation"))
ggplotly(plot_mob_facet("Workplaces"))
ggplotly(plot_mob_facet("Transit stations"))
ggplotly(plot_mob_facet("Parks"))


g <- ggplot(Subset_Mobility_Data_long %>% 
              filter(Mobility_Type != "Parks") %>% 
              filter(Mobility_Type != "Residential"), 
            aes(x = Date, 
                y = Change, 
                color = Country)) + 
  geom_line() + 
  our_theme +
  ggtitle("Google mobility report") +
  geom_hline(yintercept=0, linetype="dashed")  +
  scale_color_manual(name = "Country", values = colors) +
  #  theme(legend.title = element_blank(), legend.position = "none") +
  ylab("Change from baseline") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~ Mobility_Type)
g

ggsave(scale = 0.6, "img/mobility_facet.png", plot = g)
ggplotly(g)
