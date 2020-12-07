library(readr)
countries <- c("Sweden", "Germany", "Denmark", "Belgium", "France", "Czech Republic", "Czechia", "Austria", "United Kingdom", "Slovakia")

df_jhu <- read_csv("data/subset_jhu.csv")
df_jhu <- transform(df_jhu, Country=factor(Country,levels=countries))

Subset_Mobility_Data_long <- read_csv("data/Subset_Mobility_Data_long.csv")
Subset_Mobility_Data_long <-transform(Subset_Mobility_Data_long, Country = factor(Country,levels=countries) ) 

our_theme <- theme_minimal() + 
  theme(text=element_text(family="ETBembo-RomanLF"))

#Mob_Type <- "Retail & recreation"

plot_mob_cat_combined <- function (Mob_Type)  
{
  g <- ggplot(Subset_Mobility_Data_long %>% 
              filter(Mobility_Type == Mob_Type), aes(Date, Change, color = Country)) + 
    geom_line() + 
    geom_smooth(data = df_jhu, method = "auto", se = TRUE, span = 0.3,
                level = 0.999, aes(y = rate), size=.10, fullrange=TRUE) +
    our_theme +
    theme(legend.title = element_blank(), legend.position = "none") +
    ggtitle(paste("Google mobility - ", Mob_Type)) +
    ylab("Change in behaviour") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    geom_hline(yintercept=0, linetype="dashed")  +
    scale_x_date(date_breaks = "1 month", 
                 limits = as.Date(c('2020-10-01','2020-12-04')),
                 labels = date_format("%b")) +
    facet_wrap(~ Country)
    ggsave(scale = 0.6, paste("img/", Mob_Type, "_combined.png"), plot = g)
    return(g)
}
ggplotly(plot_mob_cat_combined("Residential"))
ggplotly(plot_mob_cat_combined("Retail & recreation"))
ggplotly(plot_mob_cat_combined("Workplaces"))
ggplotly(plot_mob_cat_combined("Transit stations"))
ggplotly(plot_mob_cat_combined("Parks"))


