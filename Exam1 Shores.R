

library(tidyverse)

Oh_Vaccine_Percentage <- read_csv("oh_vacc_percentage.csv")

Oh_Vaccine_Long <- Oh_Vaccine_Percentage %>% 
  pivot_longer(cols = c(primary_booster, updated_booster),
               names_to = "Booster",
               values_to = "Percentage")

ggplot(data = Oh_Vaccine_Long, 
       mapping = aes(x= Booster, y= Percentage, fill = Booster)) +
  geom_bar(stat = "identity") +
  labs(title = "Covid-19 Vaccine Percentages For Ohio",
       x = "Booster",
       y = "Percentage",
       fill= "Booster Type")


library(tidyquant)

Oh_covid_data <- read_csv("oh_covid_data.csv")


Total_case_count_day <- Oh_covid_data %>% group_by(onset_date) %>% 
  summarize(cases = sum(case_count))


Total_case_count_day <- Total_case_count_day %>%
  mutate(`Moving average- cases` = rollmean(cases, k = 21, align = "right", fill =NA))

ggplot(data=Total_case_count_day,aes(x=onset_date,y=cases)) +
  geom_bar(stat = "identity") +
  labs(title = "Ohio Covid-19 Cases per Day")+
  ylab("Number of Cases")+
  xlab("Onset Date")+
  geom_line(aes(x= onset_date, y = `Moving average- cases`), 
            color = "blue", size = 0.5 ) +
  scale_x_date(date_breaks = "4 month",date_labels = "%b-%y")


library(tidyverse)
library(sf)
library(RColorBrewer)
library(classInt)
library(gridExtra)

Franklin_2020<- st_read("Franklin_2020.geojson")

SES_intervals <- classIntervals(Franklin_2020$RPL_THEME1, n = 5, style = "jenks")

Franklin_2020 <- Franklin_2020 %>%
  mutate(SES_class = cut(RPL_THEME1, SES_intervals$brks, include.lowest = TRUE))


map1 <- generate_choropleth_map(sf_object = Franklin_2020, 
                                column_name_as_string = "RPL_THEME1",
                                classification_scheme = "quantile",
                                number_of_classes = 5,
                                color_palette = "YlOrRd",
                                plot_title = "Socioeconomic Status")

map1

#map2

Household_intervals <- classIntervals(Franklin_2020$RPL_THEME2, n = 5, style = "jenks")

Franklin_2020 <- Franklin_2020 %>%
  mutate(Household_class = cut(RPL_THEME2, Household_intervals$brks, include.lowest = TRUE))

map2 <- generate_choropleth_map(sf_object = Franklin_2020, 
                                column_name_as_string = "RPL_THEME2",
                                classification_scheme = "quantile",
                                number_of_classes = 5,
                                color_palette = "YlOrRd",
                                plot_title = "Household Charecteristics")

map2

#map3

RaceEth_intervals <- classIntervals(Franklin_2020$RPL_THEME3, n = 5, style = "jenks")

Franklin_2020 <- Franklin_2020 %>%
  mutate(RaceEth_class = cut(RPL_THEME3, RaceEth_intervals$brks, include.lowest = TRUE))

map3 <- generate_choropleth_map(sf_object = Franklin_2020, 
                                column_name_as_string = "RPL_THEME3",
                                classification_scheme = "quantile",
                                number_of_classes = 5,
                                color_palette = "YlOrRd",
                                plot_title = "Racial and Ethnic Minority Status")
map3

#map4

HouseTrans_intervals <- classIntervals(Franklin_2020$RPL_THEME4, n = 5, style = "jenks")

Franklin_2020 <- Franklin_2020 %>%
  mutate(HouseTrans_class = cut(RPL_THEME4, HouseTrans_intervals$brks, include.lowest = TRUE))

map4 <- generate_choropleth_map(sf_object = Franklin_2020, 
                                column_name_as_string = "RPL_THEME4",
                                classification_scheme = "quantile",
                                number_of_classes = 5,
                                color_palette = "YlOrRd",
                                plot_title = "Housing Type and Transportation")

map4
grid.arrange(map1, map2, map3, map4)





