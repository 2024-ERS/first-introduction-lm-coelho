#restore library and load required packages
renv::restore()
library(tidyverse)
library(RColorBrewer)

#clean the environment
rm(list = ls())

# read endobenthos data 
FactEndobenthosCores <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTBzZSXWMgUTgtLWM-lHIcsgXMmTKSzuXHSVXe_FOp4l3h_GND7jfHxeo_JvfMAVPN_0O73Y32wrFA5/pub?gid=1901178818&single=true&output=csv")

#data set with diversity of 2023 and 2024
data23_24 <- FactEndobenthosCores |>
  filter(year == 2023 | year == 2024) |>
  group_by(TransectPoint_ID,year,depth1,species_ID) |>
  summarise(count=n())


#apply colorpalette
palette1 <- brewer.pal(12, "Set3")
palette2 <- brewer.pal(8, "Paired")
#combine palettes
combined_palette <- c(palette1, palette2)


#data set of 5 depth diversity
data_5 <- FactEndobenthosCores |>
  filter(year == 2023 | year == 2024, depth1==5) |>
  filter(!(species_ID=="Cerastoderma_edule" | species_ID=="Macoma_balthica")) |>
  group_by(TransectPoint_ID,year,species_ID) |>
  summarise(count=n())


data_5 |>
  ggplot(aes(x = TransectPoint_ID, y = count, fill = species_ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = combined_palette) +
  labs(title = "Number of individuals per transect point in depth 5",
       x = "Transect point ID",
       y = "Number of individuals") +
  facet_wrap(~year) +
  theme(text = element_text(size=18))


#data set of 25 depth diversity
data_25 <- FactEndobenthosCores |>
  filter(year == 2023 | year == 2024, depth1==25) |>
  filter(!(species_ID=="Cerastoderma_edule" | species_ID=="Macoma_balthica")) |>
  group_by(TransectPoint_ID,year,species_ID) |>
  summarise(count=n())


data_25 |>
  ggplot(aes(x = TransectPoint_ID, y = count, fill = species_ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = combined_palette) +
  labs(title = "Number of individuals per transect point in depth 25",
       x = "Transect point ID",
       y = "Number of individuals") +
  facet_wrap(~year) +
  theme(text = element_text(size=18))






data23_24 |>
  filter(depth1 == 25) |>
  ggplot(aes(x = TransectPoint_ID, y = count, fill = species_ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = combined_palette) +
  labs(title = "Number of individuals per transect point in 2024",
       x = "Transect point ID",
       y = "Number of individuals") +
  #facet_wrap(~depth1) +
  theme(text = element_text(size=18))






#count the number of individuals per transect point and species
count_data <- FactEndobenthosCores %>%
  group_by(TransectPoint_ID, year, depth1, species_ID) %>%
  summarise(count = n()) %>%
  ungroup()
print(count_data)



graph <- count_data |>
  filter(year == 2024) |>
  ggplot(aes(x = TransectPoint_ID, y = count, fill = species_ID)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = combined_palette) +
  labs(title = "Number of individuals per transect point in 2024",
       x = "Transect point ID",
       y = "Number of individuals") +
  facet_wrap(~depth1) +
  theme(text = element_text(size=18))
print(graph)
