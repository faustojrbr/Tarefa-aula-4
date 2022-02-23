
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
pacman::p_load(terra, spData)


meu = rast("brasil_coverage_2020.tif")
plot(meu)

cit = read_citicipality(year =2020)
citymaravilha = cit %>% 
  filter(abbrev_state == "RJ") 


mil = milop(meu, citymaravilha)
mas = mask(mil, vect(citymaravilha))
extra = extratract(mas, vect(citymaravilha))

plot(mil)
plot(mas)


cover <- extra %>%
  group_by(ID) %>%
  summarise(cover = n())


coverv <- extra %>%
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  summarise(cover_v = n())


coverrio <- merge(cover, coverv, by=c("ID"), all.x=TRUE)

coverrio <- coverrio %>%
  mutate(p_v = cover_v/cover)


citymaravilha <- citymaravilha %>%
  mutate(ID = c(1:92), .after = code_citi) %>%
  left_join(coverrio, by = "ID")


plot_rio_cover <- citymaravilha %>%
  ggplot() +
  geom_sf(aes(fill = p_v), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Porcentagem") +
  labs(title = "Porcentagem de cover Vegetal", subtitle = "Estado do Rio de Janeiro")
plot_rio_cover


RJ_vegetal <- citymaravilha %>%
  subset(select = c(name_citi, p_v))