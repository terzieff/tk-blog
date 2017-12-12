library(rwunderground)
library(rvest)
library(tibble)
library(dplyr)
library(magrittr)
library(stringr)
library(readr)
library(purrr)
library(weathermetrics)
library(xkcd)
library(ggplot2)
library(extrafont)
library(ggrepel)
library(stringi)

rwunderground::set_api_key("450a973f0b99e2f0")

############################
## PART 1
############################

cities_wiki <-
  read_html("https://pl.wikipedia.org/wiki/Miasta_w_Polsce_(statystyki)")
cities_wiki %<>% html_nodes(".sortable") %>% html_table()

cities_tbl <- as_tibble(cities_wiki[[1]]) %>%
  select(city = Miasto,
         region = Województwo,
         population = `Liczba ludności (01.01.2017)`) %>%
  mutate(population = as.integer(str_replace_all(population, "\\s", "")))

top_cities_tbl <-
  cities_tbl %>%
  top_n(25, population) %>%
  select(city)

read_imgw_s_m_t <- function(file) {
  output <- read_csv(
    file,
    col_types = cols_only(
      X1 = 'c',
      X2 = 'c',
      X3 = 'i',
      X4 = 'i',
      X9 = 'd',
      X13 = 'd'
    ),
    col_names = F,
    locale = locale(encoding = "windows-1250")
  )
  colnames(output) <-
    c("id", "city", "year", "month", "temp", "relhum")
  return(output)
}

# Get the files downloaded from https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/miesieczne/synop/
# (files <- list.files(path = "weather/data", pattern = "s_m_t", full.names = T))

pl_data <- bind_rows(map(files, read_imgw_s_m_t))
pl_data %<>% filter(!(relhum == 0.0 & month %in% c(6, 7, 8)))

pl_stations <- pl_data %>% distinct(id, city)

top_cities_tbl %<>% mutate(city = toupper(city))

top_cities_stations <-
  left_join(top_cities_tbl, pl_stations)

# (dash_names <- str_subset(pl_data$city, "-") %>% unique())

pl_data %<>% mutate(city = str_extract(city, "[^-]*"))

pl_data %<>% mutate(city = if_else(city == "BIELSKO", "BIELSKO-BIAŁA", city))

pl_stations <- pl_data %>% distinct(id, city)
top_cities_stations <-
  left_join(top_cities_tbl, pl_stations)
pl_cities <-
  top_cities_stations %>% filter(!is.na(id)) %>% select(city)

pl_data %<>% filter(city %in% pl_cities$city)

pl_winter_data <- pl_data %>%
  filter(month %in% c(12, 1, 2)) %>%
  group_by(city) %>%
  summarise(avg_temp = mean(temp))

pl_summer_data <- pl_data %>%
  filter(month %in% c(6, 7, 8)) %>%
  mutate(
    heat_index = heat.index(
      t = temp,
      rh = relhum,
      temperature.metric = "celsius",
      output.metric = "celsius",
      round = 1
    )
  ) %>%
  group_by(city) %>%
  summarise(avg_heat_index = mean(heat_index))

pl_data_final <- left_join(pl_winter_data, pl_summer_data)

pl_data_final %<>% mutate(city = str_to_title(stri_trans_general(city, "latin-ascii")))
write_delim(pl_data_final, "pl_final.csv", delim = ";")

xrange <- range(pl_data_final$avg_heat_index)
yrange <- range(pl_data_final$avg_temp)

set.seed(42)

ggplot(pl_data_final,
       aes(avg_heat_index, avg_temp)) +
  geom_point() +
  geom_text_repel(aes(label = city),
                  family = "xkcd", 
                  max.iter = 50000) +
  ggtitle("Where to live in Poland based on your temperature preferences",
          subtitle = "Data from IMGW, 2014-2016") +
  xlab("Heat index")+
  ylab("Winter temperature in Celsius degrees") +
  xkcdaxis(xrange = xrange,
           yrange = yrange)+
  theme_xkcd() +
  theme(text = element_text(size = 16, family = "xkcd"))

############################
## PART 2
############################

moral_index_countries <- 
  read_html("https://en.wikipedia.org/wiki/World_Index_of_Moral_Freedom") %>% 
  html_nodes(".sortable") %>% 
  html_table()

moral_tbl <- as_tibble(moral_index_countries[[1]]) %>% 
  select(country = COUNTRY, index = `WIMF 2016`) %>%
  mutate(index = as.double(str_replace(index, ",", ".")))

press_index_countries <- 
  read_html("https://en.wikipedia.org/wiki/Press_Freedom_Index") %>% 
  html_nodes(".sortable") %>% 
  html_table()

press_tbl <- as_tibble(press_index_countries[[1]]) %>% 
  select(1:2) %>%
  mutate(index = str_sub(`2017[4]`, 6)) %>%
  select(country = Country, index) %>%
  mutate(country = str_replace(country, "\\[.{1}\\]", ""),
         index = as.double(str_replace(index, "\\s", "")))
  
freedom_tbl <- full_join(moral_tbl, press_tbl, by = "country") %>%
  filter(index.x > 60 & index.y < 25)

###FUNCS
get_avg_temp <- function(lat_long, start_date = "1201", end_date = "0228") {
  
  tryCatch(
  {
    tmp_df <- planner(set_location(lat_long = lat_long), 
                    start_date = start_date, 
                    end_date = end_date,
                    message = F)
  Sys.sleep(7)
  
  avg_temp <- (tmp_df$temp_high_avg + tmp_df$temp_low_avg) / 2
  
  return(avg_temp)
  }
  ,
  error = function(e) NA
  )
}

get_avg_dewpt <- function(lat_long, start_date = "0601", end_date = "0831") {
  
  tryCatch(
    {
      tmp_df <- planner(set_location(lat_long = lat_long), 
                        start_date = start_date, 
                        end_date = end_date,
                        message = F)
      Sys.sleep(7)
      
      dewpt_temp <- (tmp_df$dewpt_high_avg + tmp_df$dewpt_low_avg) / 2
      
      return(dewpt_temp)
    }
    ,
    error = function(e) NA
  )
}

get_humidex <- function(temp, dewpoint) {
  temp + 0.5555 * (6.11 * exp(5417.7530 * (1/273.16 - 1/(273.15 + dewpoint))) - 10)
}
#######


#######################
# CITIES 
#######################

# Tried wiki, but it's simpler that way:
worldcities_df <- read_csv("static/data/simplemaps-worldcities-basic.csv", 
                                         col_types = cols(iso2 = col_skip(), iso3 = col_skip(), 
                                                          province = col_skip()))

world_cities_tbl <- worldcities_df %>% 
  mutate(country = if_else(country == "United States of America", "United States", country)) %>%
  filter(country %in% freedom_tbl$country | (city == "Lódz" | city == "Warsaw")) %>% 
  group_by(country) %>% 
  top_n(2, pop) %>%
  ungroup() %>%
  select(city, lat, lng) %>%
  mutate(lat_long = paste(lat, lng, sep = ","))
  # filter(!city %in% c("Berlin", "Tartu")) %>%

world_cities_tbl %<>%
  rowwise() %>% 
  mutate(wint_avg_temp = fahrenheit.to.celsius(if_else(lat > 0, 
                                 get_avg_temp(lat_long, "1201", "0228"), 
                                 get_avg_temp(lat_long, "0601", "0831"))))

world_cities_tbl %<>% 
  rowwise() %>% 
  mutate(summer_avg_temp = fahrenheit.to.celsius(if_else(lat < 0, 
                                 get_avg_temp(lat_long, "1201", "0228"), 
                                 get_avg_temp(lat_long, "0601", "0831"))),
         summer_avg_dewpt = fahrenheit.to.celsius(if_else(lat < 0, 
                                   get_avg_dewpt(lat_long, "1201", "0228"), 
                                   get_avg_dewpt(lat_long, "0601", "0831")))) %>%
  mutate(humidex = get_humidex(summer_avg_temp, summer_avg_dewpt))

# write_delim(world_cities_tbl, "results.csv", delim = ";")

################################
# (At this point I decided to add colors to the plot)
# This should be done earlier, but now I don't want to download weather data once more
# so it's done fast & dirty, sorry
################################
# world_cities_tbl <- read_delim("static/data/world_final.csv",
#                       ";", escape_double = FALSE, trim_ws = TRUE)
worldcities_df <- read_csv("../../static/data/simplemaps-worldcities-basic.csv")
worldcities_df %<>% select(city, country)
world_cities_tbl <- left_join(world_cities_tbl, worldcities_df, by = "city") %>% 
  slice(-c(2, 4, 40, 46))

freedom_tbl <- freedom_tbl %>% mutate(index.y = 100 - index.y,
                                      index = index.x + index.y) %>%
  select(country, index)

world_cities_tbl %<>% mutate(country = if_else(country == "United States of America", 
                                               "United States", 
                                               country))

world_cities_tbl <- left_join(world_cities_tbl, freedom_tbl, by = "country")

world_cities_tbl %<>% 
  filter(!is.na(humidex)) %>% 
  mutate(city = stri_trans_general(city, "latin-ascii"))

################################
# FINAL WORLD PLOT
################################
xrange <- range(world_cities_tbl$humidex)
yrange <- range(world_cities_tbl$wint_avg_temp)

set.seed(42)

ggplot(world_cities_tbl,
       aes(humidex, wint_avg_temp, color = index)) +
  geom_point() +
  geom_text_repel(aes(label = city),
                  family = "xkcd", 
                  max.iter = 50000) +
  ggtitle("Where to live based on your temperature preferences",
          subtitle = "Data from Weather Underground") +
  xlab("Summer humidex") +
  ylab("Winter temperature in Celsius degrees") +
  theme_xkcd() +
  theme(text = element_text(size = 16, family = "xkcd"))