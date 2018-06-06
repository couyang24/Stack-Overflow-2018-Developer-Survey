library(readxl)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(plotly)
library(stringr)
library(viridis)
library(gridExtra)
library(tidyverse)
library(highcharter)
library(plotly)
library(dygraphs)
library(lubridate)
library("viridisLite")
library(countrycode)
library(leaflet)
library(xts)
library(htmltools)



data <- read_csv("input/survey_results_public.csv")
data[is.na(data)] <- ""
data[data$Country=="United States",]$Country <- "United States of America"
data[data$Country=="Bolivia",]$Country <- "Bolivia (Plurinational State of)"
data[data$Country=="Venezuela, Bolivarian Republic of...",]$Country <- "Venezuela, Bolivarian Republic of"
data[data$Country=="Iran, Islamic Republic of...",]$Country <- "Iran (Islamic Republic of)"
data[data$Country=="United Kingdom",]$Country <- "United Kingdom of Great Britain and Northern Ireland"


countries <- data %>% count(Country)
names(countries) <- c("country.code", "total")

data(worldgeojson, package = "highcharter")
countries$iso3 <- countrycode(countries$country.code, 'country.name', 'iso3c')
countries$country_code <- countrycode(countries$country.code, 'country.name', 'iso3n')





library(wpp2017)

data('pop')
rm(popF, popFT, popM, popMT)

pop %>% head()
countries %>% head()

new_country <- countries %>% 
  left_join(pop, by = 'country_code') %>% 
  select(country.code, country_code, iso3, total, pop = `2015`) %>% 
  mutate(ratio = round(total/pop*1000,3))











dshmstops <- data.frame(q = c(0, exp(1:10)/exp(10)),
                        c = substring(viridis(10 + 1, option = "D"), 0, 7)) %>%  
  list_parse2()



highchart() %>% 
  hc_add_series_map(worldgeojson, new_country, value = "total", joinBy = "iso3", colorByPoint =  1) %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>%  
  hc_mapNavigation(enabled = TRUE) %>%
  # hc_tooltip(useHTML = TRUE, headerFormat = "",
  #            pointFormat = "Country") %>% 
  hc_add_theme(hc_theme_chalk()) %>% 
  hc_title(text = "Where are Stack Overflow Users?")  %>%
  hc_credits(enabled = TRUE, text = "Sources: Stack Overflow 2018 Developer Survey", style = list(fontSize = "10px")) 






















a <- highchart() %>% 
  hc_add_series_map(worldgeojson, new_country, value = "total", joinBy = "iso3", colorByPoint =  1) %>% 
  hc_colorAxis(stops = dshmstops) %>%
  hc_legend(enabled = TRUE) %>%  
  hc_mapNavigation(enabled = TRUE) %>%
  # hc_tooltip(useHTML = TRUE, headerFormat = "",
  #            pointFormat = "Country") %>% 
  hc_add_theme(hc_theme_chalk()) %>% 
  hc_title(text = "Number of Stack Overflow Users by Country")  %>%
  hc_credits(enabled = TRUE, text = "Sources: Stack Overflow 2018 Developer Survey", style = list(fontSize = "10px")) 


b <- highchart() %>% 
  hc_add_series_map(worldgeojson, new_country, value = "ratio", joinBy = "iso3", colorByPoint =  1) %>% 
  hc_colorAxis(stops = dshmstops) %>%
  hc_legend(enabled = TRUE) %>%  
  hc_mapNavigation(enabled = TRUE) %>%
  # hc_tooltip(useHTML = TRUE, headerFormat = "",
  #            pointFormat = "Country") %>% 
  hc_add_theme(hc_theme_chalk()) %>% 
  hc_title(text = "Number of Stack Overflow Users per million population by Country")  %>%
  hc_credits(enabled = TRUE, text = "Sources: Stack Overflow 2018 Developer Survey", style = list(fontSize = "10px")) 

lst <- list(a, b)

hw_grid(lst, rowheight = 350)
