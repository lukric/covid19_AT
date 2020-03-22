
# this script scrapes the data from https://info.gesundheitsministerium.at/
# the official covid19 dashboard of the Austrian ministry of health.

library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(here)

path <- here()

prefix <- "https://info.gesundheitsministerium.at/data/"
suffix <- ".js"
my_link0 <- c("Trend",
              "Geschlechtsverteilung",
              "Altersverteilung",
              "Bundesland",
              "Bezirke") # "https://info.gesundheitsministerium.at/data/SimpleData.js"

my_link <- paste0(prefix, my_link0, suffix)


covid_at <- function(my_link) {
  jscode <- readLines(my_link, warn = FALSE, encoding = "UTF-8")
  
  temp <- jscode %>% 
    # gsub('{', "", ., fixed = TRUE) %>% 
    # gsub('', "", ., fixed = TRUE) %>% 
    gsub('},{', '\",\"', ., fixed = TRUE) %>% 
    gsub('[{\"', "", ., fixed = TRUE) %>% 
    gsub('}];', "", ., fixed = TRUE) %>% 
    gsub('var.*= ', "", .) %>% 
    strsplit(., "\",\"") %>%
    unlist() %>% 
    strsplit(., "\":") %>% 
    unlist() %>% 
    gsub('\"', '', ., fixed = TRUE) %>% 
    gsub('"', '', ., fixed = TRUE)
  
  test <- matrix(temp, ncol = 2, byrow = TRUE) %>% 
    tibble::as_tibble()
  
  col1 <- test %>% 
    dplyr::filter(V1 == "label") %>% 
    dplyr::pull(V2)
  
  col2 <- test %>% 
    dplyr::filter(V1 == "y") %>% 
    dplyr::pull(V2)
  
  df <- data.frame(y = col1, value = as.numeric(col2), stringsAsFactors = FALSE)
  df
}
temp <- lapply(my_link, covid_at)
names(temp) <- my_link0

# basic stuff
jscode <- readLines(paste0(prefix, "SimpleData", suffix), warn = FALSE, encoding = "UTF-8") %>% 
  gsub('var ', "", ., fixed = TRUE) %>%
  gsub('\"', "", ., fixed = TRUE) %>% 
  gsub(';', "", ., fixed = TRUE) %>% 
  strsplit(., " = ") %>% 
  unlist()
simple0 <- matrix(jscode, nrow = 2, byrow = FALSE)
colnames(simple0) <- simple0[1, ]

simple0 <- tibble::as_tibble(simple0)
simple <- simple0[-1, ] %>% 
  dplyr::mutate_all(readr::parse_guess) %>% 
  dplyr::mutate(LetzteAktualisierung = as.POSIXct(LetzteAktualisierung, format = "%d.%m.%Y %H:%M.%S"))


# Trend
temp$Trend <- temp$Trend %>% 
  dplyr::mutate(y = as.Date(paste0(y, ".2020"), format = "%d.%m.%Y"),
                daily_cases = c(NA, diff(value))) %>% 
  dplyr::mutate(daily_cases = dplyr::if_else(is.na(daily_cases), value, daily_cases)) %>% 
  dplyr::rename("cases" = "value")

# sex
temp$Geschlechtsverteilung <- temp$Geschlechtsverteilung %>% 
  dplyr::mutate("percent" = value / 100) %>% 
  dplyr::mutate(cases_calculated = round((simple %>% pull(Erkrankungen))[1] * percent)) %>% 
  dplyr::select(-value)

temp$SimpleData <- simple

# save
daytime <- (simple %>% pull(LetzteAktualisierung))[1]

main_folder <- "data"
day_folder <- format(daytime, format = "%Y-%m-%d")
time_folder <- format(daytime, format = "%H_%M_%S")

my_path <- file.path(path, main_folder, day_folder, time_folder)
dir.create(my_path, recursive = TRUE, showWarnings = FALSE)

my_save <- function(x) {
  write.csv2(temp[[x]], file = file.path(my_path, paste0(x, ".csv")), row.names = FALSE)
}
garbage <- sapply(names(temp), my_save)

