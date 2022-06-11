# ****************************************************************
# Visualization of natural population rate in the world
# with density plots, box plots, and swarm plots in a single graph
# 
# By Thuc Dao
# ****************************************************************


if (!require("dplyr")) {
  install.packages("dplyr")
}

if (!require("devtools")) {
  install.packages("devtools")
}

# flags in round shape
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
}

# flags in rectangular form 
# if (!require("ggflags")) {
#   devtools::install_github("ellisp/ggflags")
# }

if (!require("countrycode")) {
  install.packages("countrycode")
}

library(dplyr) # to use the pipe operator %>%
library(tidyverse)
library(readxl)
library(ggdist)
library(ggbeeswarm)
library(ggtext)
library(ggflags)
library(countrycode)

# import data from the 'ESTIMATES' sheet and skip the first 16 rows
country_data <- read_excel('WPP2019_POP_F03_RATE_OF_NATURAL_INCREASE.xlsx', 
                            sheet = 'ESTIMATES', skip = 16) %>%
  
  # only select rows with 'Country/Area' value in the 'Type' column
  dplyr::filter(Type == 'Country/Area') %>% 
  
  # select columns 1, 3, 6, 8 to 21
  select(c(1, 3, 6, 8:21)) %>% 
  # now the table has 17 columns
  
  # increase the number of rows and decrease the number of columns
  # to 2 columns: 'Period', and 'NaturalRate'
  pivot_longer(4:17, names_to = 'Period', values_to = 'NaturalRate') %>% 
  
  # exclude following values in the 'Period' column
  # dplyr::filter(
  #   Period != '1950-1955', Period != '1955-1960', 
  #   Period != '1960-1965', Period != '1965-1970'
  # ) %>%
  
  # mark countries by continent and mark specific countries
  # mutate: add new variables and preserve existing ones
  mutate(
    NaturalRate = as.numeric(NaturalRate),
    isAfrica = ifelse(Index >= 27 & Index <= 88, T, F),
    isAsia = ifelse(Index >= 90 & Index <= 146, T, F),
    isLatAmCar = ifelse(Index >= 149 & Index <= 188, T, F),
    isOceania = ifelse(Index >= 190 & Index <= 206, T, F),
    isEurope = ifelse(Index >= 210 & Index <= 252, T, F),
    isNorAm = ifelse(Index >= 254 & Index <= 255, T, F),
    isVietnam = ifelse(Index == 146, T, F)
  )

# mark countries by max and min rate in each period
country_data <- country_data %>%
  group_by(Period) %>%
  mutate(
    maxPeriodRate = max(NaturalRate),
    minPeriodRate = min(NaturalRate)
  ) %>%
  mutate(isMaxMinPeriodRate = ifelse(NaturalRate == maxPeriodRate | NaturalRate == minPeriodRate, T, F)) %>%
  ungroup() # always ungroup after group_by to prevent future errors

# rename columns
names(country_data)[2] = 'Country'

# add new columns of two-letter country codes for countries of max and min rates 
# Note: The Channel Islands is not part of the UK, but ISO offers the code for it under "GB"
country_data <- country_data %>%
  mutate(CodeISO2C_Max = ifelse(NaturalRate == maxPeriodRate,
                                tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c',
                                                    custom_match = c('Channel Islands' = 'GB'))),
                                NA)) %>%
  mutate(CodeISO2C_Min = ifelse(NaturalRate == minPeriodRate,
                                tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c',
                                                    custom_match = c('Channel Islands' = 'GB'))),
                                NA)) %>%
  # add new columns of full country names for countries of max and min rates
  mutate(Country_Max = ifelse(NaturalRate == maxPeriodRate, 
                              ifelse(Country == "Dem. People's Republic of Korea", 'North Korea', Country), 
                              NA)) %>%
  mutate(Country_Min = ifelse(NaturalRate == minPeriodRate, 
                              ifelse(Country == "Dem. People's Republic of Korea", 'North Korea', Country), 
                              NA))

# add a column of two-letter country code for Vietnam
country_data <- country_data %>%
  mutate(CodeISO2C_Vietnam = ifelse(isVietnam == TRUE,
                                    tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c',
                                                        custom_match = c('Channel Islands' = 'GB'))),
                                    NA))

# add a column of two-letter country code for countries in Northern America
country_data <- country_data %>%
  mutate(CodeISO2C_NorAm = ifelse(isNorAm == TRUE, 
                                  tolower(countrycode(Country, origin = 'country.name', destination = 'iso2c',
                                                      custom_match = c('Channel Islands' = 'GB'))),
                                  NA))

# count the number of distinct periods
total_periods = nrow(unique(country_data[,'Period']))

# choose the range of colors for the density plots
column_colors <- colorRampPalette(c("#8ecae6", "#219ebc"))(total_periods)

# choose other colors
background_color = '#fffef7'
country_color = '#800080'
world_color = '#5DB6D3'

# create a function to make density plots, box plots, and swarm plots (strip plots) in the same graph
ComboGraph <- function(boolean_column){
  # create temporarily 3 columns: size_custom, color_custom, alpha_custom to plot points of the selected group
  country_data <- mutate(country_data, size_custom = ifelse(boolean_column == TRUE, 0.8, 0.5))
  country_data <- mutate(country_data, color_custom = ifelse(boolean_column == TRUE, 'highlight', Period))
  country_data <- mutate(country_data, alpha_custom = ifelse(boolean_column == TRUE, 0.9, 0.5))
  
  if(colnames(boolean_column) == 'isAfrica') {
    region_group = 'Africa'
  } else if(colnames(boolean_column) == 'isAsia') {
    region_group = 'Asia'
  } else if(colnames(boolean_column) == 'isLatAmCar') {
    region_group = 'Latin America and the Caribbean'
  } else if(colnames(boolean_column) == 'isOceania') {
    region_group = 'Oceania'
  } else if(colnames(boolean_column) == 'isEurope') {
    region_group = 'Europe'
  } else if(colnames(boolean_column) == 'isNorAm') {
    region_group = 'Northern America'
  } else if(colnames(boolean_column) == 'isMaxMinPeriodRate') {
    region_group = 'countries of max and min rates'
  } else if(colnames(boolean_column) == 'isVietnam') {
    region_group = 'Vietnam'
  }
  print(paste0('Generating the graph of ', region_group))
  
  ggplot(country_data, aes(x = Period, y = NaturalRate)) +
    geom_boxplot(fill = 'transparent', width =  0.4, color = 'orange',
                 outlier.shape = NA, alpha = 0.8, coef = 0) +
    geom_quasirandom(aes(color = color_custom, alpha = alpha_custom, size = size_custom), width = 0.20) +
    stat_halfeye(aes(fill = Period), color = NA, justification = -0.6, 
                 width = 0.4, .width = 0, alpha = 0.9) +
    geom_text(data = unique(country_data[, c('Period')]), 
              aes(y = 47, label = Period), color = column_colors, size = 2.7, family = 'Arial') +
    annotate('text', y = -16, x = 5.5, label = 'Cambodian genocide', 
             family = 'Arial', color = 'grey50', size = 3)+
    geom_curve(data = NULL, aes(x = 5.45, y = -17.5, xend = 5.9, yend = -21),
               arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
    scale_y_continuous(breaks=seq(-30, 45, 10)) +
    scale_color_manual(values = c(column_colors, country_color)) +
    scale_alpha_identity() +
    scale_size_identity() +
    scale_fill_manual(values = column_colors) +
    labs(title = paste0("Natural population rate in <span style='color:", country_color, "'>", region_group, 
                        "</span> and <span style='color:", world_color, "'>the rest of the world </span>"), 
         subtitle = 'Natural population rate = crude birth rate - crude death rate, per 1000 population',
         caption = "Made by Thuc Dao | Source: Rate of Natural Population Increase - Population Division, United Nations
                    \nhttps://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F03_RATE_OF_NATURAL_INCREASE.xlsx", 
         y = 'Rate of natural population increase, per 1000 population') +
    theme_void() +
    theme(
      text = element_text('Arial', color = 'grey50'),
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.margin = margin(0.8, 0.8, 0.8, 0.8, 'cm'),
      plot.title = element_markdown(face = 'bold', size = 16, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = 'grey60', margin = margin(b = 7.5)),
      plot.caption = element_text(colour = 'grey40', size = 8),
      legend.position = 'none', # remove all legends
      axis.line.y = element_line(colour = 'grey50'),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10), 
      axis.text.y = element_text(color = 'grey70', size = 9, margin = margin(r = 5)), 
      panel.grid.major.y = element_line(colour = 'grey80', linetype = 'dotted')
    )
  ggsave(paste0('Population rate in ', region_group,'.png'), width = 10, height = 6)
  print('Plotting is completed.')
}

# create a function to make density plots, box plots, and swarm plots (strip plots) in the same graph
ComboGraphWithFlagSingle <- function(boolean_column){
  # create temporarily 3 columns: size_custom, color_custom, alpha_custom to plot points of the selected group
  country_data <- mutate(country_data, size_custom = ifelse(boolean_column == TRUE, NA, 0.5))
  country_data <- mutate(country_data, color_custom = ifelse(boolean_column == TRUE, 'highlight', Period))
  country_data <- mutate(country_data, alpha_custom = ifelse(boolean_column == TRUE, 0.9, 0.5))
  
  if(colnames(boolean_column) == 'isVietnam') {
    region_group = 'Vietnam'
    code_col = country_data$CodeISO2C_Vietnam
  }
  print(paste0('Generating the graph of ', region_group))
  
  ggplot(country_data, aes(x = Period, y = NaturalRate)) +
    geom_boxplot(fill = 'transparent', width =  0.4, color = 'orange',
                 outlier.shape = NA, alpha = 0.8, coef = 0) +
    geom_quasirandom(aes(color = color_custom, alpha = alpha_custom, size = size_custom), 
                     width = 0.20, show.legend = FALSE) +
    stat_halfeye(aes(fill = Period), color = NA, justification = -0.6, 
                 width = 0.4, .width = 0, alpha = 0.9, show.legend = FALSE) +
    geom_text(data = unique(country_data[, c('Period')]), 
              aes(y = 47, label = Period), color = column_colors, size = 2.7, family = 'Arial') +
    geom_flag(aes(country = code_col), size = 4, 
              position = position_quasirandom(width = 0, varwidth = FALSE, bandwidth = 0.5, 
                                              nbins = NULL, method = "quasirandom", 
                                              groupOnX = NULL, dodge.width = 0)) +
    annotate('text', y = -16, x = 5.5, label = 'Cambodian genocide', 
             family = 'Arial', color = 'grey50', size = 3)+
    geom_curve(data = NULL, aes(x = 5.45, y = -17.5, xend = 5.9, yend = -21),
               arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
    scale_y_continuous(breaks=seq(-30, 45, 10)) +
    scale_color_manual(values = c(column_colors, country_color)) +
    scale_alpha_identity() +
    scale_size_identity() +
    scale_fill_manual(values = column_colors) +
    labs(title = paste0("Natural population rate in <span style='color:", country_color, "'>", region_group, 
                        "</span> and <span style='color:", world_color, "'>the rest of the world </span>"), 
         subtitle = 'Natural population rate = crude birth rate - crude death rate, per 1000 population',
         caption = "Made by Thuc Dao | Source: Rate of Natural Population Increase - Population Division, United Nations
                    \nhttps://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F03_RATE_OF_NATURAL_INCREASE.xlsx", 
         y = 'Rate of natural population increase, per 1000 population') +
    theme_void() +
    theme(
      text = element_text('Arial', color = 'grey50'),
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.margin = margin(0.8, 0.8, 0.8, 0.8, 'cm'),
      plot.title = element_markdown(face = 'bold', size = 16, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = 'grey60', margin = margin(b = 7.5)),
      plot.caption = element_text(colour = 'grey40', size = 8),
      # legend.position = c(0.5, 0.1),
      # legend.direction = 'horizontal',
      # legend.box = 'horizontal',
      legend.position = 'bottom',
      legend.title=element_blank(), # remove all legend titles
      axis.line.y = element_line(colour = 'grey50'),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10), 
      axis.text.y = element_text(color = 'grey70', size = 9, margin = margin(r = 5)),
      panel.grid.major.y = element_line(colour = 'grey80', linetype = 'dotted'),
      # panel.grid.minor.y = element_line(colour = 'grey80', linetype = 'dotted')
    )
  ggsave(paste0('Population rate in ', region_group,'.png'), width = 10, height = 6)
  print('Plotting is completed.')
}

# create a function to make density plots, box plots, and swarm plots (strip plots) in the same graph
ComboGraphWithFlagMultiple <- function(boolean_column){
  # create temporarily 3 columns: size_custom, color_custom, alpha_custom to plot points of the selected group
  country_data <- mutate(country_data, size_custom = ifelse(boolean_column == TRUE, 0.8, 0.5))
  country_data <- mutate(country_data, color_custom = ifelse(boolean_column == TRUE, 'highlight', Period))
  country_data <- mutate(country_data, alpha_custom = ifelse(boolean_column == TRUE, 0.9, 0.5))
  
  if(colnames(boolean_column) == 'isNorAm') {
    region_group = 'Northern America'
    code_col = country_data$CodeISO2C_NorAm
  }
  print(paste0('Generating the graph of ', region_group))
  
  ggplot(country_data, aes(x = Period, y = NaturalRate)) +
    geom_boxplot(fill = 'transparent', width =  0.4, color = 'orange',
                 outlier.shape = NA, alpha = 0.8, coef = 0) +
    geom_quasirandom(aes(color = color_custom, alpha = alpha_custom, size = size_custom), 
                     width = 0.20, show.legend = FALSE) +
    stat_halfeye(aes(fill = Period), color = NA, justification = -0.6, 
                 width = 0.4, .width = 0, alpha = 0.9, show.legend = FALSE) +
    geom_text(data = unique(country_data[, c('Period')]), 
              aes(y = 47, label = Period), color = column_colors, size = 2.7, family = 'Arial') +
    geom_flag(aes(country = code_col), size = 4, 
              position = position_quasirandom(width = 0.2, varwidth = FALSE, bandwidth = 0.5, 
                                              nbins = NULL, method = "quasirandom", 
                                              groupOnX = NULL, dodge.width = 0)) +
    annotate('text', y = -16, x = 5.5, label = 'Cambodian genocide', 
             family = 'Arial', color = 'grey50', size = 3)+
    geom_curve(data = NULL, aes(x = 5.45, y = -17.5, xend = 5.9, yend = -21),
               arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
    scale_y_continuous(breaks=seq(-30, 45, 10)) +
    scale_color_manual(values = c(column_colors, country_color)) +
    scale_alpha_identity() +
    scale_size_identity() +
    scale_fill_manual(values = column_colors) +
    labs(title = paste0("Natural population rate in <span style='color:", country_color, "'>", region_group, 
                        "</span> and <span style='color:", world_color, "'>the rest of the world </span>"), 
         subtitle = 'Natural population rate = crude birth rate - crude death rate, per 1000 population',
         caption = "Made by Thuc Dao | Source: Rate of Natural Population Increase - Population Division, United Nations
                    \nhttps://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F03_RATE_OF_NATURAL_INCREASE.xlsx", 
         y = 'Rate of natural population increase, per 1000 population') +
    theme_void() +
    theme(
      text = element_text('Arial', color = 'grey50'),
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.margin = margin(0.8, 0.8, 0.8, 0.8, 'cm'),
      plot.title = element_markdown(face = 'bold', size = 16, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = 'grey60', margin = margin(b = 7.5)),
      plot.caption = element_text(colour = 'grey40', size = 8),
      # legend.position = c(0.5, 0.1),
      # legend.direction = 'horizontal',
      # legend.box = 'horizontal',
      legend.position = 'bottom',
      legend.title=element_blank(), # remove all legend titles
      axis.line.y = element_line(colour = 'grey50'),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10), 
      axis.text.y = element_text(color = 'grey70', size = 9, margin = margin(r = 5)),
      panel.grid.major.y = element_line(colour = 'grey80', linetype = 'dotted'),
      # panel.grid.minor.y = element_line(colour = 'grey80', linetype = 'dotted')
    )
  ggsave(paste0('Population rate in ', region_group,'.png'), width = 10, height = 6)
  print('Plotting is completed.')
}

# create a function to make density plots, box plots, and swarm plots (strip plots) in the same graph
ComboGraphWithFlagMaxMin <- function(boolean_column){
  # create temporarily 3 columns: size_custom, color_custom, alpha_custom to plot points of the selected group
  country_data <- mutate(country_data, size_custom = ifelse(boolean_column == TRUE, 0.8, 0.5))
  country_data <- mutate(country_data, color_custom = ifelse(boolean_column == TRUE, 'highlight', Period))
  country_data <- mutate(country_data, alpha_custom = ifelse(boolean_column == TRUE, 0.9, 0.5))
  
  if(colnames(boolean_column) == 'isMaxMinPeriodRate') {
    region_group = 'countries of max and min rates'
    codeMax_col = country_data$CodeISO2C_Max
    codeMin_col = country_data$CodeISO2C_Min
    countryMax_col = country_data$Country_Max
    countryMin_col = country_data$Country_Min
  }
  print(paste0('Generating the graph of ', region_group))
  
  ggplot(country_data, aes(x = Period, y = NaturalRate)) +
    geom_boxplot(fill = 'transparent', width =  0.4, color = 'orange',
                 outlier.shape = NA, alpha = 0.8, coef = 0) +
    geom_quasirandom(aes(color = color_custom, alpha = alpha_custom, size = size_custom), 
                     width = 0.20, show.legend = FALSE) +
    stat_halfeye(aes(fill = Period), color = NA, justification = -0.6, 
                 width = 0.4, .width = 0, alpha = 0.9, show.legend = FALSE) +
    geom_text(data = unique(country_data[, c('Period')]), 
              aes(y = 47, label = Period), color = column_colors, size = 2.7, family = 'Arial') +
    geom_flag(aes(country = codeMax_col), size = 4, position = position_quasirandom()) +
    geom_text(aes(label = countryMax_col), vjust = -2, hjust = 0.5, 
              color = 'grey40', size = 2) +
    # geom_text(aes(label = toupper(codeMax_col)), vjust = -1, hjust = 0.5, 
    #           color = 'grey40', size = 2) +
    geom_flag(aes(country = codeMin_col), size = 4, position = position_quasirandom()) +
    geom_text(aes(label = countryMin_col), vjust = 3, hjust = 0.5, 
              color = 'grey40', size = 2) +
    # geom_text(aes(label = toupper(codeMin_col)), vjust = 2, hjust = 0.5, 
    #           color = 'grey40', size = 2) +
    annotate('text', y = -7, x = 2, label = 'Channel Islands\nnot part of the UK', 
             family = 'Arial', color = 'grey50', size = 3)+
    geom_curve(data = NULL, aes(x = 2, y = -4, xend = 2, yend = -0.5),
               arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
    annotate('text', y = -16, x = 5.5, label = 'Cambodian genocide', 
             family = 'Arial', color = 'grey50', size = 3)+
    geom_curve(data = NULL, aes(x = 5.45, y = -17.5, xend = 5.85, yend = -21),
               arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
    scale_y_continuous(breaks=seq(-30, 45, 10)) +
    scale_color_manual(values = c(column_colors, country_color)) +
    scale_alpha_identity() +
    scale_size_identity() +
    scale_fill_manual(values = column_colors) +
    labs(title = paste0("Natural population rate in <span style='color:", country_color, "'>", region_group, 
                        "</span> and <span style='color:", world_color, "'>the rest of the world </span>"), 
         subtitle = 'Natural population rate = crude birth rate - crude death rate, per 1000 population',
         caption = "Made by Thuc Dao | Source: Rate of Natural Population Increase - Population Division, United Nations
                    \nhttps://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F03_RATE_OF_NATURAL_INCREASE.xlsx", 
         y = 'Rate of natural population increase, per 1000 population') +
    theme_void() +
    theme(
      text = element_text('Arial', color = 'grey50'),
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.margin = margin(0.8, 0.8, 0.8, 0.8, 'cm'),
      plot.title = element_markdown(face = 'bold', size = 16, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = 'grey60', margin = margin(b = 7.5)),
      plot.caption = element_text(colour = 'grey40', size = 8),
      # legend.position = c(0.5, 0.1),
      # legend.direction = 'horizontal',
      # legend.box = 'horizontal',
      legend.position = 'bottom',
      legend.title=element_blank(), # remove all legend titles
      axis.line.y = element_line(colour = 'grey50'),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10), 
      axis.text.y = element_text(color = 'grey70', size = 9, margin = margin(r = 5)),
      panel.grid.major.y = element_line(colour = 'grey80', linetype = 'dotted'),
      # panel.grid.minor.y = element_line(colour = 'grey80', linetype = 'dotted')
    )
  ggsave(paste0('Population rate in ', region_group,'.png'), width = 10, height = 6)
  print('Plotting is completed.')
}

# make plots of column names starting with "is"
for (col_name in colnames(country_data[, grepl("is", colnames(country_data))])){
  ComboGraph(country_data[col_name])
}

# make plots with flags
ComboGraphWithFlagSingle(country_data['isVietnam'])
ComboGraphWithFlagMultiple(country_data['isNorAm'])
ComboGraphWithFlagMaxMin(country_data['isMaxMinPeriodRate'])
