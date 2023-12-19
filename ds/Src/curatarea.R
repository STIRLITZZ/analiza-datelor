library(readr)
library(tidyverse)
library(openintro)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gapminder)

meredes_luc<-read_csv('C:/Users/andro/OneDrive/Документы/merc.csv')

glimpse(meredes_luc)

print(meredes_luc$model)

meredes_luc <- meredes_luc %>%
  mutate(categorizat = case_when(`model`  == "A Class" ~ 'A Class',
                                 `model`  == "B Class" ~ 'B Class',
                                 `model`  == "C Class" ~ 'C Class',
                                 `model`  == "CL Class" ~ 'A Class',
                                 `model`  == "GLA Class" ~ 'A Class',
                                 `model`  == "CLA Class" ~ 'A Class',
                                 `model`  == "CLC Class" ~ 'C Class',
                                 `model`  == "CLS Class" ~ 'S Class',
                                 `model`  == "E Class" ~ 'E Class',
                                 `model`  == "G Class" ~ 'G Class',
                                 `model`  == "GL Class" ~ 'G Class',
                                 `model`  == "GLA Class" ~ 'G Class',
                                 `model`  == "GLB Class" ~ 'G Class',
                                 `model`  == "GLC Class" ~ 'G Class',
                                 `model`  == "GLE Class" ~ 'G Class',
                                 `model`  == "GLS Class" ~ 'G Class',
                                 `model`  == "M Class" ~ 'G Class',
                                 `model`  == "R Class" ~ 'G Class',
                                 `model`  == "S Class" ~ 'S Class',
                                 `model`  == "SL CLASS" ~ 'S Class',
                                 `model`  == "X-CLASS" ~ 'G Class',
                                 `model`  == "V Class" ~ 'V Class',
                                 `model`  == "SLK" ~ 'S Class',
                                 `model`  == "180" ~ 'S Class',
                                 `model`  == "200" ~ 'S Class',
                                 `model`  == "220" ~ 'S Class',
                                 `model`  == "230" ~ 'S Class',
                                 `model`  == "CLK" ~ 'C Class'))


meredes_luc <- meredes_luc %>%
  mutate(period = case_when(`year`  > 1995 &`year`  < 2000 ~ '1995-2000',
                            `year`  > 1999 & `year`  < 2005 ~ '2000-2005',
                            `year`  > 2004 & `year`  < 2010 ~ '2005-2010',
                            `year`  > 2009 & `year`  < 2015 ~ '2010-2015',
                            `year`  > 2014 ~ '2015-2020'))

meredes_luc <- subset(meredes_luc, engineSize > 0)

meredes_luc$l_per_100km <- 235.21 / meredes_luc$mpg
meredes_luc <- subset(meredes_luc, l_per_100km <= 200)

meredes_luc$distanta_km <- meredes_luc$mileage * 1.60934

# Adăugarea coloanei log_engineSize
meredes_luc <- meredes_luc %>% mutate(log_engineSize = log(engineSize))

# Adăugarea coloanei transmission_binara
meredes_luc <- meredes_luc %>% mutate(transmission_binara = as.numeric(transmission == "Automatic"))

# Adăugarea coloanei binar_fuelType
meredes_luc <- meredes_luc %>% mutate(binar_fuelType = as.numeric(fuelType == "Petrol"))

# Adăugarea coloanei categorizat_numeric
meredes_luc <- meredes_luc %>% mutate(categorizat_numeric = as.numeric(factor(categorizat, levels = unique(categorizat))))