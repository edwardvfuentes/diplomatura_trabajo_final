library(tidyverse)

source("./input/load_source_data.R")


## Por simplicidad, residential hukou será igual a urban

famecon2010 <- clasificar_familia(
  famecon2010_raw,
  famconf2010_raw,
  variable_urban = "urban",
  variable_hukou = "tb601_a_p"
  )

famecon2012 <- clasificar_familia(
  famecon2012_raw,
  famconf2012_raw,
  variable_urban = "urban12",
  variable_hukou = "qa301_a12_p"
)


famecon2014 <- clasificar_familia(
  famecon2014_raw,
  famconf2014_raw,
  variable_urban = "urban14.x",
  variable_hukou = "qa301_a14_p"
)


famecon2016 <- clasificar_familia(
  famecon2016_raw,
  famconf2016_raw,
  variable_urban = "urban16",
  variable_hukou = "hukou_a16_p"
)

famecon2018 <- clasificar_familia(
  famecon2018_raw,
  famconf2018_raw,
  variable_urban = "urban18",
  variable_hukou = "hukou_a18_p"
)

famecon2020 <- clasificar_familia(
  famecon2020_raw,
  famconf2020_raw,
  variable_urban = "urban20",
  variable_hukou = "hukou_a20_p"
)

famecon2022 <- clasificar_familia(
  famecon2022_raw,
  famconf2022_raw,
  variable_urban = "urban22",
  variable_hukou = "hukou_a22_p"
)

# Elimina los _raw para eliminar memoria
rm(list = ls(pattern = "_raw$"))
