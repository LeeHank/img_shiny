library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)

my_img_func = function(path, size = "50px"){
  tags$img(src = path, width = size, height = size, style = "margin: 0.5rem 0.5rem")
}


parent_cluster = 0:4
child_cluster = 0:3
df = expand.grid(parent_cluster = parent_cluster, 
                 child_cluster = child_cluster)

df = bind_rows(df, df, df, df, df) %>%
  mutate(img = list.files("www"),
         label = rbinom(100, 1, 0.2),
         x1 = rnorm(100),
         x2 = rnorm(100))