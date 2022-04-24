library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
library(prompter)




# my_picker_input2 <- function(moduleId, inputId, label, multiple_choice, choices, selected, helper) {
#   
#   if(is.na(label)){
#     label = NULL
#   }
#   if(multiple_choice == "N"){
#     max_options = 1
#   }else{
#     max_options = NULL
#   }
#   
#   base = pickerInput(
#     inputId = NS(moduleId, inputId),
#     label = label,
#     choices = choices,
#     selected = selected,
#     options = list(
#       `max-options` = max_options
#     ),
#     multiple = TRUE,
#     width = "100%"
#   )
#   
#   if(!is.na(helper)){
#     # base %>%
#     #   shinyhelper::helper(type = "inline", 
#     #                       title = inputId, 
#     #                       content = helper)
#     if(str_length(helper)>200){
#       size = "large"
#     }else if(str_length(helper)>60){
#       size = "medium"
#     }else{
#       size = NULL
#     }
#     base %>%
#       add_prompt(
#         position = "top", 
#         message = helper,
#         size = size,
#         rounded = TRUE
#       )
#   }else{
#     base
#   }
# }


# parent_cluster = 0:4
# child_cluster = 0:3
# df = expand.grid(parent_cluster = parent_cluster,
#                  child_cluster = child_cluster)
# 
# df = bind_rows(df, df, df, df, df) %>%
#   mutate(img = setdiff(list.files("www"), "custom.css"),
#          label = rbinom(100, 1, 0.2),
#          x1 = rnorm(100),
#          x2 = rnorm(100))

# saveRDS(df, "data/df.rds")
# write.csv(df, "data/df.csv", row.names = FALSE)
df = readRDS("data/df.rds")
