ui = page_navbar(
  title = "Two-stage clustering",
  #bg = "#0062cc",
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "flatly"),
  inverse = FALSE,
  nav(
    title = "Result Exploration",
    fluidRow(
      column(width = 4,
             pickerInput(
               inputId = "parent_cluster",
               label = "parent_cluster",
               choices = unique(df$parent_cluster),
               inline = TRUE
             ),
             pickerInput(
               inputId = "child_cluster",
               label = "child_cluster",
               choices = unique(df$parent_cluster),
               inline = TRUE
             ),
             actionButton("clear", "clear"),
             plotlyOutput("plot1", 
                        height = 300
             )
      ),
      column(width = 8,
             uiOutput("comparison"))
      # column(width = 4,
      #        imageOutput("myImage")),
      # column(width = 4,
      #        imageOutput("myImage2"))
    ),
    hr(),
    fluidRow(
      actionButton("show1","show parent_cluster images"),
      actionButton("show2","show child_cluster images"),
    ),
    fluidRow(
      uiOutput("image_outputs")
    )
  ),
  nav("Performance Evaluation"),
  nav_spacer(),
  nav_menu(
    "Other links", align = "right",
    nav_item(
      tags$a(icon("r-project"), "RStudio", href = "https://rstudio.com", target = "_blank")
    )
  )
)