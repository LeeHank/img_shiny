ui = page_navbar(
  title = "Two-stage clustering",
  #bg = "#0062cc",
  theme = bslib::bs_theme(version = 4,
                          bg = "#f4f6fa",
                          fg = "#000",
                          bootswatch = "flatly"),
  inverse = FALSE,
  nav(
    title = "Result Exploration",
    fluidRow(
      column(
        width = 12,
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
        )
      )
    ),
    fluidRow(
      tags$head(includeCSS("www/custom.css")),
      column(width = 4,
             tabler_card(
               title = span("Within child_cluster classification", class = "text-info"),
               height = "40vh",
               plotlyOutput("plot1", height = 230),
               tags$div(
                 style = "text-align: center",
                 tags$span(style = "color: rgb(42, 170, 82);", "label = 0"),
                 tags$span(style = "color: red;", "label = 1 "),
                 HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                 HTML("&#8226"),"pred = 0",
                 HTML("&#215"),"pred = 1"
                 #tags$span(style = "color: rgb(42, 170, 82);", HTML("&#8226")),
               )
               
             )
      ),
      column(width = 8,
             tabler_card(
               title = tags$span(
                 tags$span("Image Comparison", class = "text-info"),
                 tags$span(
                   style = "position: absolute; right: 0px; margin-right: 10px;",
                   actionButton("clear", "clear"),
                 )
               ),
               height = "40vh",
               uiOutput("comparison")
             )
             )
    ),
    hr(),
    fluidRow(
      column(
        width = 12,
        tabler_card(
          title = tags$span(
            tags$span("Recongnize image pattern within cluster", class = "text-info"),
            tags$span(
              style = "position: absolute; right: 0px; margin-right: 10px;",
              actionButton("show1","show parent_cluster images"),
              actionButton("show2","show child_cluster images"),
              actionButton("clear_small_images", "clear")
            )
          ),
          height = "30vh",
          uiOutput("image_outputs")
        )
      )
    )
  ),
  nav("Performance Evaluation"),
  nav_spacer(),
  nav_menu(
    "Source Code", align = "right",
    nav_item(
      tags$a(icon("r-project"), "RStudio", href = "https://rstudio.com", target = "_blank")
    )
  )
)