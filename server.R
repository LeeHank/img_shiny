server <- function(input, output, session) {
  
  observeEvent(input$parent_cluster,{
    child_choices = df %>%
      filter(parent_cluster == input$parent_cluster) %>%
      pull(child_cluster) %>%
      unique()
    
    updatePickerInput(
      session = session,
      inputId = "child_cluster",
      choices = child_choices
    )
  })
  
  
  sub_df = reactive({
    df %>%
      filter(parent_cluster == input$parent_cluster &
               child_cluster == input$child_cluster) %>%
      mutate(label = as.character(label),
             x1 = round(x1, 4),
             x2 = round(x2, 4))
  })
  
  output$plot1 <- renderPlotly({
    
    p = sub_df() %>%
      ggplot(aes(x1, x2, col = label)) + 
      geom_point() +
      theme_bw() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank())
    
    ggplotly(p) %>%
      event_register('plotly_click') %>%
      config(displayModeBar = F)
  })
  

  small_cluster_imgs = reactiveVal()
  
  observeEvent(input$show1, {
    
    imgs = df %>%
      filter(parent_cluster == input$parent_cluster) %>%
      pull(img)
    size = rep("50px", length(imgs))
    
    small_cluster_imgs(purrr::map2(imgs, size, my_img_func))
    
  })
  
  observeEvent(input$show2, {
    
    imgs = df %>%
      filter(parent_cluster == input$parent_cluster,
             child_cluster == input$child_cluster) %>%
      pull(img)
    
    size = rep("50px", length(imgs))
    small_cluster_imgs(purrr::map2(imgs, size, my_img_func))
    
  })
  
  observeEvent(input$clear_small_images, {
    small_cluster_imgs(NULL)
  })
  output$image_outputs = renderUI({
    if(is.null(small_cluster_imgs())){
      tags$span("Click button to show images :)", class = "text-primary")
    }else{
      div(
        style = "display: flex; flex-direction: row; flex-wrap: wrap;",
        small_cluster_imgs()
      )
    }
  })
  
  observeEvent(input$select_link,{
    showModal(
      modalDialog(
        size = "l",
        title = tags$span(
          tags$b(input$select_link), 
          "的詳細資訊", HTML("&nbsp;&nbsp;"), 
          tags$span(
            style = "position: absolute; right: 0px; margin-right: 10px;",
            modalButton("", icon = icon('times'))
          )
        ),
        easyClose = TRUE,
        tags$img(src = input$select_link),
        footer = NULL
      )
    )
  })
  
  click_history = reactiveVal()
  observeEvent(event_data("plotly_click"),{
    
    click_data = event_data("plotly_click") %>%
      select(x, y) %>%
      rename(x1 = x, x2 = y) %>%
      left_join(sub_df(), by = c("x1","x2"))
    
    new = bind_rows(
      click_history(),
      click_data
    )
    click_history(new)
      
  })
  observeEvent(input$clear, {
    click_history(NULL)
  })
  
  output$comparison = renderUI({
    
    if(is.null(click_history())){
      tags$span("Click chart point to show images :)", class = "text-primary")
    }else{
      imgs = click_history() %>% pull(img)
      size = rep("200px", length(imgs))
      
      div(
        style = "display: flex; flex-direction: row; flex-wrap: wrap;",
        purrr::map2(imgs, size, my_img_func)
      )
    }
  })
  
  output$myImage <- renderImage({
    
    d <- event_data("plotly_click")
    req(d)
    
    if (!is.null(d)){
      img = sub_df() %>%
        filter(x1 == d$x,
               x2 == d$y) %>%
        pull(img)
      
      list(
        src = img, #df$img[1],
        alt = "hahaha",
        width = "300",
        height = "300"
      )
    }
    
  }, deleteFile = FALSE)
  
}