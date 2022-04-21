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
      theme_bw()
    
    ggplotly(p)
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
  
  output$image_outputs = renderUI({
    req(small_cluster_imgs())
    small_cluster_imgs()
    
  })
  
  click_history = reactiveVal()
  observeEvent(event_data("plotly_click"),{
    new = bind_rows(
      click_history(),
      event_data("plotly_click")
    )
    click_history(new)
  })
  observeEvent(input$clear, {
    click_history(NULL)
  })
  
  output$comparison = renderUI({
    
    req(click_history())
    
    click_df = click_history() %>%
      select(x, y) %>%
      rename(x1 = x, x2 = y)
    
    imgs = click_df %>%
      left_join(sub_df(), by = c("x1","x2")) %>%
      pull(img)
    size = rep("200px", length(imgs))
    
    purrr::map2(imgs, size, my_img_func)
    
    
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