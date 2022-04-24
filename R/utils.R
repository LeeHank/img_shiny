# ui relevant
my_img_func = function(path, size = "50px"){
  div(
    id = path,
    style="cursor: pointer;",
    onclick = 'Shiny.setInputValue(\"select_link\", this.id, {priority: \"event\"})',
    tags$img(src = path, 
             width = size, 
             height = size, 
             style = "margin: 0.5rem 0.5rem")
  )
}

tabler_card = function(title, height = NULL, ...){
  
  if(is.null(height)){
    style = NULL
  }else{
    style = paste0("height: ", height)
  }
  
  div(
    class = "tabler-card",
    style = style,
    div(
      class = "tabler-card-body",
      span(class = "tabler-card-title", title),
      ...
    )
  )
}