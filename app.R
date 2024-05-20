
library(tidyverse)

library(EBImage)
library(imager)
library(magick)

library(shiny)
library(shinyWidgets)

library(sf)

library(bslib)

#sources
source("R/cropping.R")

my_thp1_image <- "data/20231116_1300_mm_THP1/20231116_1300_mm_D5_EXP1_BF.tif" %>% 
  EBImage::readImage() 

my_c2c12_image <- "data/20221207_1700_gt_C2C12/20221207_1700_gt_C8_EXP4_BF.tif" %>% 
  EBImage::readImage()

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel(
    h4("Cell size tool"),
    
  ),
  sidebarLayout(
    
    sidebarPanel(width = 2,
                 style = "height: 90vh; overflow-y: auto;", 
                 strong("Select input image:"),
                 fileInput("image", NULL,
                           buttonLabel = "Upload...",
                           multiple = FALSE),
                 selectInput("default_image", 
                             "Or select a default image:", 
                             c("THP1",
                               "C2C12")),
                 hr(),
                 switchInput(inputId = "enhance_contrast", 
                             "Equalize",
                             value = FALSE)
                 
    ),
    mainPanel(width = 10,
              style = "height: 90vh; overflow-y: auto;", 
              tabsetPanel(
                tabPanel("Cell size",
                         
                         sliderInput("mywidth", "width of the pencil", min=1, max=10, step=1, value=3),
                         
                         selectInput("snapshot_region_zoom", "Select a region:", 
                                     c("leftuppercorner", "leftbottomcorner", 
                                       "rightuppercorner", "rightbottomcorner" , 
                                       "middle", "random")),
                         
                         sliderInput("myzoom", "zoom", min=0.1, max=0.5, step=0.02, value=0.25),
                         
                         actionButton("reset_polygon", "reset polygon"),
                         
                         actionButton("reset_mean_area", "reset mean_area"),
                         
                         actionButton("record", "record"),
                         
                         verbatimTextOutput("mean_area"),
                         
                         tableOutput("area_table"),
                         div(
                           class = "large-plot",
                           plotOutput(outputId = "plot_bg",
                                      width = "915px",
                                      height = "674px",),
                           plotOutput(outputId = "plotx",
                                      width = "915px", 
                                      height = "674px",
                                      click="plot_click")
                         ),
                         tags$style(
                           ".large-plot {
              position: relative;
              }
              #plot_bg {
                position: absolute;
              }        
              #plotx {
                position: absolute;
              }"
                         )
                         
                ) #tabpanel 
              ) #tabsetpanel
    ) #mainpanel
  ) #sidebarlayout
) #ui

server <- function(input, output, session) {
  
  input_image <- reactive({
    if (is.null(input$image)) {
      #my_image <- my_thp1_image
      
      my_selected_celltype <- input$default_image
      
      if (my_selected_celltype == "THP1"){my_image <- my_thp1_image} else{
        if (my_selected_celltype == "C2C12"){my_image <- my_c2c12_image}}  
      
    } else {
      my_image <- input$image$datapath %>% EBImage::readImage()
    }
  })
  
  original_image <- reactive({
    
    image_1 <- input_image() %>% 
      magick::image_read() 
    
    if (input$enhance_contrast){
      image_1 <- image_1 %>%
        magick::image_equalize()}
    
    return(image_1)
  })
  
  #tabPanel 1
  xy <- reactiveVal(tibble(x = integer(), 
                           y = integer()))
  area_table <- reactiveVal(tibble(n = integer(), 
                                   area = numeric()))
  
  mean_area <- reactive({
    area_table() %>%  
      pull(area) %>%  
      mean()
  })
  
  polygon <- reactive({
    first_value_x <- xy()$x %>% nth(1)
    first_value_y <- xy()$y %>% nth(1)
    
    x_coords <- c(xy()$x, first_value_x) %>%  na.omit()
    y_coords <- c(xy()$y, first_value_y) %>%  na.omit()
    
    sf::st_sfc(sf::st_polygon(list(cbind(x_coords,y_coords))))
  })
  
  image_bg <- reactive({ 
    
    i <- req(original_image())
    my_image <- i %>% 
      imager::magick2cimg() %>% 
      subset_imager(input$snapshot_region_zoom, 
                    subset_percentage = input$myzoom,
                    my_seed = 1000) 
  })
  
  image_bg_h <- reactive({
    image_bg() %>%  dim() %>%  nth(2)
  })
  
  image_bg_w <- reactive({
    image_bg() %>%  dim() %>%  nth(1)
  })
  
  #observers
  observeEvent(input$plot_click, handlerExpr = {
    
    xy() %>%
      add_row(
        x = input$plot_click$x,
        y = input$plot_click$y
      ) %>%
      xy()
    
  })
  
  observeEvent(input$reset_polygon, handlerExpr = {
    xy(tibble(x = integer(), 
              y = integer()))
  })
  
  observeEvent(input$reset_mean_area, handlerExpr = {
    area_table(tibble(n = integer(), 
                      area = numeric()))
  })
  
  observeEvent(input$record, handlerExpr = {
    
    if(nrow((xy())!= 0)){
      area_table() %>%
        add_row(
          n = nrow(area_table()) + 1,
          area = sf::st_area(polygon())
        ) %>%
        area_table()
    }
    
    xy(tibble(x = integer(), 
              y = integer()))
  })
  
  #outputs
  output$plotx <- renderPlot({
    plot(
      x=xy()$x, y=xy()$y,
      xlim=c(0, image_bg_w()), ylim=c(0, image_bg_h()),
      ylab="y", xlab="x",
      type="l", lwd=input$mywidth)
    
  }, bg = "transparent")
  
  output$plot_bg <- renderPlot({
    
    my_image <- image_bg()
    
    plot(c(0, image_bg_w()), c(0, image_bg_h()),
         type = "n", xlab = "", ylab = "",
         xaxt='n', ann=FALSE)
    
    rasterImage(my_image,
                xleft=0, xright=image_bg_w(),
                ybottom=0, ytop=image_bg_h())
    
    
  })
  
  output$polygon <- renderPlot({
    req(input$plot_click, cancelOutput = TRUE)
    
    plot(polygon(), axes = TRUE)
    
  })
  
  output$area <- renderText({
    req(input$click, cancelOutput = TRUE)
    
    sf::st_area(polygon())
  })
  
  output$area_table <- renderTable(area_table())
  
  output$xy <- renderTable(xy())
  
  output$mean_area <- renderText({
    
    paste0("mean cell size: ", mean_area())
    
  })
  
  output$mean_area_2 <- renderText({
    
    paste0("mean cell size: ", mean_area())
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

#background css from:
#https://stackoverflow.com/questions/71161027/r-shiny-create-non-reactive-background-in-plotoutput

#polygon from:
#from https://stackoverflow.com/questions/61975159/how-do-i-calculate-area-in-polygon-from-x-and-y-points-in-r

#first clicking/hovering from:
#https://stackoverflow.com/questions/41701807/way-to-free-hand-draw-shapes-in-shiny/48442522#48442522

