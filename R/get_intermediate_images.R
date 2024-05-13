

get_intermediate_images <- 
  function(path_name,
           crop_percentage = 0,
           my_isoblur = 1, 
           my_k = 0.01, 
           my_ws = 201,
           my_clean = 2,
           my_grow = 0, 
           my_shrink = 3){
    
    #from shiny app 
    
    results <- list()
    
    path_name %>% 
      EBImage::readImage() %>%
      percentage_crop(crop_percentage) %>% 
      magick::image_read() %>%
      imager::magick2cimg() %>%
      imager::grayscale() %>>% (~results$original = .) %>% 
      imager::isoblur(my_isoblur) %>>% (~results$isoblur = .) %>% 
      imagerExtra::ThresholdAdaptive(
        k = my_k, #0.1
        windowsize = my_ws, #39
        range = c(0,1)) %>>% (~results$adapThresh = . ) %>% 
      imager::clean(my_clean) %>>% (~results$clean = . ) %>% 
      imager::imgradient("xy") %>%
      imager::enorm() %>%
      imager::as.pixset() %>>% (~results$edge = . ) %>% 
      imager::grow(my_grow) %>>% (~results$grow = . ) %>%   
      imager::shrink(my_shrink) %>>% (~results$shrink = . )
    
    return(results)
  }