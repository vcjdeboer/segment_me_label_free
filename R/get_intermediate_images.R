

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

get_pixset_from_BF_fresh_image_input <- function(path_name,
                                                 crop_percentage = 0,
                                                 my_isoblur = 1, 
                                                 my_k = 0.01, my_ws = 201,
                                                 my_clean = 2,
                                                 my_grow = 0, my_shrink = 3){
  results <- list()
  
  file_name <- "my_default_wellname"
  
  path_name %>% 
    #EBImage::readImage() %>%
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
  
  results <- results %>%  
    structure(
      crop_precentage = crop_percentage,
      my_k  = my_k,
      my_ws = my_ws,
      my_isoblur = my_isoblur,
      my_clean = my_clean,
      my_grow = my_grow,
      my_shrink = my_shrink,
      my_filename = file_name)
  #my_pathname = path_name)
  
  return(results)
}
