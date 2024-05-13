


get_features_BF <- function(image_seed){
  #image_seed <- pixset_BF
  
  FS_moment <- image_seed %>% 
    #imager::shrink(3) %>% 
    EBImage::Image() %>% 
    EBImage::getFrame(1) %>% 
    EBImage::bwlabel() %>% 
    EBImage::computeFeatures.moment() %>% 
    as_tibble() %>% 
    dplyr::mutate(number = row_number()) %>% 
    dplyr::rename(x = m.cx,
                  y = m.cy)
  
  filepath <- attr(image_seed, "original_filepath")
  
  features <- FS_moment %>% 
    dplyr::mutate(file = basename(filepath)) %>% 
    dplyr::select(file, x,y)
  
  return(features)
  
}

get_features_BF_new <- function(image_seed){
  
  my_filepath <- attr(image_seed, "original_filepath")
  
  if (is.null(my_filepath)){
    my_filepath <- "unknown_filename"}
  
  image_seed %>% 
    #imager::shrink(3) %>% 
    EBImage::Image() %>% 
    EBImage::getFrame(1) %>% 
    EBImage::bwlabel() %>% 
    EBImage::computeFeatures.moment() %>% 
    tidyr::as_tibble() %>% 
    dplyr::mutate(number = row_number()) %>% 
    dplyr::rename(x = m.cx,
                  y = m.cy) %>% 
    dplyr::mutate(file = basename(my_filepath)) %>% 
    dplyr::select(file, x,y)
  
}

get_features_BF_for_mini <- function(image_seed){
  
  #image_seed <- my_mean %>%  pluck(1)
  my_filepath <- attr(image_seed, "original_filepath")
  
  if (is.null(my_filepath)){
    my_filepath <- "unknown_filename"}
  
  features_shape <-  image_seed %>% 
    EBImage::Image() %>% 
    EBImage::getFrame(1) %>% 
    EBImage::bwlabel() %>% 
    EBImage::computeFeatures.shape() %>% 
    tidyr::as_tibble() %>% 
    dplyr::mutate(number = row_number())
  
  features_moment <- image_seed %>% 
    EBImage::Image() %>% 
    EBImage::getFrame(1) %>% 
    EBImage::bwlabel() %>% 
    EBImage::computeFeatures.moment() %>% 
    tidyr::as_tibble()
  
  
  features <- cbind(features_moment, features_shape)
  
  if("m.cx" %in% colnames(features)){
    features <- features %>% 
      dplyr::rename(x = m.cx,
                    y = m.cy) %>% 
      dplyr::mutate(file = basename(my_filepath)) 
    
  } else{
    
    #dummy tibble of size ten to allow calculations by deldir
    features <- tibble(file = my_filepath,
                       x = sample.int(400, 10, replace = FALSE),
                       y = sample.int(200, 10, replace = FALSE),
                       s.area = sample.int(10, 10, replace = FALSE),
                       s.radius.max = sample.int(10, 10, replace = FALSE),
                       s.perimeter = sample.int(10, 10, replace = FALSE),
                       s.radius.mean = sample.int(10, 10, replace = FALSE))
    
    attributes(features)$empty_image <- TRUE
    
    #filepath <- attr(image_seed, "original_filepath")
    features <- features %>% 
      dplyr::mutate(file = basename(my_filepath)) 
  }
  
  if(nrow(features) < 10){
    #dummy tibble of size ten to allow calculations by deldir
    features <- tibble(file = my_filepath,
                       x = sample.int(400, 10, replace = FALSE),
                       y = sample.int(200, 10, replace = FALSE),
                       s.area = sample.int(10, 10, replace = FALSE),
                       s.radius.max = sample.int(10, 10, replace = FALSE),
                       s.perimeter = sample.int(10, 10, replace = FALSE),
                       s.radius.mean = sample.int(10, 10, replace = FALSE))
    attributes(features)$empty_image <- TRUE
    
    #filepath <- attr(image_seed, "original_filepath")
    features <- features %>% 
      dplyr::mutate(file = basename(my_filepath)) 
    
    
  }
  
  return(features)
}