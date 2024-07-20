

get_nuclei_old <- function(img, filepath, 
                           my_offset = 0.02,
                           my_opening_brush_size = 3,
                           brush_size = 21, #21
                           brush_type = "disc",
                           ws_tol = 1,
                           do_blur = FALSE,
                           sigma_blur = 0.001){
  
  # img <- filepath %>% EBImage::readImage() %>%
  # percentage_crop(crop_percentage)
  
  #brush_size = 11
  
  
  #set constants for EBImage::filter2
  disc = EBImage::makeBrush(brush_size, brush_type) #21
  disc = disc / sum(disc)
  offset = my_offset #default 0.02
  
  number_of_dimensions <- length(dim(img))
  
  nuc <- img
  
  #colormode
  EBImage::colorMode(nuc) <- EBImage::Grayscale
  
  if (do_blur == TRUE){
    
    nuc <- EBImage::gblur(nuc, sigma = sigma_blur)
  }
  
  #filter threshold (adaptive thresholdinng)
  # or in https://www.bioconductor.org/packages/devel/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html
  # nucThresh = original_img - bkgd_img > offset
  # nucThresh = nuc > nuc_bkgd + offset
  
  
  if(number_of_dimensions == 3){
    #take the third channel (this is the brightest grey channel)
    nucThresh = (nuc[,,3] - EBImage::filter2(nuc[,,3], disc ) > my_offset)
  }
  
  if(number_of_dimensions == 2){
    nucThresh = (nuc - EBImage::filter2(nuc, disc ) > my_offset)
  }
  
  #remove noise by opening
  nucOpened = EBImage::opening(nucThresh, 
                               kern = EBImage::makeBrush(my_opening_brush_size, 
                                                         shape = "disc")) #default 3
  
  #do watershedding (for dense images a higher )
  image_seed <- EBImage::watershed(EBImage::distmap(nucOpened), tolerance = ws_tol, ext = 1 ) #default tol = 1, ext = 1
  
  #burn path in attribute image object
  attributes(image_seed)$original_filepath <- filepath
  
  
  return(list(image_seed))
  
}

get_nuclei <- function(img, filepath, 
                       my_offset = 0.02,
                       my_opening_brush_size = 3,
                       brush_size = 21,
                       brush_type = "disc",
                       ws_tol = 1,
                       do_blur = FALSE,
                       sigma_blur = 0.001){
  
  filter_brush <- EBImage::makeBrush(size = brush_size, 
                                     shape = brush_type)
  filter_brush <- filter_brush / sum(filter_brush)
  opening_brush <- EBImage::makeBrush(size = my_opening_brush_size, 
                                      shape = brush_type)
  number_of_dimensions <- length(dim(img))
  
  if(number_of_dimensions == 4){
    nuc <- img[,,1,1]}
  
  if(number_of_dimensions == 3){
    nuc <- img[,,3]}
  
  if(number_of_dimensions == 2){
    nuc <- img}
  
  if (nuc %>% pluck(colorMode) == 2){
    EBImage::colorMode(nuc) <- EBImage::Grayscale}
  
  if (do_blur == TRUE){
    nuc <- EBImage::gblur(nuc, sigma = sigma_blur)}
  
  image_seed <- nuc %>% 
    magrittr::subtract(EBImage::filter2(., filter = filter_brush)) %>% 
    magrittr::is_greater_than(my_offset) %>% 
    EBImage::opening(., kern = opening_brush) %>% 
    EBImage::distmap() %>%
    EBImage::watershed(.,tolerance = ws_tol, ext = 1) %>% 
    structure(original_filepath = filepath)
  
  return(list(image_seed))
  
}



get_nuclei_fancy <- function(img, filepath, 
                             brush_size = 21,
                             brush_type = "disc",
                             ws_tol = 0.1,
                             my_sigma = 0.9,
                             my_SPE = 10,
                             sharpen_multiplier =  3){
  
  disc = EBImage::makeBrush(brush_size, brush_type) #21
  disc = disc / sum(disc)
  
  EBImage::colorMode(img) <- EBImage::Grayscale
  
  im_diff <- (img[,,3] - EBImage::filter2(img[,,3], disc))
  
  image_seed <-  (img[,,3] + sharpen_multiplier*im_diff) %>% 
    EBImage::normalize() %>% 
    magick::image_read() %>% 
    imager::magick2cimg() %>%  
    isoblur(.,my_sigma) %>% 
    imagerExtra::EqualizeADP() %>% 
    imagerExtra::SPE(my_SPE) %>% 
    imager::threshold() %>%  #a kmeans type Otsu variant thresh for bimodal distirbutions
    EBImage::Image() %>% 
    EBImage::fillHull() %>% 
    EBImage::distmap() %>% 
    EBImage::watershed(tolerance = ws_tol, 
                       ext = 1 )
  
  return(list(image_seed[,,1,1]))
  
  
}



get_nuclei_fancy_3 <- function(img, filepath, 
                               brush_size = 21,
                               brush_type = "disc",
                               ws_tol = 0.1,
                               my_sigma = 0.9,
                               my_SPE = 10,
                               sharpen_multiplier =  3,
                               my_clean = 2){
  
  filter_brush <- EBImage::makeBrush(size = brush_size, 
                                     shape = brush_type)
  filter_brush <- filter_brush / sum(filter_brush)
  
  EBImage::colorMode(img) <- EBImage::Grayscale
  
  number_of_dimensions <- length(dim(img))
  
  if(number_of_dimensions == 4){
    img <- img[,,1,1]}
  
  if(number_of_dimensions == 3){
    img <- img[,,3]}
  
  if(number_of_dimensions == 2){
    img <- img}
  
  if(number_of_dimensions == 4){
    
    #unsharp masking from:
    #https://bioimagebook.github.io/chapters/2-processing/4-filters/filters.html#chap-filters
    im_diff <- (img[,,1,1] - EBImage::filter2(img[,,1,1], filter_brush)) 
    
    image_seed <-  (img[,,1,1] + sharpen_multiplier*im_diff) %>%
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      imager::isoblur(.,my_sigma) %>% 
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE) %>% 
      imagerExtra::ThresholdML(k = 3) %>% 
      imager::imchange(. < 1.1, ~ 0) %>%
      imager::clean(my_clean) %>%  
      EBImage::Image() %>% 
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  
  if(number_of_dimensions == 3){
    
    im_diff <- (img[,,3] - EBImage::filter2(img[,,3], filter_brush))
    
    image_seed <-  (img[,,3] + sharpen_multiplier*im_diff) %>%
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      imager::isoblur(.,my_sigma) %>%
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE) %>% 
      imagerExtra::ThresholdML(k = 3) %>% 
      imager::imchange(. < 1.1, ~ 0) %>%
      imager::clean(my_clean) %>%  
      EBImage::Image() %>%
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  if(number_of_dimensions == 2){
    
    
    im_diff <- (img - EBImage::filter2(img, filter_brush))
    
    intermed <-  (img + sharpen_multiplier*im_diff) %>% 
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      isoblur(.,my_sigma) %>% 
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE)
    
    intermed_2 <- intermed %>% 
      imagerExtra::ThresholdML(k = 3)
    
    intermed_2[intermed_2<1.1] <- 0
    
    image_seed <- intermed_2 %>%
      clean(my_clean) %>%  
      EBImage::Image() %>% 
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  
  
  return(image_seed_returned)
  
  
}



get_nuclei_fancy_4 <- function(img, filepath, 
                               brush_size = 21,
                               brush_type = "disc",
                               ws_tol = 0.1,
                               my_sigma = 0.9,
                               my_SPE = 10,
                               sharpen_multiplier =  3,
                               my_clean = 2){
  
  # img <- "/Users/vincentdeboer/Documents/R/aws_targets_imageflower/protected/eu-north-1:ebba7029-2ec3-461a-aacd-fadab1f54b23/20221207_1700_gt_C8_EXP4_DAPI.tif" %>%
  #   EBImage::readImage() %>%
  #   percentage_crop()
  
  filter_brush <- EBImage::makeBrush(size = brush_size, 
                                     shape = brush_type)
  filter_brush <- filter_brush / sum(filter_brush)
  
  if (img %>% pluck(colorMode) == 2){
    EBImage::colorMode(img) <- EBImage::Grayscale}
  
  number_of_dimensions <- length(dim(img))
  
  if(number_of_dimensions == 4){
    img <- img[,,1,1]}
  
  if(number_of_dimensions == 3){
    img <- img[,,3]}
  
  if(number_of_dimensions == 2){
    img <- img}
  
  #unsharp masking from:
  #https://bioimagebook.github.io/chapters/2-processing/4-filters/filters.html#chap-filters
  

  
  im_diff <- img %>% 
    magrittr::subtract(EBImage::filter2(., filter_brush)) 
  
img %>%
    magrittr::add(sharpen_multiplier*im_diff) %>% 
    EBImage::normalize() %>% 
    magick::image_read() %>% 
    imager::magick2cimg() %>% 
    imagerExtra::EqualizeADP() %>%  
    imager::isoblur(.,my_sigma) %>% 
    imagerExtra::EqualizeADP() %>% 
    imagerExtra::SPE(my_SPE) %>% 
    imagerExtra::ThresholdML(k = 3) %>% 
    imager::imchange(. < 1.1, ~ 0) %>%
    imager::clean(my_clean) %>%  
    EBImage::Image() %>% 
    EBImage::fillHull() %>% 
    EBImage::distmap() %>% 
    EBImage::watershed(tolerance = ws_tol, 
                       ext = 1 ) %>% 
    EBImage::getFrame(1) %>% 
    structure(original_filepath = filepath)
  
  
img %>% 
  magick::image_read() %>%
  imager::magick2cimg() %>%
  imager::grayscale() %>% 
  imager::isoblur(1) %>% 
  imagerExtra::ThresholdAdaptive(
    k = my_k, #0.1
    windowsize = my_ws, #39
    range = c(0,1)) %>% plot

  return(list(image_seed))
  
}

get_nuclei_features_fancy <- function(image_seed, wellname){
  
  image_seed <- image_seed %>% purrr::pluck(1) 
  filepath <- attr(image_seed, "original_filepath")
  
  #get the features using the seed image
  FS_moment <- tidyr::as_tibble(EBImage::computeFeatures.moment(image_seed))
  FS_shape <- tidyr::as_tibble(EBImage::computeFeatures.shape(image_seed))
  features <- cbind(FS_moment, FS_shape) 
  
  #if images do not have features, thus are empty (like background images)
  #generate a table with only one object, else the output df cannot be generated
  if("m.cx" %in% colnames(features)){
    features <- features %>% 
      dplyr::rename(x = m.cx,
                    y = m.cy)
    
    #filepath <- attr(image_seed, "original_filepath")
    features <- features %>% 
      dplyr::mutate(file = basename(filepath)) %>% 
      dplyr::mutate(well = wellname) %>% 
      dplyr::select(file, well, everything())
    
  } else {
    #filepath <- attr(image_seed, "original_filepath")
    features <- tibble(file = filepath,
                       x = sample.int(400, 10, replace = FALSE),
                       y = sample.int(200, 10, replace = FALSE))
    attributes(features)$empty_image <- TRUE
    
    #filepath <- attr(image_seed, "original_filepath")
    features <- features %>% 
      dplyr::mutate(file = basename(filepath)) %>% 
      dplyr::mutate(well = wellname) %>% 
      dplyr::select(file, well, everything())
    
    
  }
  
  if(nrow(features) < 10){
    #filepath <- attr(image_seed, "original_filepath")
    features <- tibble(file = filepath,
                       x = sample.int(400, 10, replace = FALSE),
                       y = sample.int(200, 10, replace = FALSE))
    attributes(features)$empty_image <- TRUE
    
    #filepath <- attr(image_seed, "original_filepath")
    features <- features %>% 
      dplyr::mutate(file = basename(filepath)) %>% 
      dplyr::mutate(well = wellname) %>% 
      dplyr::select(file, well, everything())
    
    
  }
  
  # #return the file and x,y
  # filepath <- attr(image_seed, "original_filepath")
  # features <- features %>% 
  #   dplyr::mutate(file = basename(filepath)) %>% 
  #   dplyr::select(file, x,y)
  
  return(features)
  
}
 


get_nuclei_fancy_2 <- function(img, filepath, 
                               brush_size = 21,
                               brush_type = "disc",
                               ws_tol = 0.1,
                               my_sigma = 0.9,
                               my_SPE = 10,
                               sharpen_multiplier =  3,
                               my_clean = 1){
  
  disc = EBImage::makeBrush(brush_size, brush_type) #21
  disc = disc / sum(disc)
  
  #EBImage::colorMode(img) <- EBImage::Grayscale
  
  number_of_dimensions <- length(dim(img))
  
  if(number_of_dimensions == 4){
    
    #unsharp masking from:
    #https://bioimagebook.github.io/chapters/2-processing/4-filters/filters.html#chap-filters
    im_diff <- (img[,,1,1] - EBImage::filter2(img[,,1,1], disc)) 
    
    intermed <-  (img[,,1,1] + sharpen_multiplier*im_diff) %>% 
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      isoblur(.,my_sigma) %>% 
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE)
    
    intermed_2 <- intermed %>% 
      imagerExtra::ThresholdML(k = 3)
    
    intermed_2[intermed_2<1.1] <- 0
    
    image_seed <- intermed_2 %>%
      clean(my_clean) %>%  
      EBImage::Image() %>% 
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  if(number_of_dimensions == 3){
    
    im_diff <- (img[,,3] - EBImage::filter2(img[,,3], disc))
    
    intermed <-  (img[,,3] + sharpen_multiplier*im_diff) %>% 
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      isoblur(.,my_sigma) %>% 
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE)
    
    intermed_2 <- intermed %>% 
      imagerExtra::ThresholdML(k = 3)
    
    intermed_2[intermed_2<1.1] <- 0
    
    image_seed <- intermed_2 %>%
      clean(my_clean) %>%  
      EBImage::Image() %>% 
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  if(number_of_dimensions == 2){
    
    
    im_diff <- (img - EBImage::filter2(img, disc))
    
    intermed <-  (img + sharpen_multiplier*im_diff) %>% 
      EBImage::normalize() %>% 
      magick::image_read() %>% 
      imager::magick2cimg() %>%  
      isoblur(.,my_sigma) %>% 
      imagerExtra::EqualizeADP() %>% 
      imagerExtra::SPE(my_SPE)
    
    intermed_2 <- intermed %>% 
      imagerExtra::ThresholdML(k = 3)
    
    intermed_2[intermed_2<1.1] <- 0
    
    image_seed <- intermed_2 %>%
      clean(my_clean) %>%  
      EBImage::Image() %>% 
      EBImage::fillHull() %>% 
      EBImage::distmap() %>% 
      EBImage::watershed(tolerance = ws_tol, 
                         ext = 1 )
    
    image_seed_returned <- list(image_seed[,,1,1])
    
  }
  
  return(image_seed_returned)
  
  
}