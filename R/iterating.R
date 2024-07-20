my_loop_function <- function(path_name,
                             well_name,
                             i,j,k,l,m,n, 
                             crop_percentage = 0.05,
                             pixset_dims){
  
  processed_image <- path_name %>% 
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = crop_percentage) %>%
    magick::image_read() %>%
    imager::magick2cimg() %>%
    imager::grayscale() %>%
    imager::isoblur(i) %>%
    imagerExtra::ThresholdAdaptive(k = l,
                                   windowsize = m,
                                   range = c(0,1)) %>%
    imager::clean(n) %>%
    imager::imgradient("xy") %>%
    imager::enorm() %>%
    imager::as.pixset() %>%
    imager::grow(j) %>%
    imager::shrink(k) %>%
    structure(original_filepath = well_name) 
  
  tessels <- processed_image %>% 
    get_features_BF_for_mini() %>% 
    do_tesselation(., well_name,
                   c(0, pixset_dims %>% nth(1),0,
                     pixset_dims %>%  nth(2)))
  
  my_mean_G3 <- tessels %>% 
    do_mclust(my_G= 3)
  
  my_mean_G2 <- tessels %>% 
    do_mclust(my_G= 2)
  
  my_mean_noG <- tessels %>% 
    pull(s.area) %>%  mean()
  
  my_seed_max_area <- tessels %>% 
    pull(s.area) %>%  max()
  
  my_number_of_seeds <- nrow(tessels)
  
  df <- tibble(my_isoblur = i,
               my_grow = j,
               my_shrink = k,
               my_k = l,
               my_ws = m,
               my_clean = n,
               my_mean_G3 = my_mean_G3,
               my_mean_G2 = my_mean_G2,
               my_mean_noG = my_mean_noG,
               my_seed_max_area = my_seed_max_area,
               my_number_of_seeds = my_number_of_seeds,
               #my_tessels = tessels,
               path_name = path_name,
               file_name = basename(path_name))
  
  return(df)
  
  
}


my_loop_function_small <- function(path_name, 
                                   well_name, 
                                   i,j,k,l,m,n, 
                                   crop_percentage = 0.05,
                                   pixset_dims){
  
  seeds <- path_name %>%
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = crop_percentage) %>%
    magick::image_read() %>%
    imager::magick2cimg() %>%
    imager::grayscale() %>%
    imager::isoblur(i) %>%
    imagerExtra::ThresholdAdaptive(k = l,
                                   windowsize = m,
                                   range = c(0,1)) %>%
    imager::clean(n) %>%
    imager::imgradient("xy") %>%
    imager::enorm() %>%
    imager::as.pixset() %>%
    imager::grow(j) %>%
    imager::shrink(k) %>%
    structure(original_filepath = well_name) %>% 
    get_features_BF_for_mini() %>% 
    do_tesselation(.,well_name,
                   c(0, pixset_dims %>% nth(1),0,
                     pixset_dims %>%  nth(2))) %>% 
    summarize(n = n()) %>%  pull(n)
  
  return(tibble(well_name = well_name,
                n = seeds,
                path_name = path_name))
  
  
}

my_loop_function_small_2 <- function(path_name, 
                                     well_name, 
                                     i,j,k,l,m,n, 
                                     crop_percentage = 0.05){
  
  seeds <- path_name %>%
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = crop_percentage) %>%
    magick::image_read() %>%
    imager::magick2cimg() %>%
    imager::grayscale() %>%
    imager::isoblur(i) %>%
    imagerExtra::ThresholdAdaptive(k = l,
                                   windowsize = m,
                                   range = c(0,1)) %>%
    imager::clean(n) %>%
    imager::imgradient("xy") %>%
    imager::enorm() %>%
    imager::as.pixset() %>%
    imager::grow(j) %>%
    imager::shrink(k) %>%
    structure(original_filepath = well_name) %>% 
    get_features_BF_for_mini() %>% 
    do_tesselation(.,well_name,
                   c(0, pixset_dims %>% nth(1),0,
                     pixset_dims %>%  nth(2))) 
  
  return(seeds)
  
  
}

my_loop_function_small_3 <- function(path_name, 
                                     well_name, 
                                     i,j,k,l,m,n, 
                                     crop_percentage = 0.05,
                                     pixset_dims){
  
  seeds <- path_name %>%
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = crop_percentage) %>%
    magick::image_read() %>%
    imager::magick2cimg() %>%
    imager::grayscale() %>%
    imager::isoblur(i) %>%
    imagerExtra::ThresholdAdaptive(k = l,
                                   windowsize = m,
                                   range = c(0,1)) %>%
    imager::clean(n) %>%
    imager::imgradient("xy") %>%
    imager::enorm() %>%
    imager::as.pixset() %>%
    imager::grow(j) %>%
    imager::shrink(k) %>%
    structure(original_filepath = well_name) %>% 
    get_features_BF_for_mini() 
  
  
  return(seeds)
  
  
}

do_mclust <- function(my_tessels, my_G = 3){
  
  #my_tessels <- my_mean
  
  my_mods <- my_tessels %>% 
    nest(.by = c(well)) %>%
    mutate(posteriors =
             purrr::map(data, ~ .x %>%
                          pull(dir.area) %>%
                          mclust::densityMclust(., G = my_G, modelName = "V", #g=2 or 3
                                                plot = FALSE,
                                                verbose = FALSE))) %>%
    mutate(error = case_when(is.null(posteriors %>%  pluck(1)) ~ TRUE,
                             .default = FALSE))
  
  if (my_mods$error %>% pluck(1)){
    my_mean = 0
  } else{
    
    my_mean <- 
      my_mods %>% 
      mutate(posteriors_2 = 
               purrr::map(.x = posteriors, 
                          .f = ~ .x %>% 
                            keep(names(.) %in% c("data","classification", "z"))%>%
                            as.data.frame())) %>% 
      unnest(c(posteriors_2, data)) %>%
      summarize(mean_dir.area = mean(dir.area),
                .by = classification) %>%
      arrange(classification) %>%
      filter(classification == 1) %>%
      pull(mean_dir.area)
    
  }
  return(my_mean)
  
}

my_loop_function_smallest <- function(path_name, 
                                      well_name, 
                                      i,j,k,l,m,n, 
                                      crop_percentage = 0.05){
  
  path_name %>%
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = crop_percentage) %>%
    magick::image_read() %>%
    imager::magick2cimg() %>%
    imager::grayscale() %>%
    imager::isoblur(i) %>%
    imagerExtra::ThresholdAdaptive(k = l,
                                   windowsize = m,
                                   range = c(0,1)) %>%
    imager::clean(n) %>%
    imager::imgradient("xy") %>%
    imager::enorm() %>%
    imager::as.pixset() %>%
    imager::grow(j) %>%
    imager::shrink(k) %>%
    structure(original_filepath = well_name)
  
  
}