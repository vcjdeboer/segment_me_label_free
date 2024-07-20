
#collection of all libraries in all figure qmd files
library(tidyverse)
library(future)
library(here)

library(fs)
library(inborutils)

library(EBImage)
library(imager)
library(imagerExtra)
library(magick)

library(deldir)

library(mclust)

library(geomtextpath)
library(spatstat)
library(gt)
library(dbscan)

library(pipeR)

library(ggdist)
library(ggforce)
library(colorspace)

library(jsonlite)

library(NCmisc)

library(shiny)
library(shinyWidgets)
library(bslib)
library(sf)

#functions 
source("R/cropping.R")
source("R/iterating.R")
source("R/get_tesselation.R")
source("R/get_features.R")
source("R/iterating.R")

       
segment_my_image <- function(filepath, cell_size_target, 
                             my_crop_percentage = 0.05,
                             number_of_workers = 10){
  
  pixset_dims <- filepath %>% 
    EBImage::readImage() %>%
    percentage_crop(crop_percentage = 
                      my_crop_percentage) %>% 
    dim()
  
  #define parameter set
  my_isoblur <- c(0,1)
  my_grow <- c(0,1,2)
  my_shrink <- c(3,4,5,6)
  my_k <- c(0.1) #for thp1 and c2c12 no 0.01
  my_ws <- c(39, 99, 201)
  my_clean <- c(0, 1, 2)
  
  param_set <- expand_grid(
    path_name = filepath,
    my_isoblur,
    my_grow,
    my_shrink,
    my_k,
    my_ws,
    my_clean)
  
  future::plan(future::multisession, workers = number_of_workers)
  
  params_results_c2c12 <-
    list(file_name = param_set$path_name,
         .i = param_set$my_isoblur,
         .j = param_set$my_grow,
         .k = param_set$my_shrink,
         .l = param_set$my_k,
         .m = param_set$my_ws,
         .n = param_set$my_clean) %>%
    furrr::future_pmap(
      .l = .,
      .f = function(file_name, .i, .j, .k, .l, .m, .n)
        file_name %>%
        my_loop_function(well_name = basename(.),
                         i = .i, j = .j, k = .k, l = .l, m = .m, n = .n,
                         crop_percentage = my_crop_percentage,
                         pixset_dims = pixset_dims),
      .options = furrr::furrr_options(seed = NULL)) %>%
      list_rbind()
  

  params_results_c2c12_annotated <-
    params_results_c2c12 %>%
    mutate(well = str_sub(file_name, 18, 20)) %>%
    mutate(well = case_when(
      str_detect(well, "_") ~ paste0(
        str_sub(well, 1, 1), "0",
        str_sub(well, 2, 2)),
      .default = well)) %>%
    mutate(id = str_sub(file_name, 1, -8)) %>%
    mutate(cell_size_xpol = cell_size_target) 
  
  summarized_c2c12 <- params_results_c2c12_annotated %>%
    mutate(distance_mean = (my_mean_G3 - cell_size_xpol)) %>%
    filter(my_seed_max_area < cell_size_target) %>%
    slice_max(order_by = -abs(distance_mean), n = 20) %>%
    summarize(
      mean_G3 = mean(my_mean_G3),
      mean_G2 = mean(my_mean_G2),
      mean_noG = mean(my_mean_noG),
      n = mean(my_number_of_seeds),
      .by = file_name)
  
  #get xy positions for optimized paramters
  col <- "C"
  row <- "8"
  my_well_with_zero <- paste0(col, "0", row)
  my_well <- paste0(col, row)
  
  my_file_path <- params_results_c2c12_annotated %>% 
    filter(str_detect(path_name, my_well)) %>%  pull(path_name) %>%  unique()
  
  my_mean_n <- summarized_c2c12 %>% 
    filter(str_detect(file_name, my_well)) %>% pull(n)
  
  my_best_param_set <- params_results_c2c12_annotated %>% 
    mutate(distance_mean = (my_mean_G3-cell_size_xpol)) %>% 
    #mutate(distance_max = (my_seed_max_area-cell_size_target)) %>% 
    filter(my_seed_max_area< cell_size_target) %>% 
    slice_max(order_by = -abs(distance_mean), n = 20, by = file_name ) %>% 
    filter(well == my_well_with_zero) %>% 
    mutate(delta = abs(my_number_of_seeds - my_mean_n)) %>% 
    arrange(delta) %>% 
    slice(1) %>% 
    select(1:6)
  
  
  my_best_xy <- my_loop_function_small_3(
    my_file_path, 
    my_well,
    i = my_best_param_set$my_isoblur ,
    j = my_best_param_set$my_grow ,
    k = my_best_param_set$my_shrink ,
    l = my_best_param_set$my_k ,
    m = my_best_param_set$my_ws ,
    n = my_best_param_set$my_clean,
    pixset_dims = pixset_dims)
  
  return(list( n = summarized_c2c12$n, xy = my_best_xy %>% select(x, y)))
  
}
