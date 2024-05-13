
do_tesselation <-  function(all_features, well_name, boundary_box){
  
  # all_features <- tessels_THP1 %>%
  #  filter(str_detect(file, "B4"))
  # well_name = "well"
  # boundary_box = c(0, pixset_dims %>% nth(1),0,
  #                 pixset_dims %>%  nth(2))
  
  all_features <- all_features %>% 
    mutate(id = row_number()) 
  
  tessels <- all_features %>% 
    mutate(well = well_name) %>% 
    group_by(well) %>%
    nest() %>% 
    mutate(tessel = purrr::map(data, 
                               ~{deldir::deldir(.x$x, .x$y, rw = boundary_box) %>%
                                   pluck("summary") %>% 
                                   mutate(id = row_number()) })) %>% 
    dplyr::select(-data) %>% 
    unnest(cols = c(tessel)) %>% 
    ungroup() 
  
  
  tessels <- tessels %>% 
    left_join(all_features, by = c("id")) %>% 
    rename(x = x.x,
           y = y.x)
  
  return(tessels)
  
  
}

prepare_colors <- function(tessels, input_image = "H"){
  
  if (input_image == "H"){
    bins_cuts <- c(0, 125, 250, 500, 1000,2000,4000, Inf)
  }
  if (input_image == "BF"){
    bins_cuts <- c(0, 125, 250, 500, 1000,2000,4000, Inf)/4
  }
  
  #generate the cut factorial column with color scheme for all wells in a plate
  tessels_colored <- 
    tessels %>% 
    group_by(well) %>% 
    nest() %>% 
    mutate(cut = purrr::map(data, 
                            ~{cut(.x$dir.area, 
                                  breaks=bins_cuts, 
                                  right=FALSE,
                                  labels=c("0", "125","250","500","1000","2000",">4000"))})) %>% 
    unnest(c= c(data, cut))
  
  return(tessels_colored)
  
}

cook_plot <- function(one_tessel, boundary_rectangle){
  
  grecosColors <- c("#4C3EBB","#3270FC","#009FFD","#00C1E1","#00D2A7","#B2CE38","#FFCA2E","#FFF700")
  empty_theme <- function(){theme(axis.text.x=element_blank(),
                                  axis.text.y =element_blank(),
                                  axis.title.x=element_blank(),
                                  axis.title.y =element_blank(),
                                  panel.background=element_blank(),
                                  panel.border=element_blank(),
                                  panel.grid.major=element_blank(),
                                  panel.grid.minor=element_blank(),
                                  plot.title = element_text(hjust = 0.5, vjust= 0, size =12),
                                  axis.ticks=element_blank(),
                                  axis.line=element_blank(),
                                  legend.spacing.x = unit(0.1, 'cm'),
                                  plot.background=element_blank())}
  
  plot <- one_tessel%>%
    ggplot() +
    #ggvoronoi::geom_voronoi(
    ggforce::geom_voronoi_segment(
      aes(x=x,
          y=-y,
          fill = cut),
      color = "black",
      linewidth = 0.02,
      #outline=boundary_rectangle) +
      bound = boundary_rectangle) +
    scale_fill_manual(values = grecosColors,
                      breaks = c("0", "75","150","300","600","1200","2400")) +
    theme(legend.position="none")  +
    empty_theme() +
    coord_fixed()
  
  attributes(plot)$well_name <- one_tessel %>% pull(well) %>% unique()
  
  return(plot)
  
}

cook_plot_new <- function(one_tessel, boundary_rectangle, 
                          print_well_label = FALSE,
                          color_me_white = FALSE,
                          color_my_class = FALSE,
                          color_non_white = 7,
                          my_max_radius = 10000,
                          my_linewidth = 0.02,
                          plot_points = FALSE,
                          with_legend = FALSE){
  
  grecosColors <- c("#4C3EBB","#3270FC","#009FFD","#00C1E1","#00D2A7","#B2CE38","#FFCA2E","#FFF700")
  my_breaks <- c("0", "125","250","500","1000","2000",">4000")
  if (color_me_white  == TRUE){
    max_bins = 8
    grecosColors <- c(rep("#4C3EBB", max_bins - color_non_white), rep("white", color_non_white))
  }
  
  if (color_my_class == TRUE){
    one_tessel <- one_tessel %>% 
      select(-cut) %>% 
      mutate(cut = as.factor(classification))
    grecosColors <- c("#4C3EBB","#00D2A7","#FFCA2E")
    my_breaks <- c("1", "2","3")
  }
  
  empty_theme <- function(){theme(axis.text.x=element_blank(),
                                  axis.text.y =element_blank(),
                                  axis.title.x=element_blank(),
                                  axis.title.y =element_blank(),
                                  panel.background=element_blank(),
                                  panel.border=element_blank(),
                                  panel.grid.major=element_blank(),
                                  panel.grid.minor=element_blank(),
                                  plot.title = element_text(hjust = 0.5, vjust= 0, size =12),
                                  axis.ticks=element_blank(),
                                  axis.line=element_blank(),
                                  legend.spacing.x = unit(0.1, 'cm'),
                                  plot.background=element_blank())}
  
  plot <- one_tessel %>%
    
    ggplot() +
    ggforce::geom_voronoi_tile(
      aes(x=x,
          y=y,
          fill = cut,
          group = -1L),
      color = "black",
      linewidth = my_linewidth,
      bound = boundary_rectangle,
      max.radius = my_max_radius,
      normalize = FALSE,) +
    {if(plot_points){geom_point(aes(x =x, y = y), 
                                size = 5, 
                                color = "darkgreen", alpha = 1)}}+
    {if(print_well_label){
      annotate(geom = "text",
               x = (boundary_rectangle %>% nth(2)) * 0.05,
               y = (boundary_rectangle %>% nth(3)) * -0.05,
               label = one_tessel %>%  pull(well) %>%  unique(),
               hjust = 0,
               vjust = 1,
               size = 12,
               color = "white")}}+
    scale_fill_manual(values = grecosColors,
                      breaks = my_breaks,
                      name = "Tile area\n(pixels)")+
    #labels = c("<31", "31-62","62-125","125-250","250-500","500-1000",">1000"),
    #name = "Tile area (pixels)") +
    scale_y_reverse()+
    {if(!with_legend){theme(legend.position="none")}}  +
    {if(with_legend){theme(legend.key.size = unit(2., 'cm'), #change legend key size
                           legend.key.height = unit(2, 'cm'), #change legend key height
                           legend.key.width = unit(2, 'cm'), #change legend key width
                           legend.title = element_text(size=45), #change legend title font size
                           legend.text = element_text(size=42))}}+ #change legend text font size
    empty_theme() +
    coord_fixed() 
  
  attributes(plot)$well_name <- one_tessel %>% pull(well) %>% unique()
  
  return(plot)
  
}
