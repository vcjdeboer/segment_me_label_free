


manual_crop <- function(image,
                        crop_y_left, crop_y_right,
                        crop_x_low, crop_x_up){
  
  # crop_y_left = 0.26
  # crop_y_right = 0.34
  # crop_x_low = 0.33
  # crop_x_up = 0.37
  
  leftBound <- round((crop_y_left)*(nrow(image)))
  rightBound <- round((nrow(image)-(crop_y_right)*(nrow(image))))
  lowerBound <- round((crop_x_low)*(ncol(image)))
  upperBound <- round((ncol(image)-(crop_x_up)*(ncol(image))))
  
  ix <- leftBound:rightBound
  iy <- lowerBound:upperBound
  
  if (length(dim(image)) == 2){
    cropped_image <- image[ix,iy]}
  
  if (length(dim(image)) == 3){
    cropped_image <- image[ix,iy,]}
  
  if (length(dim(image)) == 4){
    cropped_image <- image[ix,iy,,]} 
  
  return(cropped_image)
}

percentage_crop<-function(image, crop_percentage = 0.05){
  
  leftBound <- round((crop_percentage)*(nrow(image)))
  rightBound <- round((nrow(image)-(crop_percentage)*(nrow(image))))
  lowerBound <- round((crop_percentage)*(ncol(image)))
  upperBound <- round((ncol(image)-(crop_percentage)*(ncol(image))))
  ix <- leftBound:rightBound
  iy <- lowerBound:upperBound
  
  if (length(dim(image)) == 2){
    cropped_image <- image[ix,iy]}
  
  if (length(dim(image)) == 3){
    cropped_image <- image[ix,iy,]}
  
  if (length(dim(image)) == 4){
    cropped_image <- image[ix,iy,,]} 
  
  return(cropped_image)
}


subset_EBImage <- function(image, 
                           where = "rightbottomcorner",
                           subset_percentage = 0.15){
  
  max_x <- dim(image)[1]
  max_y <- dim(image)[2]
  
  if (where == "all"){
    
    ix <- 0:max_x
    iy <- 0:max_y
  }
  
  if (where == "rightbottomcorner"){
    my_x <- round(max_x - subset_percentage * max_x)
    my_y <- round(max_y - subset_percentage * max_y)
    ix <- my_x:max_x
    iy <- my_y:max_y
  }
  
  if (where == "leftuppercorner"){
    my_x <- round(subset_percentage * max_x)
    my_y <- round(subset_percentage * max_y)
    ix <- 0:my_x
    iy <- 0:my_y
  }
  
  if (where == "leftbottomcorner"){
    my_x <- round(subset_percentage * max_x)
    my_y <- round(max_y - subset_percentage * max_y)
    ix <- 0:my_x
    iy <- my_y:max_y
  }
  
  if (where == "rightuppercorner"){
    my_x <- round(max_x - subset_percentage * max_x)
    my_y <- round(subset_percentage * max_y)
    ix <- my_x:max_x
    iy <- 0:my_y
  }
  
  if (where == "middle"){
    my_x <- round(max_x/2 - ((subset_percentage/2) * (max_x)))
    max_x <- round(max_x/2 + ((subset_percentage/2) * (max_x)))
    my_y <- round(max_y/2 - ((subset_percentage/2) * (max_y)))
    max_y <- round(max_y/2 + ((subset_percentage/2) * (max_y)))
    ix <- my_x:max_x
    iy <- my_y:max_y
  }
  
  if (where == "random"){
    
    x_allowed <- round(subset_percentage/2 * max_x +2):round((max_x - subset_percentage/2 * max_x) - 2)
    y_allowed <- round(subset_percentage/2 * max_y +2):round((max_y - subset_percentage/2 * max_y) - 2)
    
    random_x <- sample(x_allowed, 1) 
    random_y <- sample(y_allowed, 1) 
    
    my_x <- round(random_x - subset_percentage/2 * max_x/2)
    max_x <- round(random_x + subset_percentage/2 * max_x/2)
    my_y <- round(random_y - subset_percentage/2 * max_y/2)
    max_y <- round(random_y + subset_percentage/2 * max_y/2)
    
    ix <- my_x:max_x
    iy <- my_y:max_y
  }
  
  
  if (length(dim(image)) == 2){
    cropped_image <- image[ix,iy]}
  
  if (length(dim(image)) == 3){
    cropped_image <- image[ix,iy,]}
  
  if (length(dim(image)) == 4){
    cropped_image <- image[ix,iy,,]} 
  
  return(cropped_image)
  
}

subset_imager <- function(image, 
                          where = "rightbottomcorner",
                          subset_percentage = 0.15){
  
  max_x <- dim(image)[1]
  max_y <- dim(image)[2]
  
  if (where == "all"){
    
    ix <- c(0,max_x)
    iy <- c(0,max_y)
  }
  
  if (where == "rightbottomcorner"){
    my_x <- round(max_x - subset_percentage * max_x)
    my_y <- round(max_y - subset_percentage * max_y)
    ix <- c(my_x,max_x)
    iy <- c(my_y,max_y)
  }
  
  if (where == "leftuppercorner"){
    my_x <- round(subset_percentage * max_x)
    my_y <- round(subset_percentage * max_y)
    ix <- c(1,my_x)
    iy <- c(1,my_y)
  }
  
  if (where == "leftbottomcorner"){
    my_x <- round(subset_percentage * max_x)
    my_y <- round(max_y - subset_percentage * max_y)
    ix <- c(1,my_x)
    iy <- c(my_y,max_y)
  }
  
  if (where == "rightuppercorner"){
    my_x <- round(max_x - subset_percentage * max_x)
    my_y <- round(subset_percentage * max_y)
    ix <- c(my_x,max_x)
    iy <- c(1,my_y)
  }
  
  if (where == "middle"){
    my_x <- round(max_x/2 - (subset_percentage/2 * max_x))
    max_x <- round(max_x/2 + (subset_percentage/2 * max_x))
    my_y <- round(max_y/2 - (subset_percentage/2 * max_y))
    max_y <- round(max_y/2 + (subset_percentage/2 * max_y))
    ix <- c(my_x,max_x)
    iy <- c(my_y,max_y)
  }
  
  if (where == "random"){
    
    x_allowed <- round(subset_percentage/2 * max_x +2):round((max_x - subset_percentage/2 * max_x) - 2)
    y_allowed <- round(subset_percentage/2 * max_y +2):round((max_y - subset_percentage/2 * max_y) - 2)
    
    random_x <- sample(x_allowed, 1) 
    random_y <- sample(y_allowed, 1) 
    
    my_x <- round(random_x - subset_percentage/2 * max_x/2)
    max_x <- round(random_x + subset_percentage/2 * max_x/2)
    my_y <- round(random_y - subset_percentage/2 * max_y/2)
    max_y <- round(random_y + subset_percentage/2 * max_y/2)
    
    ix <- c(my_x,max_x)
    iy <- c(my_y,max_y)
  }
  
  
  if (length(dim(image)) == 2){
    cropped_image <- imsub(image,x %inr% ix, y %inr% iy)}
  
  if (length(dim(image)) == 3){
    cropped_image <- imsub(image,x %inr% ix, y %inr% iy)}
  
  if (length(dim(image)) == 4){
    cropped_image <- imsub(image,x %inr% ix, y %inr% iy)}
  
  return(cropped_image)
  
}