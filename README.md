
<!-- README.md is generated from README.Rmd. Please edit that file -->

# segment_me_label_free

<!-- badges: start -->
<!-- badges: end -->

The goal of segment_me_label_free is to quantitate cell numbers from
high-content microscope images using only the image. We implement this
for Bright-field label-free images and it does not require a training
dataset.

To run the pipeline, we have a Shiny app that loads the bright field and
is used to establish the cell size. This is important, because cell size
is the variable that is used to optimize the parameter automation. We
also have a function that takes as input the image and the cell size.
The function returns the cell number and their xy position

In the github, we also have all code organized in quarto note book that
generates all data plots and tables from the paper

## installation

Install all the below packages to be able to run the functions and
regenerate the plots

``` r

#required image analysis packages
install.packages("BiocManager")
BiocManager::install("EBImage")
install.packages(c("imager", "imagerExtra", "magick"))

#required data stats and plotting packages
install.packages(c("spatstat", "deldir",  "mclust"))

#required standard tidy and base R packages
install.packages(c("pipeR", "tidyverse"))
install.packages(c("furrr", "future"))
install.packages("fs")
install.packages("here")

#used additionally in paper data figures and notebook
install.packages("geomtextpath", )
install.packages("gt")
install.packages("ggdist", "ggforce", "dbscan")
install.packages("jsonlite")
install.packages("NCmisc")
install.packages("devtools")
devtools::install_github("inbo/inborutils")

#required for shiny app
install.packages(c("sf", "bslib", "shiny", "shinyWidgets"))

#`r sprintf("![shiny_workflow](%s)", shiny_example.png)`
```

## shiny

Open the shiny app in Rstudio, by opening the app.R file and click
`Run App`. You can upload your image and follow the three steps as
described in the figure below

<figure>
<img src="shiny_example.png" alt="screenshot" />
<figcaption aria-hidden="true">screenshot</figcaption>
</figure>

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->
