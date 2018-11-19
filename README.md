# mammalsmilk

The goal of mammalsmilk is to replicate the analyses of Skibiel et al 2013  "The evolution of the nutrient composition of mammalian milks" and illustrate regression modeling techinques using these data.

## Installation

You can install the developement version of mammals milk from GitHub using the devtools package

``` r
#install devtools if needed
## install.packages("devtools")

#load devtools
library(devtools)

#download from github
dvetools::install_github("brouwern/mammalsmilk")

#load into R
library(mammalsmilk)
```

## Example

mammalsmilk contains data by Skibiel et al (2013) on the relationship between various life history and morphological traits and milk composition.  For example, carnivores produce milk with a much higher fat content than herbivores and omnivores.

``` r
library(ggpubr)
data(milk)
ggboxplot(data = milk,
y = "fat", 
x ="diet",
fill = "diet")
```

