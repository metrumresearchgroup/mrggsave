
Setup
=====

``` r
library(mrggsave)
library(ggplot2)

dir <- "inst/img"
Script <- "README.Rmd"

x <- runif(1000, 10,100)
y <- 10 + 0.2*x + rnorm(length(x),1)
data <- data.frame(x = x, y = y)
```

Plots get drawn with annotations
================================

``` r
p <- ggplot(data = data, aes(x,y)) + geom_point() +
  xlab("The x-axis title here (units)")

mrggdraw(p, Script, "ggplot")
```

![](inst/img/readme-unnamed-chunk-3-1.png)

And save with annotation
------------------------

``` r
mrggsave(p, Script, "myggplot", width = 5, height = 4, dir = dir)
```

GGally - draw and save
======================

``` r
library(GGally)
p2 <- ggpairs(data)
mrggsave(p2, Script, "myggpairsplot", dir = dir)
```

The saved plot can be seen [here](inst/img/myggpairsplot.pdf) and will look like:

``` r
mrggdraw(p2, Script,  "myggpairsplot", 
         dir = dir, 
         save = TRUE)
```

![](inst/img/readme-unnamed-chunk-6-1.png)

Arrange plots on a page
=======================

The saved plot can be seen [here](inst/img/arranged.pdf) and will look like:

``` r
mrggdraw(list(p,p,p), Script,  "arranged", 
         arrange = TRUE, ncol = 3, dir = dir, 
         save = TRUE)
```

![](inst/img/readme-unnamed-chunk-7-1.png)
