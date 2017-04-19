
<!-- README.md is generated from README.Rmd. Please edit that file -->
IOSlides Plus
=============

The goal of **iosp** is to extends [ioslides\_presentation](http://rmarkdown.rstudio.com/ioslides_presentation_format.html) with a custom `css` and a fixed prettyprint javascript

The main **features** are:

-   easily create boxes with/out titles in the [bootstrap](http://getbootstrap.com/) fashion using the third level `###`
-   create your own background colors for boxes filling
-   align top / middle / bottom adjacent boxes
-   offset boxes to center or justify along the slide' width
-   prettier code highlighting
-   easy footer

Installation
------------

You can install iosp from github with:

``` r
# install.packages("devtools")
devtools::install_github("koncina/iosp")
```

Demo template
-------------

In **Rstudio**, create a new `Rmd` &gt; From template &gt; IOSlides Plus

The following `Rmd` code

    ## Slide with columns as boxes

    Just a proof of concept. I changed the original lua file from `ioslides_presentation`.

    ### Box 1{.col-4 .box .bg-red}

    Test 1

    ### Box 2{.col-6 .box .bg-green}

    - line 1
    - line 2

    ### Box 3{.col-6 .box .bg-blue}

    This box has again a width of 6 col: it will be rendered in a new row

    %end%

    We are able to exit the box and row using the keyword `%end%` as suggested [here](https://github.com/koncina/iosp/issues/5).

Give this slide output:

![](demo.png)

Custom colors
-------------

see example with [box\_colour](https://koncina.github.io/iosp/box_colours.html)

Box colours gallery
-------------------

see example with [box\_colour\_gallery](https://koncina.github.io/iosp/box_colours_gallery.html)
