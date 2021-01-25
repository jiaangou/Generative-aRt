
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Generative-aRt

<!-- badges: start -->
<!-- badges: end -->

This repository is my attempt at generative art. Very much still a
working progress, check back in 30 years when I’m much more skilled💩

## 1. Random walk

-   Motivation: to create a hex sticker image for
    [PalCreatoR](https://github.com/GenChangHSU/PalCreatoR)
-   I first used
    [`create_pal`](https://genchanghsu.github.io/PalCreatoR/reference/create_pal.html)
    to generate color for 2 separate images, which I will use to color
    the paths of my random walk
-   The random walk algorithm I wrote is really simple:
    1.  Setup starting points by drawing randomly from normal
        distribution 3 times for each of the x, y, z axes
    2.  Step 1 is repeated, depending on how many **“walks”** one wishes
        to make
    3.  Every **step** has a total of 3 possibilities: i) move back
        (-1), ii) don’t move (0), or iii) move forward (+1) and the
        probability for any one of the 3 possibility is the same. So
        steps are drawn from this pool of possibilities with the number
        of times equal to how many **steps** one wishes to take
    4.  Just to make it more fun, one can modify the **step size** that
        each walk can take by drawing from either a uniform distribution
        or exponential distribution
    5.  For demonstration, I created 2 separate walking scenarios, where
        one set of walks has more **“more walks”** and **“less steps”**
        and another with “less walks” and “more steps”
    6.  Each set of walks was color coded by each of the color palette,
        with each walk within each set assigned randomly to one of the
        colors in the palette
-   [script](https://github.com/jiaangou/Generative-aRt/blob/master/random-walk-art.R)

``` r
magick::image_read('random-walk-static.jpg')
```

<img src="README_files/figure-gfm/unnamed-chunk-1-1.png" width="5669" />

``` r
magick::image_read('random-walks.gif')
```

![](README_files/figure-gfm/unnamed-chunk-2-1.gif)<!-- -->
