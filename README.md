<!-- README.md is generated from README.Rmd. Please edit that file -->
ggbash: A Faster Way to Write ggplot2
=====================================

[![Travis-CI Build Status](https://travis-ci.org/caprice-j/ggbash.svg?branch=master)](https://travis-ci.org/caprice-j/ggbash) [![Build status](https://ci.appveyor.com/api/projects/status/vfia7i1hfowhpqhs?svg=true)](https://ci.appveyor.com/project/caprice-j/ggbash) [![codecov](https://codecov.io/gh/caprice-j/ggbash/branch/master/graph/badge.svg)](https://codecov.io/gh/caprice-j/ggbash) <!-- [![Coverage Status](https://coveralls.io/repos/github/caprice-j/ggbash/badge.svg)](https://coveralls.io/github/caprice-j/ggbash) --> [![Issue Count](https://codeclimate.com/github/caprice-j/ggbash/badges/issue_count.svg)](https://codeclimate.com/github/caprice-j/ggbash/issues)

ggbash provides a bash-like REPL environment for [ggplot2](https://github.com/tidyverse/ggplot2).

Installation
------------

``` r
devtools::install_github("caprice-j/ggbash")
```

Usage
-----

``` bash
library(ggbash)
ggbash(iris) # start a ggbash session (using iris as main dataset)
```

``` bash
user@host currentDir (iris) $ p Sepal.W Sepal.L c=Sp si=Petal.W
```

``` r
executed:
    ggplot(iris) +
    geom_point(aes(Sepal.Width,
                   Sepal.Length,
                   color=Species,
                   size=Petal.Width))
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

Or if you just need one figure,

``` r
executed <- drawgg(iris, 'p Sepal.W Sepal.L c=Sp siz=Petal.W')
copy_to_clipboard(executed$cmd)
# copied: ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, colour=Species, size=Petal.Width))
```

Features
--------

### 1. Column Index Match

``` bash
# 'list' displays column indices
user@host currentDir (iris) $ list

    1: Sepal.L  (ength)
    2: Sepal.W  (idth)
    3: Petal.L  (ength)
    4: Petal.W  (idth)      
    5: Spec     (ies)

# the same as the above 'p Sepal.W Sepal.L c=Sp si=Petal.W'
user@host currentDir (iris) $ p 2 1 c=5 si=4

# you can mix both notations
user@host currentDir (iris) $ p 2 1 c=5 si=Petal.W
```

Column Index Match is perhaps the fastest way to build a ggplot object. In the above case, while the normal ggplot2 notation (`ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, colour=Species, size=Petal.Width))`) contains 90 characters (spaces not counted), `p 2 1 c=5 si=4` is just **10 characters -- more than 80% keystroke reduction**.

With more elaborated plots, the differences become much larger.

### 2. Partial Prefix Match

In the first example (`p Sepal.W Sepal.L c=Sp si=Petal.W`), ggbash performs partial matches seven times.

-   **geom names**
    -   `p` matches `geom_point`
        -   Note: the common prefix geom\_ is removed beforehand.
-   **column names**
    -   `Sepal.W` matches `iris$Sepal.Width`.

    -   `Sepal.L` matches `iris$Sepal.Length`.

    -   `Sp` matches `iris$Species`.

    -   `Petal.W` matches `iris$Petal.Width`.

-   **aesthetics names**
    -   `c` matches `color`, which is the aesthetic of geom\_point.
    -   `si` matches `size` ('s' cannot identify which one of `shape`, `size`, and `stroke`).

Note: Approximate String Match (e.g. identifying `size` by `sz`)is not supported in the current version.

### 3. Predefined Precedence

Even if unique identification is not possible, `ggbash` tries to guess what is specified instead of bluntly returning an error, hoping to achieve least expected keystrokes.

For example, in the input `p Sepal.W Sepal.L c=Sp s=Petal.W`, `p` ambiguously matches four different geoms, `geom_point`, `geom_path`, `geom_polygon`, and `geom_pointrange`.
Among these geoms, `ggbash` determines the geom to use according to the above predefined order of precedence (the first geom, `point`, is selected in this example).

Similarly, `s` matches three aesthetics, `size`, `shape`, and `stroke`, preferred by this order. Thus, `s` is interpreted as the `size` aesthetic.

While it's possible to define your own precedence order through `define_constant_list()`, adding one or two characters might be faster in most cases.

### 4. Piping to copy/save results

``` r
    user@host currentDir (iris) $ cd imageDir

    user@host currentDir (iris) $ ls
    /Users/myname/currentDir/imageDir
    
    user@host imageDir (iris) $ p 2 1 c=5 si=4 | png big
    saved as 'iris-Sepal.W-Sepal.L-Sp.png' (big: 1960 x 1440)
    
    user@host imageDir (iris) $ p 1 2 col=Sp siz=4 | copy
    copied to clipboard:
    ggplot(iris) + geom_point(aes(x=Sepal.Length,
                                  y=Sepal.Width,
                                  colour=Species,
                                  size=Petal.Width))
```

`png` can receive three arguments: plot size, file name, and resolution (pixels per inch). If none specified, the default values are used.

`png` command interprets a single- or double-quoted token as file name ("iris-for"), a single number as resolution, and otherwise plot size. `png` is order-agnostic. In other words, any one of the following notations generates the same png file.

`p 1 2 | png "my-iris-plot" big 50` `p 1 2 | png big 50 "my-iris-plot"` `p 1 2 | png big "my-iris-plot" 50`

### 5. Auto-assigned Filenames

The `png` function in R saves a plot in `Rplot001.png` if no file name is specified. That function can easily overwrite the previous plot, and users often set file names manually.

The `png` command in `ggbash` tries to generate a file name based on the given dataset and aesthetic names if no file name is specified.

For example, when you are using the `iris` dataset which has 150 rows, the output of `p Sepal.W Sepal.L | png` is saved in `iris-150/point_x-Sepal.Width_y-Sepal.Length.480x480.72.png` (72 is the default pixels per inch in R).

If you happen to have the different `iris` dataset which has a different number of rows (say 33), the same command result is saved in `iris-33/` directory.

### 6. Type-specific For Loop (To Be Implemented)

``` r
# to be implemented

> str(iris)
'data.frame':   150 obs. of  5 variables:
 $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
 $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
 $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
 $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
 $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

# a : use all five variables (numeric and factor)
user@host imageDir (iris) $ for a in 2:5 point 1 a | pdf
saved as 'iris-150/x-Sepal.Width_y-fora.pdf' (960 x 960)

# a : for loop on four numeric (integer) variables
user@host imageDir (iris) $ for i in 2:5 point 1 i | pdf
saved as 'iris-150/x-Sepal.Width_y-fori.pdf' (960 x 960)
```

You might have met `non-numeric argument to 'pairs()'` error. When you try to generate tens or hundreds of plots, usually a type-specific error occurs and users are required to subset dataset columns by types.

In `ggbash`, the for-loop variable expresses its type as follows:

-   `for a in 1:5` : **A**ll five variables will be iterated.
-   `for c in 1:5` : Among five columns, only **c**haracter variables will be iterated.
-   `for f in 1:5` : Among five columns, only **f**actor variables will be iterated.
-   `for i in 1:5` : Among five columns, only **i**nteger variables will be iterated.
-   `for n in 1:5` : Among five columns, only **n**umeric variables will be iterated.

(Note: boolean variables will also be iterated in `i` and `n` cases.)

#### TODO how can we encode scales/facets/themes differences?

Scales are ... Facets are ... for is by pdf extension and i values Themes are abstracted away and not encoded in file names.

Goals
-----

ggbash has two main goals:

1.  *Better EDA experience.* Provide blazingly fast way to do Exploratory Data Analysis.

    -   less typing by Column Index Match, Partial Prefix Match, and Predefined Precedence.

    -   casualy save plots with auto-generated unique file names

2.  *Faster tweaking.* Make it less stressful to finalize your plots.

    -   adjust colors or lineweights

    -   rotate axis labels

    -   decide tick label intervals and limits

    -   generate line-wrapped titles or legends

Learning ggbash
---------------

`ggbash` follows ggplot2 notations as much as possible for reducing learning costs of current ggplot2 users.

Therefore, these [document](http://docs.ggplot2.org/current/) and [book](https://github.com/hadley/ggplot2-book) are good ways to get the hang of ggplot2.

The [vignette](https://github.com/caprice-j/ggbash/blob/master/vignettes/Introduction-to-ggbash.Rmd) of ggbash is still a draft.
