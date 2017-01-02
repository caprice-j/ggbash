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

### Interactive

``` bash
library(ggbash)
ggbash() # start a ggbash session
```

``` bash
gg iris  +  p Sepal.W Sepal.L c=Sp si=Petal.W  | echo
```

``` r
# this is the result of the above 'echo' command
ggplot(iris) +
geom_point(aes(Sepal.Width,
               Sepal.Length,
               colour=Species,
               size=Petal.Width))
```

![](README-example-1.png)

### One-liner

``` r
executed <- ggbash('gg iris + p Sepal.W Sepal.L c=Sp siz=Petal.W')
copy_to_clipboard(executed$cmd)
# copied: ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, colour=Species, size=Petal.Width))
```

Features
--------

### 1. Partial Prefix Match

In the above example (`p Sepal.W Sepal.L c=Sp si=Petal.W`), ggbash performs partial matches seven times.

-   **geom names**
    -   `p` matches `geom_point`
        -   Note: the common prefix geom\_ is removed beforehand for simplicity.
-   **column names**
    -   `Sepal.W` matches `iris$Sepal.Width`.

    -   `Sepal.L` matches `iris$Sepal.Length`.

    -   `Sp` matches `iris$Species`.

    -   `Petal.W` matches `iris$Petal.Width`.

-   **aesthetics names**
    -   `c` matches `colour`, which is the aesthetic of geom\_point.
    -   `si` matches `size` ('s' cannot identify which one of `shape`, `size`, and `stroke`).

Note: Approximate String Match (e.g. identifying `size` by `sz`)is not supported in the current version.

### 2. Predefined Precedence

Even if unique identification is not possible, `ggbash` tries to execute its best guess instead of bluntly returning an error. Everything is designed to achieve least expected keystrokes.

For example, in the input `p Sepal.W Sepal.L c=Sp s=Petal.W`, `p` ambiguously matches four different geoms, `geom_point`, `geom_path`, `geom_polygon`, and `geom_pointrange`.
Among these geoms, `ggbash` determines the geom to use according to the above predefined order of precedence (the first one, `geom_point`, is selected in this example).

Similarly, `s` matches three aesthetics, `size`, `shape`, and `stroke`, preferred by this order. Thus, `s` is executed as the `size` aesthetic.

While it's possible to define your own precedence order through `define_constant_list()`, adding one or two characters may be faster in most cases.

### 3. Column Index Match

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

Column Index Match is perhaps the fastest way to build a ggplot2 object. In the above case, while the normal ggplot2 notation (`ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length, colour=Species, size=Petal.Width))`) contains 90 characters (spaces not counted), `p 2 1 c=5 si=4` is just **10 characters -- more than 80% keystroke reduction**.

With more elaborated plots, the differences become much larger.

### 4. Pipe Operator (`|` and `+`)

#### Adding Layers

While ggplot2 library only interprets `%>%` operator as pipe, ggbash interprets both `+` and `|` symbols as the same pipe operator.

``` r
ggbash('gg mtcars + point mpg wt + smooth mpg wt')
```

![](README-pipe_example-1.png)

<!-- FIXME impelment gg mtcars mpg wt + point + smooth -->
#### Save or Copy Results

``` r
    ggbash()
    user@host currentDir $ cd imageDir

    user@host imageDir $ ls
    /Users/myname/currentDir/imageDir
    
    user@host imageDir $ gg iris | p 2 1 c=5 | png big
    saved as 'iris-150/x-Sepal.Width_y-Sepal.Length-colour-Species.1960x1440.png'
    
    user@host imageDir $ gg iris | p 1 2 col=Sp siz=4 | copy
    copied to clipboard:
    ggplot(iris) + geom_point(aes(x=Sepal.Length,
                                  y=Sepal.Width,
                                  colour=Species,
                                  size=Petal.Width))
```

#### Save PNG Files

`png` could receive plot size and file name. If none specified, the default values are used.

`png` command interprets a single- or double-quoted token as file name ("iris-for" in the following example), and otherwise plot size. `png` is order-agnostic. Both of the following notations generates the same png file whose size is 960 pixels in width and 480 pixels in height.

``` bash
gg iris + p 1 2 | png "my-iris-plot" 960x480    
gg iris + p 1 2 | png 960x480 "my-iris-plot"
```

#### Save PDF Files

<!-- 1 inch == 2.54 cm -->
While the `pdf` function in R only recognizes width and height as inches, the `pdf` command in ggbash recognizes both inches and pixels. **If the given `width` or `height` in `<width>x<height>` is less than 50** (the same limit of `ggplot2::ggsave`) **, the numbers are interpreted as inches (1 inch == 2.54 cm).**

``` bash

# pdf of 15 inch width (=~ 40 cm) and 9 inch height (=~ 23 cm)
gg iris + p 1 2 | pdf 16x9

# pdf of 1440 pixel (=~ 50 cm) width and height
gg iris + p 1 2 | pdf 1440x1440

# the png command in ggbash also recognises inches
gg iris + p 1 2 | png 16x9
gg iris + p 1 2 | pdf 16x9
# both return almost the same output
```

Note: the default dpi in ggbash is 72 (R's default) and cannot be changed. If you would like to change the dpi, you could consider `ggplot2::ggsave` function.

### 5. Auto-generated Filenames

The `png` and `pdf` functions in R save a plot in `Rplot001.{png|pdf}` if no file name is specified. That function can easily overwrite the previous plot, and users often have to set file names manually.

The `png` command in `ggbash` tries to generate a file name based on the given dataset and aesthetic names if no file name is specified.

For example, when you are using the `iris` dataset which has 150 rows, the output of `p Sepal.W Sepal.L | png` is saved in `iris-150/point_x-Sepal.Width_y-Sepal.Length.480x480.png`.

If you happen to have the different `iris` dataset which has a different number of rows (say 33), the same command result is saved in `iris-33/` directory.

<!-- ### 6. Type-specific For Loop (To Be Implemented) -->
<!-- ```{r, eval=FALSE} -->
<!-- # to be implemented -->
<!-- > str(iris) -->
<!-- 'data.frame':  150 obs. of  5 variables: -->
<!--  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ... -->
<!--  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ... -->
<!--  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ... -->
<!--  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ... -->
<!--  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ... -->
<!-- # a : use all five variables (numeric and factor) -->
<!-- user@host imageDir (iris) $ for a in 2:5 point 1 a | pdf -->
<!-- saved as 'iris-150/x-Sepal.Width_y-fora.pdf' (960 x 960) -->
<!-- # a : for loop on four numeric (integer) variables -->
<!-- user@host imageDir (iris) $ for i in 2:5 point 1 i | pdf -->
<!-- saved as 'iris-150/x-Sepal.Width_y-fori.pdf' (960 x 960) -->
<!-- ``` -->
<!-- You might have met `non-numeric argument to 'pairs()'` error. -->
<!-- When you try to generate tens or hundreds of plots, usually -->
<!-- a type-specific error occurs and users are required to subset -->
<!-- dataset columns by types. -->
<!-- In `ggbash`, the for-loop variable expresses its type as follows: -->
<!-- +  `for a in 1:5` : **A**ll five variables will be iterated. -->
<!-- +  `for c in 1:5` : **C**haracter variables among five columns will be iterated. -->
<!-- +  `for f in 1:5` : **F**actor variables among five columns will be iterated. -->
<!-- +  `for i in 1:5` : **I**nteger variables among five columns will be iterated. -->
<!-- +  `for n in 1:5` : **N**umeric variables among five columns will be iterated. -->
<!-- (Note: boolean variables will also be iterated in `i` and `n` cases.) -->
<!-- #### TODO how can we encode scales/facets/themes differences? -->
<!-- Scales are ... -->
<!-- Facets are ... -->
<!-- for is by pdf extension and i values -->
<!-- Themes are abstracted away and not encoded in file names. -->
Goals
-----

ggbash has two main goals:

1.  **Better EDA experience.** Provide blazingly fast way to do Exploratory Data Analysis.

    -   less typing by Column Index Match, Partial Prefix Match, and Predefined Precedence.

    -   casualy save plots with Auto-generated Filenames.

2.  **Intuitive tweaking.** Make it less stressful to finalize your plots.

    -   adjust colours or lineweights

    -   rotate axis labels

    -   decide tick label intervals and limits

    -   generate line-wrapped titles or legends

Learning ggbash
---------------

`ggbash` follows ggplot2 notations as much as possible for reducing learning costs of current ggplot2 users.

Learning ggplot2 might be the best way to understand ggbash notations. The [document](http://docs.ggplot2.org/current/) and [book](https://github.com/hadley/ggplot2-book) of ggplot2 would be helpful.

The [vignette](https://github.com/caprice-j/ggbash/blob/master/vignettes/Introduction-to-ggbash.Rmd) of ggbash is still in a draft.
