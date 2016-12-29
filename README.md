<!-- README.md is generated from README.Rmd. Please edit that file -->
ggbash
======

ggbash provides a bash-like REPL environment for ggplot2.

-   partial match

In ggbash session, almost everything can be specified by partial match.

Basic Usage
-----------

``` bash
library(ggbash)
ggbash(iris) # use iris as main dataset
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

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

Features
--------

### 1. Column Index Match

``` bash
# 'ls' checks column indices
user@host currentDir (iris) $ ls

    1: Sepal.L  (ength)
    2: Sepal.W  (idth)
    3: Petal.L  (ength)
    4: Petal.W  (idth)      
    5: Spec     (ies)

# the same as the above 'p Sepal.W Sepal.L c=Sp si=Petal.W'
user@host currentDir (iris) $ p 2 1 c=5 si=4
```

### 2. Partial Match

In the first example, ggbash performs partial matches seven times.

-   **geom names**
    -   `p` matches to `geom_point`.
-   **column names**
    -   `Sepal.W` matches to `iris$Sepal.Width`.

    -   `Sepal.L` matches to `iris$Sepal.Length`.

    -   `Sp` matches to `iris$Species`.

    -   `Petal.W` matches to `iris$Petal.Width`.

-   **aesthetics names**
    -   `c` matches to `color`, which is the aesthetic of geom\_point.
    -   `si` matches to `size` ('s' is ambiguous between 'shape' and 'size').

### 3. Save results

``` r
    user@host currentDir (iris) $ cd imageDir

    user@host imageDir (iris) $ p 2 1 c=5 si=4 | png big
    saved as 'iris-Sepal.W-Sepal.L-Sp.png' (1960 x 1440)
    
    user@host imageDir (iris) $ for (i in 2:5) p 1 i | pdf --name iris-for
    saved as 'iris-for.pdf' (960 x 960)
    # TBI
```
