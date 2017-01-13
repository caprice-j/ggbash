`%>>%` <- function(e1, e2, ...) #ggbash(substitute(e2))

#`%>>%` <- function(e1, ...) print(substitute(...))#ggbash(substitute(e2))


`%>>%` <- function(e1, ...) {
    print(e1)
    #sapply(list(substitute(...)), function(x) { substitute(x)})
    for (i in 1:2) {
        print(substitute(...[[i]]))
    }
}

gggbash <- function(x) {
    substitute(x)
}

gggbash(gg(iris) + point(x=, y=Sepal.L) + theme(text(size=20), l.t.x(size=15)))

