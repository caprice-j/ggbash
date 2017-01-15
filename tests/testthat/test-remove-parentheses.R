context("remove-paren")

test_that("remove paren", {
    ee(remove_unnecessary_parentheses("gg(iris) + p(Sepal.W, y=do(Sepal.L))"),
       "gg iris  + p Sepal.W, y=do(Sepal.L) ")
    ee(remove_unnecessary_parentheses("gg iris  + p Sepal.W, y=do(Sepal.L) "),
       "gg iris  + p Sepal.W, y=do(Sepal.L) ")
})
