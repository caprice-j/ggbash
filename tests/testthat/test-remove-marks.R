context("remove-marks")

test_that("remove marks", {
    ee(remove_unnecessary_marks("gg(iris) + p(Sepal.W, y=do(Sepal.L))"),
       "gg iris  + p Sepal.W  y=do(Sepal.L) ")
    ee(remove_unnecessary_marks("gg iris  + p Sepal.W, y=do(Sepal.L) "),
       "gg iris  + p Sepal.W  y=do(Sepal.L) ")
    ee(remove_unnecessary_marks("gg a  + p b, c=do(d, e, f), g"),
       "gg a  + p b  c=do(d, e, f)  g")
})
