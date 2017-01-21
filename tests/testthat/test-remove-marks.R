context("remove-marks")

test_that("remove marks", {
    ee(remove_unnecessary_marks("gg(iris) + p(Sepal.W, y=do(Sepal.L))"),
       "gg iris  + p Sepal.W  y=do(Sepal.L) ")
    ee(remove_unnecessary_marks("gg iris  + p Sepal.W, y=do(Sepal.L) "),
       "gg iris  + p Sepal.W  y=do(Sepal.L) ")
    ee(remove_unnecessary_marks("gg a  + p b, c=do(d, e, f), g"),
       "gg a  + p b  c=do(d, e, f)  g")
})

test_that("remove_aes", {
    ee(remove_aes("gg(a) + p(x,y,z)"), "gg(a) + p(x,y,z)")
    ee(remove_aes("gg(a) + p(aes(x,y),c)"), "gg(a) + p(x,y,c)")
    ee(remove_aes("gg(a) + p(aes (x,y),c)"), "gg(a) + p( x,y,c)")
    ee(remove_aes("gg(a) + p(aes  (x,y),c)"), "gg(a) + p(  x,y,c)")
    ee(remove_aes("gg(a) + p(aes(x,y),c) + p(aes(l),d)"),
       "gg(a) + p(x,y,c) + p(l,d)")
    ee(remove_aes("gg(a) + p(aes(x,y),c) + p(aes(aes_dummy,l),d)"),
       "gg(a) + p(x,y,c) + p(aes_dummy,l,d)")


})
