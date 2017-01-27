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

test_that("replace_plus", {
    ee(replace_plus("gg(x)+p(a+b)"), "gg(x)##p(a+b)")
    ee(replace_plus("gg(x) + p(a+b)"), "gg(x) ## p(a+b)")
    ee(replace_plus("gg(x) + p(a + b)"),  "gg(x) ## p(a + b)")
    ee(replace_plus("gg(x) + p(a+b) + p(a+b) + p(a+b)"),
       "gg(x) ## p(a+b) ## p(a+b) ## p(a+b)")

    ee(replace_plus("gg x + rect x=a+b y=c+d+e"), "gg x ## rect x=a+b y=c+d+e")
})

test_that("remove_element_whatever", {
    ee(remove_element_whatever("theme(l=element_text(size=20, face='bold'))"),
       "theme(l(size=20, face='bold'))"
    )
    ee(remove_element_whatever("theme(l=element_rect(size=20, face='bold'))"),
       "theme(l(size=20, face='bold'))"
    )
    ee(remove_element_whatever("theme(l=element_line(size=20, face='bold'))"),
       "theme(l(size=20, face='bold'))"
    )
    ee(remove_element_whatever("theme(l=element_blank())"),
       "theme(l())"
    )
    # need a test for element_grob()? I haven't used it before
})

test_that("remove_theme_unit", {
    ee(remove_theme_unit("theme(a=unit(.5, 'cm'))"), "theme(a(.5, cm))")
})
