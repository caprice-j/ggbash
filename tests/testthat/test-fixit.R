context("fixit")

test_that("fixit column name", {

    expect_equal(
        bash("gg iris + point Sepal.W Sepal.L + theme txt size=1"),
        "ggplot(iris) + geom_point(aes(x=Sepal.Width, y" %++%
            "=Sepal.Length)) + theme(text = element_text(size=1))"
    )

    expect_message(
        gbash("gg iris + point Sepal.Wd Sepal.L"),
        "Column name not found"
    )

})
