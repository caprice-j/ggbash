context("fixit")

test_that("fixit column name", {

    expect_message(
        gbash("gg iris + point Sepal.W Sepal.L + theme txt: size=1"),
        "Prefix match for theme element name failed"
    )

    expect_message(
        gbash("gg iris + point Sepal.Wd Sepal.L"),
        "Column name not found"
    )

})

