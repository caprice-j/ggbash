library(ggbash)
context('ggbash-REPL')

test_that('ggbash', {
    expect_equal(exec_ggbash(iris, 'exit'), TRUE)
    expect_equal(exec_ggbash(iris, 'quit'), TRUE)

    expect_output(exec_ggbash(iris, 'use iris'), 'setosa')
    expect_output(exec_ggbash(iris, 'show iris'), 'setosa')

    expect_message(exec_ggbash(iris, 'echo hi'), 'hi')

    expect_equal(exec_ggbash(iris, 'p 1 2 | copy'), FALSE)
    expect_equal(exec_ggbash(iris, 'p 1 2'), FALSE)
})
