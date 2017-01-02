library(ggbash)
context('ggbash-REPL')

test_that('ggbash', {
    expect_equal(exec_ggbash('exit'), TRUE)
    expect_equal(exec_ggbash('quit'), TRUE)
    expect_equal(exec_ggbash('q'), TRUE)

    expect_output(exec_ggbash('show iris'), 'setosa')

    expect_message(exec_ggbash('gg iris | p 1 2 | echo'), 'geom_point')
    expect_message(exec_ggbash('echo hi'), 'hi')

    expect_equal(exec_ggbash('gg iris | p 1 2 | copy'), FALSE)
    expect_equal(exec_ggbash('gg iris | p 1 2'), FALSE)
})
