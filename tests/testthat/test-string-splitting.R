library(ggbash)
context('string-splitting')

test_that('', {

    expect_equal(
        split_by_pipe(input='point 1 2 color=3 | copy'),
        c('point 1 2 color=3 ', ' copy')
    )

    expect_equal(
        split_by_space(input='point 1 2 color=3'),
        c('point', '1', '2', 'color=3')
    )

    expect_equal(
        split_by_space(input='  point 1 2 color=3   '),
        c('point', '1', '2', 'color=3')
    )

})
