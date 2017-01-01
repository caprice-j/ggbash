library(ggbash)
context('find-first-partial-match')

test_that('find_first', {

    f <- find_first

    expect_error(f('a', c(     'b', 'c')), 'no such prefix: a')
    expect_equal(f('a', c('a', 'b', 'c')), 1)
    expect_warning(f('a', c('a', 'aa', 'c')), 'Ambiguous match. Use "a" among a, aa')

})