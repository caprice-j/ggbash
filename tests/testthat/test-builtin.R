library(ggbash)
context('ggbash-builtin')

test_that('builtins', {
    e <- execute_ggbash_builtins
    const <- define_constant_list()
    oldwd <- getwd()
    # Can I assume some particular directory here?
    expect_message(e(  'pwd',   'pwd', const, iris))
    expect_message(e('getwd', 'getwd', const, iris))

    expect_message(e('list', 'list', const, iris))
    expect_message(e( 'str',  'str', const, iris))

    expect_message(e( 'ls',  'ls', const, iris))
    expect_message(e('dir', 'dir', const, iris))

    expect_error(e(   'cd',    'cd', const, iris), regexp=NA) # expect no error
    expect_error(e('setwd', 'setwd', const, iris), regexp=NA)
    expect_error(e('cd ..', c('cd', '..'), const, iris), regexp=NA) # expect no error
    setwd(oldwd)

    expect_message(e('echo hello', c('echo', 'hello'), const, iris), 'hello')
    expect_message(e('print hero', c('echo', 'hero'),  const, iris), 'hero')
})
