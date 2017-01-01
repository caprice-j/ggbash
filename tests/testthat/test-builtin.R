library(ggbash)
context('ggbash-builtin')

test_that('builtins', {
    e <- execute_ggbash_builtins
    const <- define_constant_list()
    oldwd <- getwd()
    rmdir <- function(dir) unlink(dir, recursive = TRUE)
    # Can I assume some particular directory here?
    expect_message(e(  'pwd',   'pwd', const, iris))
    expect_message(e('getwd', 'getwd', const, iris))

    gibberish <- 'badcfeghzfumfduqkm'
    unlink(gibberish)
    expect_error(e(paste0('mkdir ', gibberish),
                   c('mkdir', gibberish), const, iris), regexp=NA)
    rmdir(gibberish)
    expect_error(e(paste0('dir.create ', gibberish),
                   c('dir.create', gibberish), const, iris), regexp=NA)
    expect_error(e(paste0('rm ', gibberish),
                 c('rm', gibberish), const, iris), 'this is a directory')
    rmdir(gibberish)

    file.create(gibberish)
    expect_error(e(paste0('rmdir ',gibberish),
                   c('rmdir', gibberish), const, iris), 'this is not a directory')
    unlink(gibberish)

    expect_message(e('list', 'list', const, iris))
    expect_message(e( 'str',  'str', const, iris))

    expect_message(e( 'ls',  'ls', const, iris))
    expect_message(e('dir', 'dir', const, iris))

    expect_error(e(   'cd',    'cd', const, iris), regexp=NA) # expect no error
    expect_error(e('setwd', 'setwd', const, iris), regexp=NA)
    expect_error(e('cd ..', c('cd', '..'), const, iris), regexp=NA) # expect no error
    setwd(oldwd)
})
