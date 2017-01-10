library(ggbash)
context("ggbash-builtin")

test_that("builtins", {
    e <- execute_ggbash_builtins
    const <- define_ggbash_constants()
    oldwd <- getwd()
    rmdir <- function(dir) unlink(dir, recursive = TRUE)
    # Can I assume some particular directory here?
    expect_message(e(  "pwd",   "pwd", const))
    expect_message(e("getwd", "getwd", const))

    gibberish <- "badcfeghzfumfduqkm"
    unlink(gibberish)
    expect_error(e(paste0("mkdir ", gibberish),
                   c("mkdir", gibberish), const), regexp = NA)
    rmdir(gibberish)
    expect_error(e(paste0("dir.create ", gibberish),
                   c("dir.create", gibberish), const), regexp = NA)
    expect_error(e(paste0("rm ", gibberish),
                 c("rm", gibberish), const), "this is a directory")
    rmdir(gibberish)

    file.create(gibberish)
    expect_error(e(paste0("rmdir ", gibberish),
                   c("rmdir", gibberish), const), "this is not a directory")
    unlink(gibberish)

    expect_message(e("list iris", c("list", "iris"), const))
    expect_message(e("str iris", c("str", "iris"), const))

    expect_message(e("ls", "ls", const))
    expect_message(e("dir", "dir", const))

    expect_error(e("cd", "cd", const), regexp = NA)
    expect_error(e("setwd", "setwd", const), regexp = NA)
    expect_error(e("cd ..", c("cd", ".."), const), regexp = NA)
    setwd(oldwd)
})
