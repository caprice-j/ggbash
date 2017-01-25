context("suffix2geom")

test_that("geom related", {
    # all geoms including not documented ones (like geom_col)
    geoms <- ls(pattern = "^geom_", envir = asNamespace("ggplot2"))
    for (g in gsub("geom_", "", geoms))
        if (g != "map")
            ee(suffix2geom(g),
                eval(parse(text=paste0("ggplot2::geom_",g,"()"))))

    g <- ggplot2::geom_text()
    target <- "text"

    ee(names(g$geom_params), get_geom_params(target))
    ee(g$geom$required_aes, get_required_aes(target))
    # FIXME write tests for get_possible_aes (in a clean way)

})
