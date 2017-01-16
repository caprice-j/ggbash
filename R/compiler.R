

# CONSTAES : Constant Aesthetics
# CHARAES : Character Aesthetics
GGPLOT2_TOKENS <- c("GGPLOT", "NAME", "CONSTAES",
                    "CHARAES", "THEME", "LAYER",
                    "BOOLEAN", "QUOTED", "UNIT",
                   "BOOLEANAES")
# SCALE "ScaleDiscrete" "Scale"         "ggproto"
# GEOM/STAT "LayerInstance" "Layer"         "ggproto"
# COORD "CoordCartesian" "Coord"          "ggproto"
# FACET "FacetGrid" "Facet"     "ggproto"
# labs ?
# POSITION  "PositionDodge" "Position"      "ggproto"
# THEME "theme" "gg"
GGPLOT2_LITERALS <- c() # needed?
GGPLOT2INVALIDTOKEN <- " <<INVALID_TOKEN_HERE>> "

# MAYBE-LATER don't know how to pass variables between yacc's production rules
ggbashenv <- new.env() # Note: This is a global variable.

ggregex <- list(
    plus_pipe  = "(\\+|\\|)\\s*",
    quoted     = paste0("('|\\\")",                      # start from a quote
                        "[a-zA-Z0-9\\._\\+\\-\\*\\/\\^ ]+",
                        "('|\\\")"),                      # end by a quote
    booleanaes = paste0("[a-zA-Z_][a-zA-Z_0-9\\.]\\s*=\\s*",
                        "(TRUE|FALSE|true|false|True|False)"),
    boolean    = "^(TRUE|FALSE|T|F|t|f|true|false|True|False)$",
    charaes    = paste0("[a-z]+=('|\\\").*?('|\\\")"),
    constaes   = "[a-z]+=c\\([0-9\\.,\\)]+", # FIXME adhoc for binw=c(.1, .1)
    unit       = "[0-9\\.]+\\s*(cm|in|inch|inches)",
    data       = "data="
)

set_ggbashenv_warning <- function(){
    if (is.null(ggbashenv$show_amb_warn))
        ggbashenv$show_amb_warn <- TRUE
}

Ggplot2Lexer <-
    R6::R6Class(
        "Lexer",
        public = list(
            tokens = GGPLOT2_TOKENS,
            literals = GGPLOT2_LITERALS,
            #states = list(c('ggplot')),
            # Note: t_(function) defines precedences implicitly
            t_GGPLOT = function(
            re = "^(g|gg|ggp|ggpl|ggplo|ggplot)\\s+[a-zA-Z_\\.][a-zA-Z_0-9\\.]*",
            t) {
                t$value <- gsub("^g(g|gp|gpl|gplo|gplot)?\\s*",
                                "ggplot2::ggplot(",
                                t$value)
                return(t)
            },
            t_CONSTAES = function(re="[a-z]+\\s*=\\s*-*[0-9\\./\\*-\\+:]*[0-9]", t) {
                if (grepl("^group=", t$value)) {
                    t$type <- "NAME"
                    # aes(group=1)
                    return(t)
                }

                # last [0-9] is needed to interpret
                # size=7 + theme as "size=7" and "+ theme"
                return(t) # integers and floats
            },
            # I believe CONSTAES cannot contain +-*/^, because
            # gg iris + point Sepal.W Sepal.L size=4 + smooth colour="red"
            # will be interpreted as
            # LexToken(CHARAES,colour="blue" size=4 + smooth colour="red",1,33)
            # MAYBE LATER default arguments of functions cannot accept
            # global variables as defaults?
            # ggregex$charaes is falsely evaluated as empty string
            t_CHARAES = function(re="[a-z]+\\s*=\\s*('|\\\").*?('|\\\")", t) {
                return(t)
            },
            t_NAME      = function(re="(\\\"|')?[\\.a-zA-Z0-9_\\(\\)][a-zA-Z_0-9\\.,=\\(\\)]*(\\\"|')?(\\s*inches|\\s*inch|\\s*in|\\s*cm)?", t) {

                if (grepl(ggregex$data, t$value)) {
                    dbgmsg("  t_NAME: DATA ", t$value)
                    t$type <- "CONSTAES"
                } else if (grepl(ggregex$booleanaes, t$value)){
                    dbgmsg("  t_NAME: BOOLEANAES ", t$value)
                    t$type <- "BOOLEANAES"
                } else if (grepl(ggregex$constaes, t$value)) {
                    dbgmsg("  t_NAME: CONSTAES ", t$value)
                    t$type <- "CONSTAES"
                } else if (grepl(ggregex$boolean, t$value)) {
                    dbgmsg("  t_NAME: BOOLEAN ", t$value)
                    t$type <- "BOOLEAN"
                } else if (grepl(ggregex$unit, t$value)) {
                    dbgmsg("  t_NAME: UNIT ", t$value)
                    t$type <- "UNIT"
                    # ex. LexToken(UNIT,.20 cm,1,50)
                } else if (grepl(ggregex$quoted, t$value)) {
                    dbgmsg("  t_NAME: QUOTED ", t$value)
                    t$type <- "QUOTED"
                } else {
                    dbgmsg("  t_NAME: ", t$value)
                }
                return(t)
            },
            #t_LPAREN  = '\\(',
            #t_RPAREN  = '\\)',
            #t_COMMA = ',',
            t_THEME = "(\\+|\\|)\\s*theme", # t_THEME is preferred to t_LAYER
            t_LAYER = function(re="(\\+|\\|)\\s*[a-z_2]+", t) {
                # "2" for bin2d
                # TODO missing geom handling here
                if (grepl("(\\+|\\|)\\s*theme", t$value)) {
                    t$type <- "THEME"
                    return(t)
                }
                partial <- gsub(paste0(ggregex$plus_pipe, "(geom_)?"),
                                "",
                                t$value)
                ggbashenv$const <- define_ggbash_constants()
                set_ggbashenv_warning()
                gv <- ggbashenv$const$geom_namev
                geom_sth <- gv[find_first_by_prefix(partial, gv,
                                                    ggbashenv$show_amb_warn)]

                t$value <- paste0(" + geom_", geom_sth)
                return(t)
            },
            t_ignore = " \t",
            t_newline = function(re="\\n+", t) {
                t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
                return(NULL)
            },
            t_error = function(t) {
                cat(sprintf("Illegal character '%s'", t$value[1]))
                t$lexer$skip(1)
                return(t)
            }))

dbgmsg <- function(...) {
    if (exists("ggbash_debug"))
        message(...)
}

Ggplot2Parser <-
    R6::R6Class(
        "Parser",
        public = list(
            tokens = GGPLOT2_TOKENS,
            literals = GGPLOT2_LITERALS,
            # Parsing rules
            #precedence = list(),
            # dictionary of names
            names = new.env(hash = TRUE),
            # Note: ggproto contains '+' signs in LAYER tokens
            p_expression_func = function(
                    doc="expression : gg_init
                                    | gg_init aes_func
                                    | gg_init ggproto_list
                                    | gg_init aes_func ggproto_list", p) {
                dbgmsg("p_expression_func LAST")

                ggbashenv$dataset_name <-
                    gsub("ggplot2::ggplot\\(", "", p$get(2))

                if (ggbashenv$dataset_name != ".")
                    ggbashenv$dataset <-
                        eval(as.symbol(ggbashenv$dataset_name),
                             envir = .GlobalEnv)
                ggbashenv$geom <- ""

                if (p$length() == 2) {
                    dbgmsg("GGPLOT only ")
                    p$set(1, paste0(p$get(2), ")"))
                } else if (p$length() == 3 && (! grepl("\\+", p$get(3)))[1] ) {
                    p$set(1,
                          paste0(p$get(2), ", ggplot2::aes(", p$get(3), ")"))
                } else if (p$length() == 3) {
                    p$set(1, paste0(p$get(2), ")", p$get(3)))
                } else {
                    p$set(1,
                          paste0(p$get(2),
                                 ", ggplot2::aes(", p$get(3), ")", p$get(4)))
                }
                },
            p_gg_init = function(doc="gg_init : GGPLOT", p) {
                dbgmsg("p_gg_init: ", p$get(2))
                ggbashenv$dataset_name <-
                    gsub("ggplot2::ggplot\\(", "", p$get(2))
                if (ggbashenv$dataset_name == ".") {
                    # ggbashenv$dataset is already set
                } else {
                    ggbashenv$dataset <- tryCatch( {

                        eval(as.symbol(ggbashenv$dataset_name),
                             envir = .GlobalEnv)
                    }, error = function(err) {"error"} )
                    ggbashenv$colv <- colnames(ggbashenv$dataset)
                }


                if (class(ggbashenv$dataset)[1] == "character") {
                    errinfo <-
                        list(id = "p_gg_init:dataset",
                             type = "No dataset found",
                             input = ggbashenv$dataset_name)
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                    # FIXME doesn't stop
                }

                ggbashenv$conf <-
                    list(aes = c(), non_aes = c(), geom_list = c())
                set_ggbashenv_warning()
                dbgmsg("  set dataset name: ", ggbashenv$dataset_name)

                p$set(1, p$get(2))
            },
            p_ggproto_list = function(doc="ggproto_list : ggproto
                                      | ggproto ggproto_list", p) {
                dbgmsg("p_ggproto_list: ", p$get(2))
                if (p$length() == 2)
                    p$set(1, p$get(2))
                else
                    p$set(1, paste0(p$get(2), p$get(3)))
                },
            p_ggproto_layer =
                function(doc =
                "ggproto : layer_init
                         | layer_init layer_aes_list
                         | layer_init layer_raw_aes
                         | layer_init layer_aes_list layer_raw_aes
                         | layer_init layer_raw_aes layer_aes_list", p) {
                dbgmsg("p_ggproto_layer: ", p$get(2), " NONTERMINAL")

                # ex: ggbashenv$geom is 'point'
                if (p$length() == 2) {
                    return(p$set(1, paste0(p$get(2), "()")))
                }

                # FIXME more general
                dbgmsg("  3rd is : ", p$get(3))
                raw_is_3rd <-
                    grepl("=([0-9\\.\\+\\-\\*\\/\\^]+|\\\"|')", p$get(3)) ||
                    grepl(ggregex$booleanaes, p$get(3)) ||
                    grepl(ggregex$constaes, p$get(3))

                if (grepl("\\.\\.", p$get(3))) {
                    dbgmsg("hit")       # FIXME ugly ..prop..
                    raw_is_3rd <- FALSE
                }

                if (raw_is_3rd) {
                    if (p$length() == 3) {
                        dbgmsg("    len==3 and raw_is_3rd: ",
                               p$get(2), " ", p$get(3))
                        p$set(1, paste0(p$get(2), "(", p$get(3)))
                    } else {
                        dbgmsg("    len!=3 and raw_is_3rd: ",
                               p$get(2), " ", p$get(3), " ", p$get(4))
                        p$set(1, paste0(p$get(2), "(", p$get(3),
                            ", ggplot2::aes(", p$get(4), ")"))
                    }
                } else {
                    if (p$length() == 3) {
                        dbgmsg("    len==3 and raw != 3rd: ",
                               p$get(2), " ", p$get(3))
                        p$set(1,
                            paste0(p$get(2),
                                "(ggplot2::aes(", p$get(3), ")"))
                    } else {
                        dbgmsg("    len==3 and raw != 3rd: ",
                               p$get(2), " ", p$get(3), " ", p$get(4))
                        p$set(1,
                            paste0(p$get(2),
                                "(ggplot2::aes(", p$get(3), ", ", p$get(4)))
                    }
                }
                },
            p_layer_init = function(doc="layer_init : LAYER", p) {
                # initialization
                dbgmsg("p_layer_init: ", p$get(2))
                dbgmsg("  ggbashenv$geom(before): ", ggbashenv$geom)
                prev <- gsub("\\s*(\\+|\\|)\\s*(geom_)?", "", p$get(2))
                gv <- ggbashenv$const$geom_namev
                ggbashenv$geom <-
                    gv[find_first_by_prefix(prev, gv, ggbashenv$show_amb_warn)]
                dbgmsg("  ggbashenv$geom(after): ", ggbashenv$geom)
                ggbashenv$conf$geom_list <-
                    c(ggbashenv$conf$geom_list, ggbashenv$geom)
                ggbashenv$aes_i <- 1
                p$set(1, paste0(" + ggplot2::geom_", prev))
            },
            p_layer_aes_list = function(
                doc="layer_aes_list : layer_aes
                                    | layer_aes layer_aes_list", p) {
                dbgmsg("p_layer_aes_list: ", p$get(2), " NONTERMINAL")

                if (p$length() == 2) {
                    p$set(1, paste0(p$get(2), ")"))
                } else {
                    p$set(1, paste0(p$get(2), ", ", p$get(3)))
                }
            },
            p_layer_aes = function(doc="layer_aes : NAME", p) {
                dbgmsg("p_layer_aes: ", p$get(2))

                # do column-name partial match
                single_quote <- "'"
                double_quote <- '"'

                colnamev <- ggbashenv$colv

                must_aesv <- get_required_aes(ggbashenv$geom)
                all_aesv <- get_possible_aes(ggbashenv$geom)
                index <- ggbashenv$aes_i
                if (index < 1) {
                    return(p$set(1, paste0(p$get(2), ")")))
                    # error?
                }
                dummy_aesv <- c(rep("", index - 1), p$get(2))
                column_name <- parse_ggbash_aes(
                    index, dummy_aesv, must_aesv,
                    all_aesv, colnamev, ggbashenv$show_amb_warn)

                if (is.null(column_name)) {
                    errinfo <-
                        list(
                            id = "p_layer_aes:column_prefix",
                            type = "Column name not found",
                            input = p$get(2)
                        )
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                if (! grepl("=", p$get(2)))
                    ggbashenv$aes_i <- ggbashenv$aes_i + 1
                ggbashenv$conf$aes <- c(ggbashenv$conf$aes, column_name)

                dbgmsg("  parsed: ", column_name)

                p$set(1, column_name)
            },
            p_layer_raw_aes = function(
                doc="layer_raw_aes : CHARAES
                                   | CONSTAES
                                   | BOOLEANAES
                                   | CHARAES layer_raw_aes
                                   | CONSTAES layer_raw_aes
                                   | BOOLEANAES layer_raw_aes", p) {
                dbgmsg("p_layer_raw_aes: ", p$get(2))

                if (grepl("data=", p$get(2))) {
                    if (p$length() == 2)
                        return(p$set(1, paste0(p$get(2), ")")))
                    else
                        return(p$set(1, paste0(p$get(2), ", ", p$get(3))))
                }

                all_aesv <- get_possible_aes(ggbashenv$geom)
                layer_params <- get_layer_params(ggbashenv$geom)
                all_rawv <- c(all_aesv, layer_params)
                all_rawv <- unique(all_rawv)
                colnamev <- ggbashenv$colv

                raw_aes <- parse_ggbash_non_aes(p$get(2), all_rawv, colnamev,
                                                ggbashenv$show_amb_warn)
                ggbashenv$conf$non_aes <- c(ggbashenv$conf$non_aes, raw_aes)


                if (is.null(raw_aes)) {
                    errinfo <- list(
                        id = "p_layer_raw_aes:partial_match",
                        type = "No such parameter for the geom",
                        input = p$get(2),
                        table = all_rawv
                    )
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                if (p$length() == 2)
                    p$set(1, paste0(raw_aes, ")"))
                else
                    p$set(1, paste0(raw_aes, ", ", p$get(3)))
                },
            # p_position_func = function(doc="position_func : ", p) {
            #
            # },
            p_aes_func = function(doc="aes_func : NAME
                                                | CONSTAES aes_func
                                                | NAME aes_func", p) {
                dbgmsg("p_aes_func: ", p$get(2))

                colnamev <- ggbashenv$colv

                geom_tmp <- "point" # FIXME more general
                # FIXME defaultZproblem - z should not be removed
                # StatContour$required_aes has "z" as well as "x" and "y"
                # But how p_aes_func can know
                # the following geom is geom_contour()?
                # Currently adhoc fix in parse_ggbash_aes
                must_aesv <- get_required_aes(geom_tmp)
                all_aesv <- get_possible_aes(geom_tmp)

                column_name <-
                    parse_ggbash_aes(1, p$get(2), must_aesv,
                                    all_aesv, colnamev, ggbashenv$show_amb_warn)
                if (is.null(column_name)) {
                    input <- gsub("[a-z]+=", "", p$get(2))
                    errinfo <-
                        list(
                            id = "p_aes_func:prefix_match",
                            type = "No such column names\n",
                            input = input,
                            table = colnamev
                        )
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                # FIXME defaultZproblem - z should not be removed
                if (grepl("(x=|y=)", column_name))
                    column_name <- gsub("[a-z]+=", "", column_name)


                if (p$length() == 2) {
                    p$set(1, paste0(column_name, ")"))
                } else {
                    p$set(1, paste0(column_name, ", ", p$get(3)))
                }
            },
            # see p_ggproto_layer
            p_ggproto_theme = function(doc="ggproto : theme_init
                                       | theme_init theme_elem_list", p) {
                dbgmsg("p_ggproto_theme: ", p$get(2), " -- add ) ")
                if (p$length() == 2) {
                    end <- paste0(p$get(2), ")")
                    p$set(1, end)
                } else {
                    p$set(1, paste0(p$get(2), p$get(3), ")"))
                }
            },
            p_theme_init = function(doc="theme_init : THEME", p) {
                # initialization
                dbgmsg("p_theme_init: ", p$get(2), " -- add (")
                # theme, theme_bw, theme_linedraw, ...
                theme_str <- gsub("\\s|\\+", "", p$get(2))
                p$set(1, paste0(" + ggplot2::", theme_str, "("))
            },
            p_theme_elem_list = function(
                doc="theme_elem_list : theme_elem theme_conf_list
                                | theme_elem theme_conf_list theme_elem_list",
                p) {
                dbgmsg("p_theme_elem_list", p$get(2), " ",
                       p$get(3), " -- add ( and ) ")
                elem <- p$get(2)
                if (p$length() == 3) {
                    # last configuration
                    p$set(1, paste0(elem, "(", p$get(3), ")"))
                    # close ggplot2::element_*(
                    # MAYBE-LATER "none" is now ("none")
                } else {
                    #if (! ggbashenv$elem_class %in% c("logical", "character"))
                        p$set(1, paste0(elem, "(", p$get(3), "), ", p$get(4)))
                    #else
                    #    p$set(1, paste0(elem, p$get(3), "), ", p$get(4)))
                    # text = element_text(...) , ... so no need to close paren
                }
            },
            p_theme_elem = function(doc="theme_elem : NAME", p) {
                dbgmsg("p_theme_elem: ", p$get(2))
                tdf <- ggbashenv$const$themedf

                # 'axis.te:' will be 'axis.te'
                #elem_name_partial <- gsub("\\:", "", p$get(2))
                elem_name_partial <- p$get(2)

                elem_name <- tdf$name[find_first_index(elem_name_partial,
                                                 tdf$name, show_warn = FALSE)]

                # do partial match for theme element
                # (ex. 'legend.t' -> 'legend.text')
                elem_class <- tdf$class[find_first_index(elem_name,
                                                   tdf$name,
                                                   show_warn = FALSE)]

                if (length(elem_class) == 0 || is.na(elem_class)) {
                    errinfo <-
                    list(
                        id = "p_theme_elem:prefix_match",
                        type = "Prefix match for theme element name failed.",
                        raw = p$get(2),
                        input = elem_name_partial,
                        table = tdf$name
                        )
                    show_fixit_diagnostics(errinfo)

                    ggbashenv$error <- TRUE

                    return(p$set(1, GGPLOT2INVALIDTOKEN))

                } else if (length(elem_class) > 1) {
                    message("UNKNOWN ERROR in p_theme_elem: ",
                            paste0(elem_class, collapse = " "))
                    elem_class <- elem_class[1] # What's this error?
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                ggbashenv$elem_class <- elem_class

                if (grepl("^element_|margin", elem_class)) {
                    modifier <- "ggplot2::"
                    function_name <- paste0(modifier, elem_class)
                } else if (elem_class == "unit") {
                    modifier <- "grid::"
                    function_name <- paste0(modifier, elem_class)
                } else if (elem_class %in% c("logical", "character") ){
                    function_name <- ""
                } else {
                    message("ERROR: cannot get correct ",
                            "classes for a theme element: ",
                            paste0(elem_class, collapse = " "))
                    elem_class <- elem_class[1] # What's this error?
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                p$set(1, paste0(elem_name, " = ", function_name))
            },
            p_theme_conf_list = function(doc="theme_conf_list : CONSTAES
                                         | CHARAES
                                         | QUOTED
                                         | BOOLEAN
                                         | UNIT
                                         | CONSTAES theme_conf_list
                                         | CHARAES theme_conf_list", p) {
                dbgmsg("p_theme_conf_list: ", p$get(2))

                if (! is.null(ggbashenv$error)) {
                    ggbashenv$error <- NULL # FIXME too compicated
                    # FIXME the error in o_theme_elem cannot stop
                    # even if return(p$set(1, GGPLOT2_INVALIDTOKEN)).
                    # It tries to execute this production rule,
                    # so currently early retrun here.
                    # There should be more elegant error handling.
                    return(p$set(1, NULL))
                }

                conf <- p$get(2)

                if (grepl(ggregex$quoted, conf) &&
                    ! grepl("^element_|margin", ggbashenv$elem_class)) {
                    message("quoted ", conf, " env$elemclass: ", ggbashenv$elem_class)
                    return(p$set(1, conf))
                } else if (grepl(ggregex$boolean, conf)) {
                    message("boolean ", conf, " env$elemclass: ", ggbashenv$elem_class)
                    return(p$set(1, conf))
                } else if (grepl(ggregex$unit, conf)) {
                    number <- gsub("[^0-9\\.]", "", conf)
                    this_unit <- gsub("[0-9\\. ]", "", conf)
                    return(p$set(1, paste0(number, ",'", this_unit, "'")))
                }

                before_equal <- gsub("=.*", "", conf)
                after_equal  <- gsub(".*=", "", conf)

                # prefix match
                input <- ggbashenv$elem_class
                tbl <- get_theme_elem_name_conf(input)
                conf_name <- tbl[find_first_index(before_equal,
                                                  tbl, show_warn = FALSE)]

                if (is.na(conf_name)) {
                    errinfo <- list(
                        id = "p_theme_conf_list:partial_match",
                        type = paste0("Partial match for theme ",
                                      "element configuration failed."),
                        input = before_equal,
                        table = tbl
                    )
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                a_conf <- paste0(conf_name, "=", after_equal)

                if (p$length() == 2) {
                    # FIXME add spaces
                    p$set(1, a_conf)
                } else {
                    p$set(1, paste0(a_conf, ", ", p$get(3)))
                }
            },
            p_error = function(p) {
                if (is.null(p)) {
                    errinfo <- list( id = "p_error:null",
                                     type = "Syntax error at EOF")
                } else {
                    errinfo <- list( id = "p_error:non_null",
                                     type = paste0("Syntax error at \"",
                                                   p$value, "\""))
                }
                show_fixit_diagnostics(errinfo)
            }
            )
        )

#' Display useful debugging info for users
#'
#' @param err A list of error information
#'
show_fixit_diagnostics <- function(
    err = list(
        id = "p_theme_elem:prefix_match",
        type = "Prefix match for theme element name failed.",
        input = "axis.tx:",
        elem_name = "axis.tx",
        elem_table = c("axis.text", "axis.title")
    )
) {
    # MAYBE-LATER Is it possible to get the built entire ggplot object here?
    message("COMPILE ERROR: ", err$type)
    m1 <- function(...) message("  ", ...)
    m2 <- function(...) message("    ", ...)
    m3 <- function(...) message("      ", ...)

    similarv <- get_analogue(err$input, err$table)$name

    if (err$id == "p_theme_elem:prefix_match") {

        m1("Is your theme element's name correct?")
        m2("The supplied string is \"", err$raw, "\", but")
        m3("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_layer_aes:column_prefix") {
        colv <- ggbashenv$colv
        similarv <- get_analogue(err$input, colv)$name

        m1("The column name \"", err$input, "\" does not exist.")
        m2("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_theme_conf_list:partial_match") {
        m1("The column name \"", err$input, "\" does not exist.")
        m2("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_aes_func:prefix_match") {
        m1("The column name \"", err$input, "\" does not exist.")
        m2("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_layer_raw_aes:partial_match") {
        m1("The special parameter \"", err$input, "\" does not exist.")
        m2("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_gg_init:dataset") {
        m1("Is your data frame name correct?")
    } else if (err$id == "p_error:non_null") {
        m1("Did you specify a geom before aesthetics?")
        m2(" BAD: gg mtcars + mpg cyl")
        m2("GOOD: gg mtcars + point mpg cyl")
    } else if (err$id == "p_error:null") {

    }
}

#' remove parentheses and marks
#'
#' ggbash have to handle two types of parentheses and tyo types of commas.
#' One is unnecessary and the other is necessary for correct parsing.
#' For example, in \code{gg(iris) + bin2d(x, y, binwidth=c(.1, .1))},
#' all parentheses and commas before "binwidth" are unnecessary.
#' So this should be replaced by
#' \code{gg iris  + bin2d x  y  binwidth=c(.1, .1) },
#' which can be parsed by ggplot2 compiler.
#'
#' This is because while ggbash does not rely on commas and parens for
#' parsing, R relies on them.
#'
#' @param input A character
#'
#' @export
remove_unnecessary_marks <- function(
    input = "gg(m, x=f(cyl), m) + t(l=p0('l:', w))"
){
    # FIXME parentheses for no equal case
    replace_with_space <- function(input, i)
        paste0(substr(input, 1, i - 1), " ",
               substr(input, i + 1, nchar(input)))
    n_paren <- 0
    is_after <- FALSE
    state <- ""
    for ( i in 1:nchar(input)) {
        this <- substr(input, i, i)
        if (is_after) {
            if (this == "(")
                n_paren <- n_paren + 1
            else if (this == ")")
                n_paren <- n_paren - 1

            # nested equals are ignored

            if (n_paren == 0 && this == ",")
                input <- replace_with_space(input, i)

            if (n_paren < 0) {
                is_after <- FALSE
                n_paren <- n_paren + 1
                input <- replace_with_space(input, i)
            }
        } else {
            if (this == "(")
                input <- replace_with_space(input, i)
            else if (this == ")")
                input <- replace_with_space(input, i)
            else if (this == ",")
                input <- replace_with_space(input, i)
            else if (this == "=")
                is_after <- TRUE
        }
        state <- paste0(state, n_paren)
    }
    return(input)
}

coat_adhoc_syntax_sugar <- function(
    cmd = "gg(mtcars,mpg,hwy) + point(size = xyz(gear) +1, shape = 16 / 3 * 4)"
){
    out <- gsub("\\s*,\\s*", ",", cmd) # no comma
    out <- gsub("\\s*=\\s*", "=", out)
    out <- gsub("\\s*-\\s*", "-", out)
    out <- gsub("\\s*/\\s*", "/", out)
    out <- gsub("\\s*\\+\\s*", "\\+", out)
    out <- gsub("\\s*\\*\\s*", "\\*", out)
    out <- remove_unnecessary_marks(out)
    return(out)
}

#' the core function of ggbash
#'
#' @param cmd A character
#'
#' compile_ggbash returns a built ggplot object.
#'
compile_ggbash <- function(cmd){
    cmd <- coat_adhoc_syntax_sugar(cmd)
    ggobj <- rly::yacc(Ggplot2Parser)$parse(
        cmd, rly::lex(Ggplot2Lexer)
    )
    return(ggobj)
}

lex  <- rly::lex(Ggplot2Lexer)
yacc <- rly::yacc(Ggplot2Parser)

gbash <- function(ggbash_symbols) {
    is_string <- tryCatch(class(ggbash_symbols) == "character",
                          error = function(err) {FALSE})
    if (is_string) {
        cmd <- ggbash_symbols
    } else {
        raw_cmd <- deparse(substitute(ggbash_symbols),
                           width.cutoff = 500) # arbitrary large
        cmd <- raw_cmd
    }
    compile_ggbash(cmd)
}

# FIXME duplicate
bash <- function(ggbash_symbols) {
    is_string <- tryCatch(class(ggbash_symbols) == "character",
                          error = function(err) {FALSE})
    if (is_string) {
        cmd <- ggbash_symbols
    } else {
        raw_cmd <- deparse(substitute(ggbash_symbols),
                           width.cutoff = 500) # arbitrary large
        cmd <- raw_cmd
    }
    gsub("ggplot2::", "", compile_ggbash(cmd))
}
