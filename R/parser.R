dbgmsg <- function(...) {
    if (exists('ggbash_debug'))
        message(...)
}

Ggplot2Parser <-
    R6::R6Class("Parser",
                public = list(
                    tokens = GGPLOT2_TOKENS,
                    literals = GGPLOT2_LITERALS,
                    # Parsing rules
                    #precedence = list(),
                    # dictionary of names
                    names = new.env(hash=TRUE),
                    # Note: ggproto contains '+' signs in LAYER tokens
                    p_expression_func = function(doc="expression : gg_init
                                                                 | gg_init aes_func
                                                                 | gg_init ggproto_list
                                                                 | gg_init aes_func ggproto_list", p) {
                        dbgmsg('p_expression_func')

                        dbgmsg('p_expression_func plength: ', p$length())

                        ggbashenv$dataset_name <- gsub('ggplot2::ggplot\\(', '', p$get(2))
                        ggbashenv$dataset <- eval(as.symbol(ggbashenv$dataset_name), envir = .GlobalEnv)
                        ggbashenv$geom <- ''

                        if (p$length() == 2) {
                            dbgmsg('GGPLOT only ')
                            p$set(1, paste0(p$get(2), ')'))
                        } else if (p$length() == 3 && (! grepl('\\+', p$get(3)))[1] ) { # FIXME
                            p$set(1, paste0(p$get(2), ', ggplot2::aes(', p$get(3), ')'))
                        } else if (p$length() == 3) {
                            p$set(1, paste0(p$get(2), ')', p$get(3)))
                        } else { #  5
                            p$set(1, paste0(p$get(2), ', ggplot2::aes(', p$get(3), ')', p$get(4)))
                        }
                        },
                    p_gg_init = function(doc="gg_init : GGPLOT", p) {
                        dbgmsg('p_gg_init')
                        ggbashenv$dataset_name <- gsub('ggplot2::ggplot\\(', '', p$get(2))
                        ggbashenv$dataset <- eval(as.symbol(ggbashenv$dataset_name), envir = .GlobalEnv)
                        ggbashenv$conf <- list(aes=c(), non_aes=c(), geom_list=c())
                        dbgmsg('  set dataset name: ', ggbashenv$dataset_name)

                        p$set(1, p$get(2))
                    },
                    p_ggproto_list = function(doc="ggproto_list : ggproto
                                              | ggproto ggproto_list", p) {
                        dbgmsg('p_ggproto_list')
                        if (p$length() == 2)
                            p$set(1, p$get(2))
                        else
                            p$set(1, paste0(p$get(2), p$get(3)))
                        },
                    p_ggproto_layer = function(doc="ggproto : layer_init
                                                            | layer_init layer_aes
                                                            | layer_init layer_raw_aes
                                                            | layer_init layer_aes layer_raw_aes
                                                            | layer_init layer_raw_aes layer_aes", p) {
                        dbgmsg('p_ggproto_layer')

                        # ex: ggbashenv$geom == 'point'
                        if (p$length() == 2) {
                            return(p$set(1, paste0(p$get(2), '()')))
                        }

                        # FIXME more general
                        dbgmsg('  3rd is : ', p$get(3))
                        raw_is_3rd <- grepl(paste0('=([0-9\\+\\-\\*\\/\\^]+|"|', "'", ')'), p$get(3))

                        if (raw_is_3rd) {
                            if (p$length() == 3) {
                                p$set(1, paste0(p$get(2), '(', p$get(3)))
                            } else {
                                p$set(1, paste0(p$get(2), '(', p$get(3), ', ggplot2::aes(', p$get(4), ')'))
                            }
                        } else {
                            if (p$length() == 3) {
                                p$set(1, paste0(p$get(2), '(ggplot2::aes(', p$get(3), ')'))
                            } else {
                                p$set(1, paste0(p$get(2), '(ggplot2::aes(', p$get(3), ', ', p$get(4)))
                            }
                        }
                        },
                    p_layer_init = function(doc="layer_init : LAYER", p) {
                        # initialization
                        dbgmsg('p_layer_init')
                        dbgmsg('  before: ', ggbashenv$geom)
                        prev <- gsub('\\s*(\\+|\\|)\\s*(geom_)?', '', p$get(2))
                        ggbashenv$geom <- ggbashenv$const$geom_namev[find_first(prev,
                                                                                ggbashenv$const$geom_namev,
                                                                                ggbashenv$showWarn)]
                        dbgmsg('  after: ', ggbashenv$geom)
                        ggbashenv$conf$geom_list <- c(ggbashenv$conf$geom_list, ggbashenv$geom)
                        ggbashenv$aes_i <- 1
                        p$set(1, paste0(' + ggplot2::geom_', prev))
                    },
                    p_layer_aes = function(doc="layer_aes : NAME
                                                          | NAME layer_aes", p) {
                        dbgmsg('p_layer_aes')

                        # do column-name partial match
                        single_quote <- "'"
                        double_quote <- '"'
                        # for ( obj in ls(envir=ggbashenv))
                        #     dbgmsg('obj ', obj, ' ', eval(as.symbol(obj), envir=ggbashenv))

                        colnamev <- colnames(ggbashenv$dataset)

                        must_aesv <- get_required_aes(ggbashenv$geom)
                        all_aesv <- get_possible_aes(ggbashenv$geom)
                        # FIXME show in the right order if insufficient number of without-equal aes
                        index <- length(must_aesv) - ggbashenv$aes_i + 1
                        #index <- ggbashenv$aes_i
                        if (index < 1) {
                            return(p$set(1, paste0(p$get(2), ')')))
                            #return(NULL) # error
                        }
                        dummy_aesv <- c(rep('', index - 1), p$get(2))
                        column_name <- parse_ggbash_aes(index, dummy_aesv, must_aesv,
                                                        all_aesv, colnamev, ggbashenv$showWarn)
                        if (! grepl('=',p$get(2)))
                            ggbashenv$aes_i <- ggbashenv$aes_i + 1
                        ggbashenv$conf$aes <- c(ggbashenv$conf$aes, column_name)

                        if (p$length() == 2) {
                            p$set(1, paste0(column_name, ')'))
                        } else {
                            p$set(1, paste0(column_name, ', ', p$get(3)))
                        }
                        },
                    p_layer_raw_aes = function(doc="layer_raw_aes : CHARAES
                                               | CONSTAES
                                               | CHARAES layer_raw_aes
                                               | CONSTAES layer_raw_aes", p) {
                        all_aesv <- get_possible_aes(ggbashenv$geom)
                        raw_aes <- parse_ggbash_non_aes(p$get(2), all_aesv, ggbashenv$showWarn)
                        ggbashenv$conf$non_aes <- c(ggbashenv$conf$non_aes, raw_aes)

                        if (p$length() == 2)
                            p$set(1, paste0(raw_aes, ')'))
                        else
                            p$set(1, paste0(raw_aes, ', ', p$get(3)))
                        },
                    # p_position_func = function(doc="position_func : ", p) {
                    #
                    # },
                    p_aes_func = function(doc="aes_func : NAME
                                                        | NAME aes_func", p) {
                        dbgmsg('p_aes_func')

                        # for ( obj in ls(envir=ggbashenv))
                        #     dbgmsg('obj ', obj, ' ', eval(as.symbol(obj), envir=ggbashenv))

                        colnamev <- colnames(ggbashenv$dataset)

                        geom_tmp <- 'point' # FIXME more general
                        must_aesv <- get_required_aes(geom_tmp)
                        all_aesv <- get_possible_aes(geom_tmp)

                        column_name <- parse_ggbash_aes(1, p$get(2), must_aesv,
                                                        all_aesv, colnamev, ggbashenv$showWarn)
                        if (is.null(column_name)) {
                            dbgmsg('column_name is null: ', column_name)
                            Sys.sleep(3)
                        }
                        # FIXME is this okay?
                        column_name <- gsub('[a-z]+=', '', column_name)

                        if (p$length() == 2) {
                            p$set(1, paste0(column_name, ')'))
                        } else {
                            p$set(1, paste0(column_name, ', ', p$get(3)))
                        }
                    },
                    # see p_ggproto_layer
                    p_ggproto_theme = function(doc="ggproto : theme_init
                                                            | theme_init theme_elem_list", p) {
                        dbgmsg('p_ggproto_theme')
                        if (p$length() == 2) {
                            p$set(1, paste0(p$get(2), ')'))
                        } else {
                            p$set(1, paste0(p$get(2), p$get(3)))
                        }
                    },
                    p_theme_init = function(doc="theme_init : THEME
                                                            | THEME NAME", p) {
                        # initialization
                        dbgmsg('p_theme_init')
                        if (p$length() == 2) {
                            # theme, theme_bw, theme_linedraw, ...
                            theme_str <- gsub('\\s|\\+', '', p$get(2))
                            p$set(1, paste0(' + ggplot2::', theme_str, '('))
                        } else {
                            # theme bw (no underline between the two)
                            theme_str <- gsub('\\s|\\+', '', p$get(2))
                            theme_str <- paste0(theme_str, '_', p$get(3))
                            p$set(1, paste0(' + ggplot2::', theme_str, '('))
                        }
                    },
                    p_theme_elem_list = function(doc="theme_elem_list : theme_elem
                                                                      | theme_elem theme_elem_list" ,p) {
                        dbgmsg('p_theme_elem_list')
                        elem <- p$get(2)
                        if(p$length() == 2) {
                            p$set(1, paste0(elem, ')')) # close ggplot2::theme(
                        } else {
                            p$set(1, paste0(elem, ', ', p$get(3)))
                            # text = element_text(...) , ...
                        }
                    },
                    p_theme_elem = function(doc="theme_elem : THEMEELEM theme_conf_list", p) {
                        dbgmsg('p_theme_elem')
                        elem_name <- gsub('\\:', '', p$get(2))

                        tdf <- ggbashenv$const$themedf
                        # FIXME ugly
                        elem_class <- tdf[tdf == elem_name, ]$class

                        if (length(elem_class) == 0) {
                            print(p$get(2))
                            print(elem_name)
                            print(elem_class)

                            Sys.sleep(5)
                        } else if (length(elem_class) > 1) {
                            elem_class <- elem_class[1] # What's this error?
                        }

                        if (grepl('^element_|margin', elem_class)) {
                            modifier <- 'ggplot2::'
                        } else if (elem_class == 'unit') {
                            modifier <- 'grid::'
                        } else if (elem_class %in% c('logical', 'character') ){
                            modifier <- 'as.' # as.character and as.logical
                        } else {
                            print(elem_class)
                            Sys.sleep(3)
                        }

                        function_name <- paste0(modifier, elem_class)

                        p$set(1, paste0(elem_name, ' = ', function_name, '(', p$get(3)))
                    },
                    p_theme_conf_list = function(doc="theme_conf_list : CONSTAES
                                                                      | CHARAES
                                                                      | CONSTAES theme_conf_list
                                                                      | CHARAES theme_conf_list", p) {
                        dbgmsg('p_theme_conf_list')
                        conf <- p$get(2)
                        if(p$length() == 2) {
                            # FIXME add spaces
                            p$set(1, paste0(conf, ')')) # close ggplot2::element_sth(
                        } else {
                            p$set(1, paste0(conf, ', ', p$get(3)))
                        }
                    },
                    # p_statement_assign = function(doc='statement : NAME "=" expression', p) {
                    #     self$names[[as.character(p$get(2))]] <- p$get(4)
                    # },
                    # p_statement_expr = function(doc='statement : expression', p) {
                    #     cat(p$get(2))
                    #     cat('\n')
                    # },
                    # p_expression_binop = function(doc="expression : expression '+' expression
                    #                               | expression '-' expression
                    #                               | expression '*' expression
                    #                               | expression '/' expression", p) {
                    #     if(p$get(3) == '+') p$set(1, p$get(2) + p$get(4))
                    #     else if(p$get(3) == '-') p$set(1, p$get(2) - p$get(4))
                    #     else if(p$get(3) == '*') p$set(1, p$get(2) * p$get(4))
                    #     else if(p$get(3) == '/') p$set(1, p$get(2) / p$get(4))
                    #     },
                    # p_expression_uminus = function(doc="expression : '-' expression %prec UMINUS", p) {
                    #     p$set(1, -p$get(3))
                    # },
                    # p_expression_group = function(doc="expression : '(' expression ')'", p) {
                    #     p$set(1, p$get(3))
                    # },
                    # p_expression_number = function(doc='expression : NUMBER', p) {
                    #     p$set(1, p$get(2))
                    # },
                    # p_expression_name = function(doc='expression : NAME', p) {
                    #     p$set(1, self$names[[as.character(p$get(2))]])
                    # },
                    # p_number = function(doc='numeric : NUMBER', p) {
                    #     p$set(1, p$get(2))
                    #     #p$set(1, eval(p$get(2)))
                    # },
                    # p_number_signed = function(doc='number : MINUS INTEGER
                    #                      | MINUS FLOAT', p) {
                    #     p$set(1, eval(paste("-", p$get(3), collapse="")))
                    # },
                    # p_empty = function(doc='empty : ', p) {
                    #     # convention for readable yacc rules?
                    # },
                    p_error = function(p) {
                        if(is.null(p)) cat("Syntax error at EOF")
                        else           cat(sprintf("Syntax error at '%s' ", p$value))
                    }
                    )
    )
