# CONSTAES : Constant Aesthetics
# CHARAES : Character Aesthetics
GGPLOT2_TOKENS = c('GGPLOT','NAME','CONSTAES','CHARAES','THEME','LAYER','THEMEELEM','BOOLEAN','QUOTED')
# SCALE "ScaleDiscrete" "Scale"         "ggproto"
# GEOM/STAT "LayerInstance" "Layer"         "ggproto"
# COORD "CoordCartesian" "Coord"          "ggproto"
# FACET "FacetGrid" "Facet"     "ggproto"
# labs ?
# POSITION  "PositionDodge" "Position"      "ggproto"
# THEME "theme" "gg"
GGPLOT2_LITERALS = c() # needed?

# MAYBE-LATER don't know how to pass variables between yacc's production rules
ggbashenv <- new.env() # Note: This is a global variable.

ggbash_plus_pipe <- '(\\+|\\|)\\s*'

Ggplot2Lexer <-
    R6::R6Class("Lexer",
                public = list(
                    tokens = GGPLOT2_TOKENS,
                    literals = GGPLOT2_LITERALS,
                    #states = list(c('ggplot')),
                    # Note: t_(function) defines precedences implicitly
                    t_GGPLOT = function(re='^(g|gg|ggp|ggpl|ggplo|ggplot)\\s+[a-zA-Z_][a-zA-Z_0-9\\.]*', t) {
                        t$value <- gsub('^g(g|gp|gpl|gplo|gplot)?\\s*', 'ggplot2::ggplot(', t$value)
                        return(t)
                    },
                    t_CONSTAES = paste0('[a-z]+=[0-9\\.]+'), # integers and floats
                    t_BOOLEAN = '(TRUE|FALSE|T|F|0|1|true|false|True|False)',
                    t_QUOTED = paste0('^(', "'|", '")',                 # start from a quote
                                      '[a-zA-Z0-9\\._\\+\\-\\*\\/\\^]',
                                      "('|", '")'),                     # end by a quote
                    # I believe CONSTAES cannot contain +-*/^,
                    # because 'gg iris + point Sepal.W Sepal.L size=4 + smooth colour="blue"'
                    # will be interpreted as LexToken(CHARAES,colour="blue" size=4 + smooth colour="blue",1,33)
                    t_CHARAES = paste0('[a-z]+=("|', "'", ').*?("|', "'", ')'),
                    t_THEMEELEM = function(re='[a-zA-Z_][a-zA-Z\\.]*\\s*\\:', t) {
                        t$value <- gsub(' ', '', t$value)
                        return(t)
                    },
                    t_NAME      = '[a-zA-Z_][a-zA-Z_0-9\\.=]*',
                    #t_LPAREN  = '\\(',
                    #t_RPAREN  = '\\)',
                    #t_COMMA = ',',
                    t_THEME = '(\\+|\\|)\\s*theme', # t_THEME is preferred to t_LAYER
                    t_LAYER = function(re='(\\+|\\|)\\s*[a-z_]+', t) {
                        if (grepl('(\\+|\\|)\\s*theme', t$value)) {
                            t$type <- 'THEME'
                            return(t)
                        }
                        partial <- gsub(paste0(ggbash_plus_pipe, '(geom_)?'), '', t$value)
                        ggbashenv$const <- define_ggbash_constant_list()
                        # FIXME showWarn
                        ggbashenv$showWarn <- TRUE
                        geom_sth <- ggbashenv$const$geom_namev[find_first(partial,
                                                                          ggbashenv$const$geom_namev,
                                                                          ggbashenv$showWarn)]

                        t$value <- paste0(' + geom_', geom_sth)
                        return(t)
                    },
                    t_NUMBER = function(re='\\d+', t) {
                        t$value <- strtoi(t$value)
                        return(t)
                    },
                    t_ignore = " \t",
                    t_newline = function(re='\\n+', t) {
                        t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
                        return(NULL)
                    },
                    t_error = function(t) {
                        cat(sprintf("Illegal character '%s'", t$value[1]))
                        t$lexer$skip(1)
                        return(t)
                    }))

