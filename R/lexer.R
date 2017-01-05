library(rly)

TOKENS = c('GGPLOT','NAME','NUMBER','LAYER') # , 'LPAREN' , 'COMMA', 'RPAREN'
# SCALE "ScaleDiscrete" "Scale"         "ggproto"
# GEOM/STAT "LayerInstance" "Layer"         "ggproto"
# COORD "CoordCartesian" "Coord"          "ggproto"
# FACET "FacetGrid" "Facet"     "ggproto"
# labs ?
# POSITION  "PositionDodge" "Position"      "ggproto"
# THEME "theme" "gg"
LITERALS = c('=','-','*','/','^')

ggbashenv <- new.env()

Lexer <- R6Class("Lexer",
                 public = list(
                     tokens = TOKENS,
                     literals = LITERALS,
                     #states = list(c('ggplot')),
                     # Note: t_(function) defines precedences implicitly
                     t_GGPLOT = function(re='^(g|gg|ggp|ggpl|ggplo|ggplot)\\s+[a-zA-Z_][a-zA-Z_0-9\\.]*', t) {
                         t$value <- gsub('^g(g|gp|gpl|gplo|gplot)?\\s*', 'ggplot2::ggplot(', t$value)
                         return(t)
                     },
                     t_NAME = '[a-zA-Z_][a-zA-Z_0-9\\.=]*',
                     #t_LPAREN  = '\\(',
                     #t_RPAREN  = '\\)',
                     #t_COMMA = ',',
                     t_LAYER = function(re='(\\+|\\|)\\s*[a-z_]+', t) {
                         partial <- gsub('\\s*(\\+|\\|)\\s*(geom_)?', '', t$value)
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
                     }
                 )
)
lexer  <- rly::lex(module=Lexer, debug = TRUE) # Build all regular expression rules from the supplied
function(){
    lexer$input('gg iris + point abc def + smooth ghi jkl')

}

Parser <- R6Class("Parser",
                  public = list(
                      tokens = TOKENS,
                      literals = LITERALS,
                      # Parsing rules
                      #precedence = list(),
                      # dictionary of names
                      names = new.env(hash=TRUE),
                      # Note: ggproto contains '+' signs in LAYER tokens
                      p_expression_func = function(doc="expression : GGPLOT
                                                                   | GGPLOT aes_func
                                                                   | GGPLOT ggproto_list
                                                                   | GGPLOT aes_func ggproto_list", p) {
                          message('p_expression_func plength: ', p$length())

                          ggbashenv$dataset_name <- gsub('ggplot2::ggplot\\(', '', p$get(2))
                          ggbashenv$dataset <- eval(as.symbol(ggbashenv$dataset_name), envir = .GlobalEnv)

                          if (p$length() == 2) {
                              message('GGPLOT only ')
                              p$set(1, paste0(p$get(2), ')'))
                          } else if (p$length() == 3 && (! grepl('\\+', p$get(3)))[1] ) { # FIXME
                              p$set(1, paste0(p$get(2), ', ggplot2::aes(', p$get(3), ')'))
                          } else if (p$length() == 3) {
                              p$set(1, paste0(p$get(2), ')', p$get(3)))
                          } else { #  5
                              p$set(1, paste0(p$get(2), p$get(3), p$get(4)))
                          }
                      },
                      p_ggproto_list = function(doc="ggproto_list : ggproto
                                                                  | ggproto ggproto_list", p) {
                        if (p$length() == 2)
                            p$set(1, p$get(2))
                        else
                            p$set(1, paste0(p$get(2), p$get(3)))
                      },
                      p_ggproto = function(doc="ggproto : LAYER
                                                        | LAYER layer_aes", p) {
                          ggbashenv$previous_geom <- gsub('\\s*(\\+|\\|)\\s*(geom_)?', '', p$get(2))
                          # ex: ggbashenv$previous_geom == 'point'
                          if (p$length() == 2) {
                              p$set(1, paste0(p$get(2), '()'))
                          } else {
                              p$set(1, paste0(p$get(2), '(ggplot2::aes(', p$get(3), ')'))
                          }
                      },
                      p_layer_aes = function(doc="layer_aes : NAME
                                                            | NAME layer_aes", p) {
                            # column name partial match?
                        single_quote <- "'"
                        double_quote <- '"'
                        geom_sth <- ggbashenv$previous_geom
                        #message('p layer aes: ', geom_sth)
                        colnamev <- colnames(ggbashenv$dataset)
                        geom_sth <- ggbashenv$const$geom_namev[find_first(geom_sth,
                                                                          ggbashenv$const$geom_namev,
                                                                          ggbashenv$showWarn)]

                        must_aesv <- get_required_aes(geom_sth)
                        all_aesv <- get_possible_aes(geom_sth)
                        # FIXME positional
                        column_name <- parse_ggbash_aes(1, p$get(2), must_aesv,
                                                        all_aesv, colnamev, ggbashenv$showWarn)

                        if (p$length() == 2) {
                            p$set(1, paste0(column_name, ')'))
                        } else {
                            p$set(1, paste0(column_name, ', ', p$get(3)))
                        }
                      },
                      p_position_func = function(doc="position_func : ", p) {

                      },
                      p_aes_func = function(doc="aes_func : NAME
                                                          | NAME aes_func", p) {
                        if (p$length() == 2) {
                            p$set(1, paste0(p$get(2), ')'))
                        } else {
                            p$set(1, paste0(p$get(2), ',', p$get(3)))
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
                      p_number = function(doc='numeric : NUMBER', p) {
                          p$set(1, p$get(2))
                          #p$set(1, eval(p$get(2)))
                      },
                      # p_number_signed = function(doc='number : MINUS INTEGER
                      #                      | MINUS FLOAT', p) {
                      #     p$set(1, eval(paste("-", p$get(3), collapse="")))
                      # },
                      p_empty = function(doc='empty : ', p) {
                          # convention for readable yacc rules?
                      },
                      p_error = function(p) {
                          if(is.null(p)) cat("Syntax error at EOF")
                          else           cat(sprintf("Syntax error at '%s' ", p$value))
                      }
                      )
                  )




parser <- rly::yacc(Parser)
parser$parse('gg iris', lexer)
parser$parse('gg iris SepalWidth', lexer)
parser$parse('gg iris SepalWidth SepalLength', lexer)

parser$parse('gg iris + point', lexer)
parser$parse('gg iris + point Sepal.W', lexer)
parser$parse('gg iris + point Sepal.W Sepal.L', lexer)

parser$parse('gg iris + point Sepal.W Sepal.L + smooth', lexer)
parser$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W', lexer)
parser$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W Sepal.L', lexer)

parser$parse('gg iris + point Sepal.W Sepal.L + smooth', lexer)
parser$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W', lexer)
parser$parse('gg iris + point Sepal.W Sepal.L + smooth Sepal.W Sepal.L', lexer)
