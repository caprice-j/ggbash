library(rly)

partial_unique(define_constant_list()$geom_namev)

TOKENS = c('GGPLOT','NAME','NUMBER','LAYER') # , 'LPAREN' , 'COMMA', 'RPAREN'
# SCALE "ScaleDiscrete" "Scale"         "ggproto"
# GEOM/STAT "LayerInstance" "Layer"         "ggproto"
# COORD "CoordCartesian" "Coord"          "ggproto"
# FACET "FacetGrid" "Facet"     "ggproto"
# labs ?
# POSITION  "PositionDodge" "Position"      "ggproto"
# THEME "theme" "gg"
LITERALS = c('=','-','*','/','^')

Lexer <- R6Class("Lexer",
                 public = list(
                     tokens = TOKENS,
                     literals = LITERALS,
                     #states = list(c('ggplot')),
                     # Note: t_(function) defines precedences implicitly
                     t_GGPLOT = function(re='^g(g|gp|gpl|gplo|gplot)?\\s*[a-zA-Z_][a-zA-Z_0-9\\.]*', t) {
                         t$value <- gsub('^g(g|gp|gpl|gplo|gplot)?\\s*', 'ggplot2::ggplot(', t$value)
                         return(t)
                     },
                     t_NAME = '[a-zA-Z_][a-zA-Z_0-9\\.=]*',
                     #t_LPAREN  = '\\(',
                     #t_RPAREN  = '\\)',
                     #t_COMMA = ',',
                     t_LAYER = function(re='(\\+|\\|)\\s*[a-z_]+', t) {
                         t$value <- '+ partial_replaced'
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
lexer$input('gg iris + point x=Sepal.Width y=Sepal.Length + smooth')
lexer$token()

lexer$input('gg iris x=Sepal.Width y=Sepal.Length + point + smooth')
lexer$token()

lexer$input('gg iris x=Sepal.Width y=Sepal.Length')

# ggplot(mtcars) + geom_point(colour=1,aes(cyl,mpg))  # works

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
                                                                   | GGPLOT ggproto
                                                                   | GGPLOT aes_func ggproto", p) {
                          message('p_expression_func plength: ', p$length())

                          if (p$length() == 2) {
                              message('GGPLOT only ')
                              p$set(1, paste0(p$get(2), ')'))
                          } else if (p$length() == 3 && (! grepl('\\+', p$get(3))) ) {
                              p$set(1, paste0(p$get(2), ', ggplot2::aes(', p$get(3), ')'))
                          } else if (p$length() == 3) {
                              p$set(1, paste0(p$get(2), ')', p$get(3)))
                          } else { #  5
                              p$set(1, paste0(p$get(2), p$get(3), p$get(4)))
                          }
                      },
                      p_ggproto = function(doc="ggproto : layer_func
                                                        | position_func", p) {
                            p$set(1, p$get(2))
                      },
                      p_layer_func = function(doc="layer_func : LAYER
                                                              | LAYER layer_name", p) {
                          if (p$length() == 2) {
                              p$set(1, p$get(2))
                          } else {
                              p$set(1, paste0(p$get(2), p$get(3)))
                          }
                      },
                      p_layer_name = function(doc="layer_name : ", p) {

                      },
                      p_position_func = function(doc="position_func : ", p) {

                      },
                      p_aes_func = function(doc="aes_func : NAME
                                                          | NAME aes_func", p) {
                        if (p$length() == 2) {
                            p$set(1, paste0(p$get(2), ')'))
                        } else {
                            #p$set(1, p$get(2))
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
parser$parse('gg iris + point abc', lexer)
parser$parse('gg iris + point abc def', lexer)
