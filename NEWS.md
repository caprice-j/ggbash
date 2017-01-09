

# ggbash 0.3.0 (2017-01-09)

## Major Updates

### Fixit

+ From this v0.3.0, ggbash incorporates a powerful feature called *fixit*.
  It's inspired from a modern C++ compiler
  clang's [Fix-It Hints](http://clang.llvm.org/docs/InternalsManual.html#fix-it-hints).
  Now when ggbash users input a string which contains some typos,
  ggbash gives users useful information on
  what might be needed to fix the compile error.

    + For example, if users wrongly input
      the column name like `gg iris + point Sepl.W Sepal.L`,
      ggbash gives `Column name not found` error
      and also displays that the most similar column names are
      `Sepal.Width, Species, Sepal.Length, Petal.Width, Petal.Length`,
      in this order.
      This could significantly reduce the learning cost of ggplot2 and ggbash,
      having a possibility of making ggbash suitable for new ggplot2 users
      as well as its heavy users.

### Other Bug Fixes

+ checked coding style according to jimhester/lintr package.
+ renamed partial_match_utils.R to prefix_match.R (more accurate)

## ggbash 0.2.2 (2017-01-08)

+ convert `axis.ticks.length = 10 cm` into `grid::unit(10, 'cm')` in `theme()`
+ partial prefix match for theme element names

## ggbash 0.2.1

+ handle whitespaces of aesthetics before/after `=`
+ default themes (`theme_bw()`, `theme_classic()`, ...) works by `+ theme bw` or `+ theme_bw`
+ replace dplyr::tbl_df with more lightweight tibble::tibble
+ handle character/logical classes of theme() elements
+ refactor `lexer.R` and `parser.R` into `ggplot2-compiler.R`

# ggbash 0.2

## Major Updates

### Implemented ggplot2::theme()

`ggplot2::theme()` is a powerful function that tweaks a lot of plot elements.

In this v0.1.2 release, most of the functions of `theme` are implemented.

It might need more test cases because of a lot of possibilities (such as a character variable),
so the next a few weeks will be for fixing various bugs.

#### Copy of Internal Variable `ggplot2:::.element_tree`

Implementation of `theme` requires names and classes of
theme elements like `axis.text.x` or `legend.title`.

As far as I researched, using `ggplot2:::.element_tree` seemed to be the best way
to get those classes such as `element_text` or `character`.

But anyone can see in `get_all_theme_aes` implementation,
this does not look a clean way to achieve that.

#### Design Decision of `theme`

One design decision I made and am not certain is to use `:` for separating theme's elements.

One example is:
```
theme(text = element_text(size=5, face="bold"), line = element_line(linetype='dotted'))
theme text:               size=5  face="bold"   line:               linetype='dotted'
```

The other ways I considered were as follows:

```
theme text  = size=5 face="bold"   line  = linetype="dotted"   # nested =
theme text := size=5 face="bold"   line := linetype="dotted"   # new operator :=
theme text(size=5, face="bold")    line(linetype="dotted")     # function-like parentheses
theme text(size=5  face="bold")    line(linetype="dotted")     # without commas
```

I could not decide which way is the best -- 
for me, all of the above options look violated [principle of least astonishment](https://en.wikipedia.org/wiki/Principle_of_least_astonishment).

But the implemented one lookedthe  most intuitive, easy-to-learn,
and easy-to-read at least among them.
This decision will be easily changed by a more appealing alternative.


# ggbash 0.1.1

## Major Updates

### rly (R clone of Lex and Yacc)

In the previous v0.1.0, all the tokenization/parsing/evaluation are interlaced,
leading to possible hard maintainance and less readability.

In this v0.1.1 release, leveraging rly library's functionalities,
most of the tokenization and parsing are
separated into Ggplot2Lexer and Ggplot2Parser R6 objects.

Those two R6 objects are modularized just for ggplot2 grammars.
i.e. tokenization and parsing of builtin ggbash commands
such as copy, echo, or rm are still done by split_by_pipe() procedures.

This change resulted in removals of drawgg() and build_geom(),
and all done in rly branch.

Adding new ggproto objects such as Scale or Coord now become
adding new tokens in GGPLOT2_TOKENS and adding corresponding
prodcution rules in Ggplot2Parser.
This would enable much faster and less error-prone development process.

# ggbash 0.1.0.9000

* first release
