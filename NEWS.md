# ggbash 0.1.1

## Major updates

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
