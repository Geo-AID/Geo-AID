# Expressions

> <sup>**Syntax**</sup>\
> *Expressions&lt;iter&gt;* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [*ImplicitIterator*](iterators.md)<sub>only if *iter* = `true`</sub>\
> &nbsp;&nbsp; | *SimpleExpression*\
> &nbsp;&nbsp; | *Expression&lt;iter&gt;* *BinOp* *Expression&lt;iter&gt;*;\
> \
> *BinOp* :\
> &nbsp;&nbsp; `+` | `-` | `*` | `/`\
> \
> *SimpleExpression* :\
> &nbsp;&nbsp; *SimpleExpressionKind* *DisplayOptions*<sup>?</sup>\
> \
> *SimpleExpressionKind* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [IDENT](identifiers.md)\
> &nbsp;&nbsp; | [NUMBER](numbers.md)\
> &nbsp;&nbsp; | *ExprCall*\
> &nbsp;&nbsp; | *UnOp* *SimpleExpression*\
> &nbsp;&nbsp; | `(` *Expression&lt;true&gt;* `)`\
> &nbsp;&nbsp; | [*ExplicitIterator*](iterators.md)\
> &nbsp;&nbsp; | *PointCollectionConstructor*\
>\
> *ExprCall* :\
> &nbsp;&nbsp; [NAMED_IDENT](identifiers.md) `(` (*Expression&lt;false&gt;* (`,` *Expression&lt;false&gt;*)<sup>\*</sup>)<sup>?</sup> `)`\
> \
> *UnOp* :\
> &nbsp;&nbsp; `-`\
> \
> *PointCollectionConstructor* :\
> &nbsp;&nbsp; `&` `(` *Expression&lt;false&gt;* (`,` *Expression&lt;false&gt;*)<sup>\*</sup> `)`

Expressions represent all values in GeoScript. A simple example of an expression is a variable reference with the variable's value or a number literal. After most expressions display options can be given, modifying how the expression affects the final figure visually.

## Functions
Using a typical call syntax (`funcname(arg1, arg2, ...)`) one can also call functions with specified parameters. Functions can modify the visual output of the figure, e. g. add a line/ray. This behavior can be usually modified using display options. Some functions accept parameter groups, allowing infinite number of parameters. All functions return a single value. Implicit iterators cannot be used in function parameters, unless surrounded by parenthesis.

## Operators
Binary operators all have the standard mathematical operation order. Unary operators always precede binary operators and implicit iterators always precede all operators. You can define your own order with parenthesis.

Currently, Geo-AID supports only addition, subtraction, multiplication and division as binary operators and negation as unary.

## Point Collections
Expressions can also be used to construct point collections out of other expressions with `&(A, B, ...)` syntax. All expressions inside must be convertible to points.