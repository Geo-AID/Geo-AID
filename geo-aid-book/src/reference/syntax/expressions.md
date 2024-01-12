# Expressions

> <sup>**Syntax**</sup>\
> *Expression&lt;iter&gt;* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [*ImplicitIterator*](iterators.md)<sub>only if *iter* = `true`</sub>\
> &nbsp;&nbsp; | *SimpleExpression*\
> &nbsp;&nbsp; | *Expression&lt;iter&gt;* *BinOp* *Expression&lt;iter&gt;*;\
> \
> *BinOp* :\
> &nbsp;&nbsp; `+` | `-` | `*` | `/`\
> \
> *SimpleExpression* :\
> &nbsp;&nbsp; *SimpleExpressionKind* *[Properties](properties.md)*<sup>?</sup>\
> \
> *SimpleExpressionKind* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [NAMES](names.md)\
> &nbsp;&nbsp; | [NUMBER](numbers.md)\
> &nbsp;&nbsp; | *UnOp* *SimpleExpressionKind*\
> &nbsp;&nbsp; | *Exponentiation*\
> &nbsp;&nbsp; | *[ExplicitIterator](iterators.md)*\
> &nbsp;&nbsp; | *PointCollectionConstructor*\
>\
> *UnOp* :\
> &nbsp;&nbsp; `-`\
> \
> *Exponentiation* :\
> &nbsp;&nbsp; *SimpleExpressionKind* `^` `-`<sup>?</sup> *Exponent*\
> \
> *Exponent* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [INTEGER](numbers.md)\
> &nbsp;&nbsp; `(` [INTEGER](numbers.md) `/` [INTEGER](numbers.md) `)`\
> \
> *PointCollectionConstructor* :\
> &nbsp;&nbsp; `&` `(` *Expression&lt;false&gt;* (`,` *Expression&lt;false&gt;*)<sup>\*</sup> `)`

Expressions represent all values in GeoScript. A simple example of an expression is a variable reference with the variable's value or a number literal. After most expressions display options can be given, modifying how the expression affects the final figure visually.

## Names

Names can also be used as expressions. See [here](names.md) for more details.

## Operators
Binary operators all have the standard mathematical operation order. Unary operators always precede binary operators and implicit iterators always precede all operators. You can define your own order with parenthesis.

Currently, Geo-AID supports only addition, subtraction, multiplication and division as binary operators and negation as unary.

Weight in binary operations is applied to both of their operands. In unary operations, it is directly applied to their single operand.

## Exponentiation

Exponentiation takes precedence over every other operator, including negation. It can be written as `base^exp`, where `base` is the raised expression and `exp` is the exponent, either a literal integer or a fraction in the form `(nom / denom)` with `nom` and `denom` being integers and `denom` being nonzero. The exponent can also be negated by including a `-` in front of it (in case of fraction exponents, before the parenthesis).

When raising a value to a power, its unit is also raised.

Weights, like other properties are passed on to raised expressions.

## Point Collections

Expressions can also be used to construct point collections out of other expressions with `&(A, B, ...)` syntax. All expressions inside must be convertible to points.

Weights on point collections are treated as though they were applied to each of the collected points.