# Geoscript
Geoscript is a language allowing to describe a geometric figure. Its goal is to be simple, readable and easy to use. This document describes its syntax.

## Identifiers
**Identifiers** are used to reference points and other geometric constants. They can consist of small and capital letters, underscores, apostrophes and digits. They must begin with an underscore or a letter. If an identifier is composed only of capital letters and apostrophes, it's treated as a **point collection**. Otherwise it's a **named identifier**.

Those are **named identifiers**:
```
point
_A
xy'z
```

Those are **point collections**:
```
ABC
A'BC''
A
```

```
ident := [a-zA-Z_], {[a-zA-Z0-9_\']};
```

## Numbers
Numbers are series of digits possibly separated with a period token. They can define integers or rationals.

```
digit = [0-9];
number := digit, {digit}, ['.', digit, {digit}];
```

## Iterators
An iterator is a syntatic sugar for compressing multiple lines of code into one. It is done by creating variants of the specified statement by separating the variating parts with a '|' symbol. For example:

```
AB < 2.0;
BC < 2.0;

# Becomes

AB | BC < 2.0;
```

```
iterator<T> := T, {'|', T};
```

## Expressions
Expressions allow to join values with certain operations to alter them.

Example:
```
AB + BC
angle(ABC)
```

```
unop := unary-operator, simple-expr;
identexpr := ident;
numberexpr := number;
call := ident, '(', [expr, {',', expr}], ')';
parenthised := '(', expr, ')';
simple-expr := unop | identexpr | numberexpr | call | parenthised;
binop := expr, binary-operator, expr;
expr := iterator<simple-expr> | binop;
```

## Operators

### Expression operators
Expression operators are operators applicable in expressions.

```
predefined-unop := '-';
unary-operator := predefined-unop;
predefined-binop := '-' | '+' | '*' | '/';
binary-operator := predefined-binop;
```

### Rule operators
Rule operators are operators applicable in rules.

```
predefined-ruleop := '=' | '<', | '<=' | '>' | '>=';
rule-operator := ['!'], predefined-ruleop;
```

## Statements
Geoscript is a statement-based language. As of the current version, there are three (3) kinds of statements.

### Let statement
```
letstat := 'let', iterator<ident>, '=', expr, {rule-operator, expr}, ';';
```

A let statement defines a variable based on the right hand expression. After the expression there can be rules to be defined immediately on the created variable as a syntatic sugar. Let statements support iterators. All iterators must have uniform length and if the left hand side of the statement has no iterators, the expression defining the variable also must have no iterators.

If the left hand side of the statement is a point collection, the defining expression will be unpacked onto the collection if possible.

```
let A | |BC = Point() on DE | EF | DF;
```

### Rule statement
```
rulestat := expr, rule-operator, expr, ';';
```

A rule statement defines a rule - a criteria that must be met by the figure. All iterators here must be of uniform legth, only one rule operator is allowed.

### Noop statement
```
noop := ';';
```

### Statement
stat := letstat | rulestat | noop;

## Functions
Functions are called with the `call` syntax. They take arguments in and output a single value.

Example:
`let x = dst(A, B);`

## Conversions
GeoScript has implicit conversions defined for some types:
- A point collection of length 1 can be automatically converted into a point.
- A point collection of length 2 can be automatically converted into a line.
- A point collection of length 2 can be automatically converted into the distance between two points.
- A scalar with no specified type can be automatically converted into a scalar of any type.

## Scalars
Scalars are real numbers with a defined or not defined unit. Any number literal is by default a unitless scalar (not a dimensionless). Only unitless scalars can be freely converted into any other unit (except angless to avoid confusing radians with degrees). The units of GeoScript are:
- Distance
- Angle
- Scalar (dimensionless)