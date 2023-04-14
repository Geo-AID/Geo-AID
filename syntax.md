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
Geoscript is a statement-based language. As of version 0.1.0, there are two (2) kinds of statements.

### Let statement
```
letstat := 'let', iterator<ident>, '=', expr, {rule-operator, expr}, ';';
```

### Rule statement
```
rulestat := expr, rule-operator, expr, ';';
```

### Statement
stat := letstat | rulestat;

## Functions
Functions are called with the `call` syntax. They take arguments in and output a single value.

Example:
`let x = dst(A, B);`
