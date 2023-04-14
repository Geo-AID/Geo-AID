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

**Point collections** can also be constructed using the following syntax:

```
ABC

# Becomes

&(A, B, C)
```

These expressions allow creating point collections from other expressions, such as

```
&(A, intersection(BC, DE))
```

```
collection-constructor := '&', '(', expr(no implicit iterators), {',', expr(no implicit iterators)}, ')';
```

## Numbers
Numbers are series of digits possibly separated with a period token. They can define integers or rationals.

```
digit = [0-9];
number := digit, {digit}, ['.', digit, {digit}];
```

## Iterators
An iterator is a syntatic sugar for compressing multiple lines of code into one. One way to do it is to create variants of the specified statement by separating the variating parts with a ',' symbol. For example:

```
AB < 2.0;
BC < 2.0;

# Becomes

AB, BC < 2.0;
```

```
implicit-iterator<T> := T, {',', T};
```

This iterator, however, cannot appear in functions or explicit iterators. It also only supports one level of iteration. For those cases, GeoScript supports creating explicit iterators using the following syntax:

```
AB < 2.0;
BC < 2.0;

# Becomes

$0(AB, BC) < 2.0;
```

An explicit iterator starts with the `$` symbol followed by an id and then by the iterator variants in parentheses. The id of an iterator must be ab integer smaller than 256. Implicit iterators always have an id of 0. Iterators with varying ids allow creating more levels of iteration. For example:

```
$1(AB, BC) < $2(CD, DE);


# becomes
AB < CD;
AB < DE;
BC < CD;
BC < DE;
```

```
explicit-iterator<T> := '$', digit, {digit}, '(', T, {',', T}, ')';
```

All iterators must have at least two variants.

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
call := ident, '(', [expr(no implicit iterators), {',', expr(no implicit iterators)}], ')';
parenthised := '(', expr, ')';
simple-expr := unop | identexpr | numberexpr | call | parenthised | explicit-iterator<expr(no implicit iterators)>;
binop := expr, binary-operator, expr;
expr := implicit-iterator<simple-expr> | simple-expr | binop;
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
letstat := 'let', implicit-iterator<ident>, '=', expr, {rule-operator, expr}, ';';
```

A let statement defines a variable based on the right hand expression. After the expression there can be rules to be defined immediately on the created variable as a syntatic sugar. Let statements support iterators. All iterators must have uniform length and if the left hand side of the statement has no iterators, the expression defining the variable also must have no iterators.

If the left hand side of the statement is a point collection, the defining expression will be unpacked onto the collection if possible.

```
let A, B, C = Point() on DE, EF, DF;
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

### Compiler flags
Compiler flags alter the behavior of compiler and generator. They can enable or disable certain optimizations, change how a specific operation is treated and toggle certain features. They can be expressed using the following syntax:

```
@flag_name: flag_value
```

Flag value can either be a literal value or a flag set, allowing to group flags into categories. A literal value is either an identifier or a number. And a flag set is a list of flag statements like the following.

```
@flag_set1: {
    @flag: value,
    @flagset2: {
        ...
    }
}
```

```
flagvalue := ident | (digit, {digit}) | ('{', {flagstat}, '}'); 
flagstat := '@', ident, ':', flag-value;
```

Most flags require certain types. String flags accept identifiers, though sometimes there may be very limited options as to what strings exactly are allowed. Number flags sometimes want integers, sometimes floats and boolean flags can accept "on", "enabled", "true" and "1" as true and "off", "disabled", "false" and "0" as false.

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
- A scalar with no specified type can be automatically converted into a scalar of any type, except for angles.

## Scalars
Scalars are real numbers with a defined or not defined unit. Any number literal is by default a unitless scalar (not a dimensionless). Only unitless scalars can be freely converted into any other unit (except angless to avoid confusing radians with degrees). The units of GeoScript are:
- Distance
- Angle
- Scalar (dimensionless)