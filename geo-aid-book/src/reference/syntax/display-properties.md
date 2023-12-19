# Display Properties

> <sup>**Syntax**</sup>\
> *Display Properties* :\
> &nbsp;&nbsp; `[` *Property* (`;` *Property*)<sup>\*</sup> `]`\
> \
> *Property* :\
> &nbsp;&nbsp; [NAMED_IDENT](identifiers.md) `=` *PropertyValue*\
> \
> *PropertyValue* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [NUMBER](numbers.md)\
> &nbsp;&nbsp; | [IDENT](identifiers.md)\
> &nbsp;&nbsp; | [STRING](strings.md)\
> &nbsp;&nbsp; | *RawString*\
> \
> *RawString* :\
> &nbsp;&nbsp; `!` [STRING](string.md)

## Property types

### `bool`

Boolean properties represent true or false for certain properties. A true value can be represented as the following:

```
1, true, enabled, on, "true", "enabled", "on"
```

A false value can be represented like this:

```
0, false, disabled, off, "false", "disabled", "off"
```

**NOTE**: Cannot be represented by a raw string.
**NOTE**: In case of boolean values, parsing of identifiers and strings is case-insensitive.

### `Style`

Style properties tell Geo-AID how to display a given line or a circle. Available options are: `SOLID`, `DASHED`, `BOLD`, and `DOTTED`. They can be represented using identifiers or non-raw strings. When parsed, case is ignored.

### `MathString`

MathString properties usually represent label contents. MathStrings are used to write normal text while also allowing lower indices and a restricted set of mathematical characters, like greek letters.

*Parsing*

**Identifiers**

If the identifier is a single character or a character code representing a letter (character codes explained below) and a number or primes (also explained below), it can be parsed as a MathString containing only that character.

**Raw strings**

Raw strings are parsed as a set of ASCII characters without any additional processing. Useful for injecting LaTeX into point labels, should it be necessary.

**Strings**

Strings are parsed like raw strings with a few important exceptions:

* Single qoutes (`'`) are parsed as primes;
* Everything directly after a `_`, until, but not including, a space, is parsed as being in lower index;
* Lower index cannot be used inside a lower index;
* Longer text with spaces can be put inside a lower index if delimited by braces (`{}`);
* Text inside brackets (`[]`) is parsed as a character code and outputs a special character with that code;
* `\\` before a character inserts that character regardless of the above rules (it does not, however, enable using `"` in a string. You can use `[quote]` for that purpose);

*Character codes*

Character codes are used to represent special characters. Currently, Geo-AID only supports greek letters - in form of the names of those letters, where the case of the first letter decides the case of the output letter - and quotes (`"`), written as `qoute`.

*Primes*

Primes, in MathStrings, are ticks often seen beside points. They are often used to represent a point after certain transformations, like symmetry or rotation (looks like `A'`). In MathStrings, all non-escaped (`\\`) single quotes (`'`) are treated as those.

## What is displayed?

Most expressions accept a `display` (`bool`) property, that has a default value based on the *constructive*-ness of the expression. Beyond that, expressions have a tree-like structure. For example, an expression representing an orthocenter of triangle ABC.

```
intersection(perpendicular_through(AB, C), perpendicular_through(BC, A))
```

It's semantic structure is the following:

```
- intersection
    - perpendicular_through
        - AB (line)
            - A
            - B
        - C
    - perpendicular_through
        - BC (line)
            - B
            - C
        - A
```

Now, the value of the `display` property of a node in that tree (e. g. the first `perpendicular_through`) decides not only whether the expression itself is displayed, but also whether its child nodes (the `AB` and `C` in our examples) are displayed.

Display properties are a simple sequence of key-value pairs used to modify how the figure should be displayed. They're accepted in expressions, rules and variable definitions. Display properties with invalid values or unexpected properties will cause an error and the ones with invalid names will be ignored.

The principle the display system works with is that *an expression is displayed by default iff it's constructive*.

## What does it mean to display an expression?

To display an expression means to display its visual representation in its final figure. As simple example, to display a `bisector(ABC)` is to add a line representing the bisector of the angle ABC to the output figure.

## What is a constructive expression?

A constructive expression is one that *constructs* a new object: a point, a line, etc. In practice, only variable references are non-constructive GeoScript - that is, referencing a variable either through a name or through a point collection won't display anything related to that variable (note that anything that could be displayed with it, should already be marked for display while processing the definition). It is however worth noting that point collection construction *is* constructive. Additionally, collections of length 2 *are* constructive, as they are converted to a different type (a line or a distance). Certain expressions, even though constructive, don't expect any properties simply because there's nothing to display. An example of that is a literal number.

## Basic properties for types

All [types](../types.md) have their basic properties assigned to them. `Point`s, `Line`s, `Circle`s, `Scalar`s, `PointCollection`s and bundle types accept the `display` (`bool`) property. `Point`s, `Line`s and `Circle`s accept additionally `label` (`MathString`) and `display_label` (`bool`) properties. `Point`s accept a `display_dot` (`bool`). `Line`s and `Circle`s also accept `style` (`Style`) property. Variables and literals don't accept any properties, no matter the type. Beyond that, additional properties may be added depending on the kind of construction (used function). Details on those are in the documentation of respective functions.
