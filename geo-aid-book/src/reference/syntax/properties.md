# Properties

> <sup>**Syntax**</sup>\
> *Properties* :\
> &nbsp;&nbsp; `[` *Property* (`;` *Property*)<sup>\*</sup> `]`\
> \
> *Property* :\
> &nbsp;&nbsp; [NAMED_IDENT](identifiers.md) `=` *PropertyValue*\
> \
> *PropertyValue* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [NUMBER](numbers.md)\
> &nbsp;&nbsp; | [IDENT](identifiers.md)\
> &nbsp;&nbsp; | STRING\
> &nbsp;&nbsp; | *RawString*\
> \
> *RawString* :\
> &nbsp;&nbsp; `!` STRING

## Property values

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

### `number`

Number values accept positive integers and floats. They cannot be expressed by idents or strings. Used for weights.

### `Style`

Style properties tell Geo-AID how to display a given line or a circle. Available options are: `SOLID`, `DASHED`, `BOLD`, and `DOTTED`. They can be represented using identifiers or non-raw strings. When parsed, case is ignored.

### `MathString`

MathString properties usually represent label contents. MathStrings are used to write normal text while also allowing lower indices and a restricted set of mathematical characters, like greek letters.

*Parsing*

**Identifiers**

If the identifier is a single character or a character code representing a letter (character codes explained below), a number of primes (also explained below), and a `_` followed by digits, it can be parsed as a MathString containing only that character.

Examples:

```
A
B_12
C'
D''_456
```

**Raw strings**

Raw strings are parsed as a set of ASCII characters without any additional processing. Useful for injecting LaTeX into point labels, should it be necessary.

Examples:

```
!"\mathbb{X}^\prime"
!"Hello, World!"
!"_{}}}Everything is literal}"
```

**Strings**

Strings are parsed like raw strings with a few important exceptions:

* Single quotes (`'`) are parsed as primes;
* Everything directly after a `_`, until, but not including, a space, is parsed as being in lower index;
* Lower index cannot be used inside a lower index;
* Longer text with spaces can be put inside a lower index if delimited by braces (`{}`);
* Text inside brackets (`[]`) is parsed as a character code and outputs a special character with that code;
* `\\` before a character inserts that character regardless of the above rules (it does not, however, enable using `"` in a string. You can use `[quote]` for that purpose).

Examples:

```
"A"
"B_12"
"C'_{Hello, World!}"
"[Alpha] [quote]label [alpha][quote]"
```

*Character codes*

Character codes are used to represent special characters. Currently, Geo-AID only supports greek letters - in form of the names of those letters, where the case of the first letter decides the case of the output letter - and quotes (`"`), written as `qoute`.

*Primes*

Primes, in MathStrings, are ticks often seen beside points. They are often used to represent a point after certain transformations, like symmetry or rotation (looks like `A'`). In MathStrings, all non-escaped (`\\`) single quotes (`'`) are treated as those.

## LineType

`LineType` describes whether a line should be displayed as a continuous line, a ray or a segment. This property is allowed in a few functions. Possible values are: `LINE`, `RAY`, `SEGMENT`. The default depends on the context.