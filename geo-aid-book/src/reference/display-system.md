# Display system

The display system decides which expressions are displayed and which are not. Syntax documentation is available [here](syntax/display-properties.md).

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

## Properties on variable deinitions

Variable definitions display their defining expressions. Properties defined on definitions are passed onto the expression. Additionally, if no label is given, the variable name is parsed as a MathString and used as a label if the parse was successful (and if there is no `display_label=false`).

## Properties on rules

Currently, rules only accept a `display` property and display both of their sides.
