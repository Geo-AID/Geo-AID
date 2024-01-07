# Ref statements

> <sup>**Syntax**</sup>\
> *RefStatement* :\
> &nbsp;&nbsp; &nbsp;&nbsp; *[Properties](properties.md)* `?` *[Expression&lt;true&gt;](expressions.md)* `;`

Ref statements can be used to display expressions without any side effects.

## Properties of refs

If provided a non-zero `weight` property, a ref statement generates a *bias rule*. Bias rules are rules that are always true. They can be used to artificially make certain adjustables more stable. Beyond that, any display properties defined on them are treated like properties of the ref-ed expression.