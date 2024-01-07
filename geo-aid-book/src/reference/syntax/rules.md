# Rules

> <sup>**Syntax**</sup>\
> *RuleStatement* :\
> &nbsp;&nbsp; *[Properties](properties.md)* *[Expression&lt;true&gt;](expressions.md)* *RuleOp* [Expression&lt;true&gt;](expressions.md)* `;`\
> \
> *RuleOp* :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` | `<=` | `=` | `>=` | `>`\
> &nbsp;&nbsp; | [IDENT](identifiers.md)\
> &nbsp;&nbsp; | `!` *RuleOp*

Rules are the basic building blocks of a figure. They define relationships between figure objects. Geo-AID attempts to generate a figure that obeys them as best as it can. Rules tie two expressions (left and right hand side) with a relationship, otherwise known as the rule operator. Currently supported rule operators are all comparison operators. When given an identifier, a proper *defined rule operator* is looked up and compiled accordingly (currently none supported). Rules can also be inverted with an exclamation mark in front of the operator.

Weights on non-ident rules are assigned to them directly and end up being used directly in the generation process. Weights on ident rules are treated differently depending on the rule. You should seek documentation on them in docs for respective operators.
