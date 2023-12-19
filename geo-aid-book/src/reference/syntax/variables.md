# Variables

> <sup>**Syntax**</sup>\
> *LetStatement* :\
> &nbsp;&nbsp; `let` *VariableDefinition* (`,` *VariableDefinition*)<sup>\*</sup> `=` *[Expression&lt;true&gt;](expressions.md)* (*[RuleOp](rules.md)* *[Expression&lt;true&gt;](expressions.md)*)<sup>\*</sup> `;`\
> \
> *VariableDefinition* :\
> &nbsp;&nbsp; [IDENT](identifiers.md) *[DisplayProperties](display-properties.md)*<sup>?</sup>

A let statement creates variables given on the left hand side. The lhs of the statement can contain multiple variables. In that, case if the rhs has no iteration, all variables will be set to the given definition (no the same value, though). If there is one level of iteration, all variables will get their respective definition. More levels of iteration are not allowed.

The rhs expression of the statement can either become the variable's definition or it can be unpacked onto a point collection.

After each variable name there can be display options given, modifying how the construct is displayed.

The let statement accepts rules after its right hand side. They behave as if the lhs was a sequence of variable accesses in a 0-id iterator.