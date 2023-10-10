# Display Options

> <sup>**Syntax**</sup>\
> *Display Properties* :\
> &nbsp;&nbsp; `[` *Property* (`;` *Property*)<sup>\*</sup> `]`\
> \
> *Property* :\
> &nbsp;&nbsp; [NAMED_IDENT](identifiers.md) `=` *PropertyValue*\
> \
> *PropertyValue* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [NUMBER](numbers.md)\
> &nbsp;&nbsp; | [IDENT](identifiers.md)

Display options are a simple sequence of key-value pairs used to represent certain options regarding how the figure should be displayed. They're accepted in expressions and variable definitions. Display options with invalid values will cause an error and the ones with invalid names will be ignored (likely to change in the coming versions).
