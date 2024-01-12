# Identifiers

> <sup>**Lexer**</sup>\
> IDENT :\
> &nbsp;&nbsp; &nbsp;&nbsp; NAMED_IDENT\
> &nbsp;&nbsp; | POINT_COLLECTION\
> \
> NAMED_IDENT :\
> &nbsp;&nbsp; Start Continue<sup>\*</sup>\
> \
> POINT_COLLECTION :\
> &nbsp;&nbsp; (Point `'`<sup>\*</sup>)<sup>+</sup>

Where `Start` is any unicode character with the *Alphabetic* property or an underscore (`_`) character, `Continue` is `Start` or a tick (`'`) character and `Point` is any unicode character with the *Uppercase* property.

Identifiers mostly represent variables, though they may also serve as a rule operator, a function, a value for a display property or a flag value. See also: [names](names.md).

Point collections are a special kind of identifiers. They essentially represent a sequence of variables, each being a point.