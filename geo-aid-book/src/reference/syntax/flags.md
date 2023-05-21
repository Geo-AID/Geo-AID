# Flags

> <sup>**Syntax**</sup>\
> *FlagStatement* :\
> &nbsp;&nbsp; *FlagName* *FlagValue*\
> \
> *FlagName* :\
> &nbsp;&nbsp; `@` [NAMED_IDENT](identifiers.md) (`.` [NAMED_IDENT](identifiers.md))<sup>\*</sup> `:`\
>\
> *FlagValue* :\
> &nbsp;&nbsp; &nbsp;&nbsp; [NAMED_IDENT](identifiers.md)\
> &nbsp;&nbsp; | *FlagSet*\
> &nbsp;&nbsp; | [NUMBER](numbers.md)\
>\
> *FlagSet* :\
> &nbsp;&nbsp; `{` *FlagStatement*<sup>\*</sup> `}`

Flags modify the behavior of Geo-AID's generator. They have default values, though some of them need to be explicitly specified to enable certain features (e. g. `distance_literals`).

A flag statement composes of the flag's name and its value. Each flag has a predefined type and will only accept values of that type. Identifier flags accept identifiers in general, though usually only a subset of identifiers is valid, representing certain behavior options. Boolean flags are used to enable or disable certain features/modifications to the standard behavior. They accept `1`, `true`, `enabled` and `yes` as a `true` value and `0`, `false`, `disabled` and `no` as a `false` value. Number flags may accept floats or integers, depending on the flag. Flag sets are special flags that categorize other flags. If you want to modify multiple flags of the same category, simply set the value of the parent set flag to a flag set with the respective statements.

Flag statements also accept a syntatic sugar for flag indexing. Instead of writing

```
@optimizations {
    @identical_expressions: false
}
```

You can simply write

```
@optimizations.identical_expressions: false
```