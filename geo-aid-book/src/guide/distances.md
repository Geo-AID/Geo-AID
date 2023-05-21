# Working with distances

**NOTE: EVERYTHING REGARDING THIS CHAPTER IS VERY LIKELY TO CHANGE IN THE COMING UPDATES.**

If we run the following code in Geo-AID:

```
let A, B, C = Point();

AB, BC, AC = 1, 2, 3;
```
We'll get the following compiler error:

```
error: you must set a value for flag `distance_literals`.
 --> test.geo:3:14
  |
3 | AB, BC, AC = 1, 2, 3;
  |              ^ note: Required because of this line.
  |              ^ help: possible values: `adjust`, `solve`
Fix: Consider defining this flag at the top of the file.
 --> test.geo:1:1
  |
1 | @distance_literals: adjust;
  | +++++++++++++++++++++++++++
  | let A, B, C = Point();
```

Why is that?

Geo-AID operates on its own coordinate system and has to accept figures with lengths specified in thousands, as well as lengths specified in numbers lower than 1. Because of this, distance literals have to be treated in a special way. Geo-AID handles them by creating a special variable representing a distance unit. This variable then mutliplies every distance literal. For that to work, the compiler has to be informed that you are aware of using distance literals by adding a flag.

## Flags

Flags can modify the behavior of the generator. They are grouped in categories and can be set two different ways.

```
# Way 1:
@optimizations {
    @identical_expressions: false;
};

# Way 2:
@optimizations.identical_expressions: false;
```

This time, we'll want to set the `distance_literals` flag to `adjust`. (`solve` is currently not supported), as the fix proposal in the compiler error message suggets:

```
@distance_literals: adjust;
let A, B, C = Point();

AB, BC, AC = 1, 2, 3;
```

Doing so will make Geo-AID create an adjustable scalar that will serve as the distance unit.
