# Getting started

## Installation

Before you start using Geo-AID, you'll need to install it. Unfortunately, it does not come in the form of precompiled
binaries, and you'll need some other tools to build it. First, [install Rust and Cargo](https://www.rust-lang.org/).
Once you're done, there are two ways of setting up Geo-AID:

The first way is to simply use the `cargo install` method:

```shell
cargo install geo-aid
```

This has the advantage of installing Geo-AID globally, so that you can run it from anywhere. It will take care of all
dependencies for you. Building may take some time, but once it's done, it's done for good (unless you'll want to update
it).

The second way is to clone/download the [GitHub repository](https://github.com/DragonGamesStudios/Geo-AID/tree/v0.2) (
remember to get the last vX.X version) and build Geo-AID yourself. In this, case, you will also need
the [geo_aid_derive](https://github.com/DragonGamesStudios/geo_aid_derive) source. To download the repos, you'll need to
download the .zip file and unpack it somewhere. If you want to clone it (recommended), you'll
need [git](https://git-scm.com/). The clone way is shown below

```shell
mkdir geo-aid
cd geo-aid
git clone https://github.com/DragonGamesStudios/Geo-AID.git
git clone https://github.com/DragonGamesStudios/geo_aid_derive.git
cd Geo-AID
git checkout v0.2
```

It's important that if you compile from source, you should preserve this file structure:

```
| some_folder:
    | geo-aid
    | geo_aid_derive
```

Then, either build it with `cargo build --release` and use the produced executable or run it with
`cargo run --release -- <geo-aid arguments here>`.

Run the program with the `--version` flag to check if it works properly. You can also run `geo-aid --help` (replace
`geo-aid` with `cargo run --release --` if using the second way) if you want to see how to use the tool CLI (you can
also check the [CLI reference](../cli.md)).

The rest of this book will assume you have the command globally available (as if it was installed).

## Your first figure

In order to use Geo-AID, we have to tell it exactly what we want. We can do this with a script file. Geo-AID uses a
special language called GeoScript that lets us give specific instructions. Create the following file with a name of your
choice, say `figure.geo`:

```
let A = Point();
let B = Point();
let C = Point();

AB = AC;
AB = BC;
```

and run it with the following command:

```shell
geo-aid figure.geo
```

After a short wait a new file `figure.svg` should show up. Open it in any SVG previewer (could even be your browser) and
gaze at your first figure in awe:

<p><img style="background-color: white;" src="first-figure.svg" alt="An equilateral triangle ABC"></p>

Ok, but what exactly happened here? Let's take a closer look at the script we've just given to Geo-AID:

First, we have the three `let` statements. These statements are used to create variables. In our case, these variables
are points created with the `Point()` function. You can also add special display properties to the variable definitions
to change how they are rendered. For example, if you change the first line to the following:

```
let A [label = G] = Point();
```

you should get something like this:

<p><img style="background-color: white;" src="renamed-a.svg" alt="An equilateral triangle BCG"></p>

You can find out more about the display system [here](../reference/display-system.md).

After the variable definitions, we have the two lines:

```
AB = AC;
AB = BC;
```

These are called rule statements. They represent a relationship between the left hand side and the right hand side. In
this case, the relationship is the equality of lengths AB, BC and AC. It's worth noting that the equality sign
represents a *rule*, not a *definition* or a *redefinition*. GeoScript is a description language, not a programming one.

Geo-AID takes these requirements and attempts to create a figure that meets them.

`let` statements and rules can be sometimes combined by adding rules *after* the left hand side of a `let` statement.
For example:

```
# This is a comment
let A = Point();
let B = Point();
let C = Point();
let r = dst(BC) < AB;

let omega = Circle(A, r);
```

Here, `r` is set to the distance `BC` and said to be smaller than `AB`.

## Expressions

Geo-AID mostly operates on expressions. They are variable definitions and both sides of rules. Expressions can be
mathematical operations, function calls, lines, distances and literals. All expressions produce values of certain types.
These can be divided into the following categories:

- primitives
- point collections
- bundle types

Primitives are points, lines, circles and scalars. They're what the generator operates on and what everything is
ultimately compiled into. Everything else is just an abstraction over these primitives. Additionally, scalars can have
units. Performing addition or subtraction on scalars with incompatible units is an error.

Point collections are sequences of point letters, like `AB`, `ABC`, `GFED`, `X`, `A'V`. For a name to be collectable, it
has to be a single, uppercase letter with an arbitrary number of ticks (`'`) following it, that represents a point.
Point collections can also be constructed using `&(...)` syntax. You can use them on the left-hand side of `let`
statements to unpack the rhs expression onto a point collection. Note, however, that not all types have that option.

```
let ABC = &(intersection(XY, GH), mid(G, H), intersection(TU, KL));
```

Bundle types are essentially like structs in programming languages. They have their primitive fields and functions
defined on them that can be used for more comfortable workflow.

### Implicit conversions

Geo-AID is capable of performing some implicit conversions:

* Unknown-unit scalars (usually literals) can be converted into a scalar with a distance unit.
* A point collection consisting of two points can be converted into a line or the distance between the two points,
  depending on the context.
* A point collection of length one is always automatically converted into a point.
* When performing multiplication/division over a scalar with a unit and a scalar with an unknown unit, the latter is
  automatically converted into a unitless scalar (standard scalar in mathematics).
* Any variable defined with an unknown-unit scalar is assumed to be unit-less.

## Shortening the code with iterators

Many figures feature multiple points and defining each one with a separate `let` statement can feel very verbose. To
help that, GeoScript has a powerful iterator system. Iterators can be used in `let` statements and rules on both sides.
A sequence of expressions separated by a comma is called an *implicit iterator*. Using these, we can collapse multiple
lines of a script into a single one. For example, our first figure script becomes the following:

```
let A, B, C = Point();

AB = AC, BC;
```

Iterators are expanded into multiple rules/statement by simply iterating over the given sequence. Note that implicit
iterators take precedence over binary arithmetic operators. Here's a few examples:

```
AB, BC = CD, EF;

# Becomes
AB = CD;
BC = EF;
```

```
AB < XY + YZ, OI;

# Becomes.
AB < XY + YZ;
AB < XY + OI;
```

whereas

```
AB < (XY + YZ), OI;

# Becomes

AB < XY + YZ;
AB < OI;
```

To use implicit iterators inside a function call, simply put parentheses around them: `intersection(AB, (KL, XY))`

Another type of iterator is an *explicit iterator*. These are written in the following way:

```
AB = $1(AC, BC);
```

The above example is equivalent to just writing

```
AB = AC, BC;
```

The number after the dollar sign is the *id* of the iterator. If you're using a single id, they function just like
implicit iterators. However, when using multiple different ids, you can get some interesting results:

```
$1(AB, CD) < $2(XY, YZ);

# Becomes
AB < XY;
CD < XY;
AB < YZ;
CD < YZ;
```

Explicit iterators can also be nested, allowing for even more complicated rules. For example:

```
$1(AB, BC) > $1(x, $2(a, b)), $3(9, 4, 3);

# Becomes
AB > x;
BC > a;
BC > b;
AB > 9;
AB > 4;
AB > 3;
BC > 9;
BC > 4;
BC > 3;
```

There are a few important things to remember about iterators:

* All implicit iterators have an id of 0;
* All iterators of the same id must have the same length;
* The left hand side of `let` statements only accept implicit iterators;
* The right hand side of `let` statements accepts at most one level of iteration;
* The right hand side of a `let` statement may only contain iterators, if so does the left side;
* All iterators must have at least two variants;
* An iterator with id x must not contain another iterator with id x;
* Iterator length must not exceed 255;
* Iterator id must be an integer;

Exact rules regarding the iterators can be found [here](../reference/syntax/iterators.md).
