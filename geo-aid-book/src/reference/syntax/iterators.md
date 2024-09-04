# Iterators

> <sup>**Syntax**</sup>\
> *ImplicitIterator* :\
> &nbsp;&nbsp; [*Expression&lt;false&gt;*](expressions.md) (`,` [
*Expression&lt;false&gt;*](expressions.md))<sup>+</sup>\
> \
> *ExplicitIterator* :\
> &nbsp;&nbsp; `$` [INTEGER](numbers.md) `(` [*Expression&lt;false&gt;*](expressions.md) (`,` [
*Expression&lt;false&gt;*](expressions.md))<sup>*</sup> `)`

Iterators can be used in `let` statements and rules on both sides. A sequence of expressions separated by a comma is
called an *implicit iterator*. Using these, multiple lines of a script can be collapsed into a single one. For example:

```
let A, B, C = Point();

AB = AC, BC;
```

Iterators are expanded into multiple rules/statement by simply iterating over the given sequence. Implicit iterators
take precedence over any arithmetic operators. Here's a few examples:

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

The number after the dollar sign is the *id* of the iterator. If only a single id is used, they function just like
implicit iterators. However, when using multiple different ids, more complicated results can be achieved:

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

Iterators have a few rules. Not obeying them causes compilation errors.

* All implicit iterators have an id of 0;
* All iterators of the same id must have the same length;
* The left hand side of `let` statements only accept implicit iterators;
* The right hand side of `let` statements accepts at most one level of iteration;
* The right hand side of a `let` statement may only contain iterators, if so does the left side;
* All iterators must have at least two variants;
* An iterator with id x must not contain another iterator with id x;
* Iterator length must not exceed 255;
* Iterator id must be an integer;

