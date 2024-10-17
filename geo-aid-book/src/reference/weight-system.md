# Weight system

The weight system of Geo-AID is one of the core mechanisms of its generator. It directly modifies how much each adjustable
will be affected by certain rules.

## How are weights computed?

Every entity present in a rule gets assigned a weight - by default equal to 1. Weights themselves, however, ultimately
only apply to adjustables - values adjusted by the generator in the figure creation process. The weight system applies
only to rules as they are the main set of instructions on how to generate a figure. Let's take a look at an example:

```
let A, B, C = Point();

AC = BC;
```

The above script is a simple description of an isosceles triangle. In order to understand weights, we must
first understand adjustables.

Adjustables are values adjusted by the generator - this means free points, free numbers, points on lines, etc.
An example of a function generating an adjustable is the `Point()` function. It creates a free point able to
be adjusted in both dimensions. Here, we generate three different free points. One for each of `A`, `B`, and `C`.

After the definition there is a single rule: `AB = BC`. As you can see, there are three adjustables here:
`A`. `B`, `C`. Despite `B` appears twice, all adjustables get the same weight of 1.

## How are weights applied?

By the time weight computation has finished, each rule has a weight assigned to each adjustable in the figure specifying
how much the rule affects the adjustable.

When it is all computed, each weight of the latter set of weights is *normalized*, that is: squeezed into the range `[0, 1]`
by dividing each weight by the sum of them all. Finally, each weight is multiplied by the rule's assigned weight. This way
each adjustable has a weight assigned to each rule.

When rules are evaluated, they are given a quality in range `[0, 1]`.  This way, for each adjustable, all values can be put
into pairs `(quality, weight)` for each rule. From that a weighed mean is calculated (sum of the products `quality * weight` divided by the sum of weights). The result is the final quality of an adjustable.

The final quality affects how much an adjustable is adjusted when making corrections.

In short, rule weights affect how much a given adjustable is affected by the rule's quality in comparison to other rules.

## How to modify weights?

In general, weights are modified by adding a `number`-type `weight` property in square brackets:

```
[weight = 2]
AB = BC;
```

Specifics regarding how do weight properties affect certain rules are in their respective documentations.
