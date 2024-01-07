# Weight system

The weight system of Geo-AID is one of the core mechanisms of its generator. It directly modifies how much each adjustable will be affected by certain rules.

## How are weights computed?

As described [here](display-system.md), expressions in GeoScript are parsed into a tree-like structure. Every node of its tree has its assigned weight - by default equal to 1. Weights themselves, however, ultimately only apply to adjustables - values adjusted by the generator in the figure creation process. The weight system applies only to rules as they are the main set of instructions on how to generate a figure. Let's take a look at an example:

```
let A, B, C = Point();

AC = BC;
```

The above script is a simple description of an isosceles triangle. In order to understand weights, we must first understand adjustables.

Adjustables are values adjusted by the generator - this means free points, free scalars, points on lines, etc. An example of a function generating an adjustable is the `Point()` function. It creates a free point able to be adjusted in both dimensions. Here, we generate three different free points. One for each of `A`, `B`, and `C`.

After the definition there is a single rule: `AB = BC`. Its tree-like structure is the following:

```
- =
    - dst(A, C)
        - A
        - C
    - dst(B, C)
        - B
        - C
```

When computing weights, Geo-AID also performs definition expansion by substituting all variables with their definitions:

```
- =
    - dst(A, C)
        - A
            - Free Point 0
        - C
            - Free Point 2
    - dst(B, C)
        - B
            - Free Point 1
        - C
            - Free Point 2
```

Weights are computed seperately for each adjustable. Depending on which it is, nodes representing adjustables have different weights assigned to them. As an example, here, when computing weight for `Free Point 0` the `Free Point 2` node will have a weight of 0, whereas the `Free Point 0` node will have a weight of 1. All nodes not directly tied to a certain adjustable simply multiply the calculated weight by their assigned value. If two nodes are siblings, their weights are added. Here is a simple computation tree for `Free Point 2`:

```
- =                         (weight = 2)
    - dst(A, C)             (weight = 1)
        - A                 (weight = 0)
            - Free Point 0  (weight = 0)
        - C                 (weight = 1)
            - Free Point 2  (weight = 1)
    - dst(B, C)             (weight = 1)
        - B                 (weight = 0)
            - Free Point 1  (weight = 0)
        - C                 (weight = 1)
            - Free Point 2  (weight = 1)
```

You can check easily that the computed weights for the other two points will be both equal to 1. This way, the quality assigned to this rule will have twice as much effect on point `C` than on points `A` and `B`.

## How are weights applied?

By the time weight computation has finished, each rule has two parameters:
* its own assigned weight, independent of the weights of its operands' expressions;
* a weight assigned to each adjustable in the figure specifying how much the rule affects the adjustable.

When those two things are computed, each weight of the latter set of weights is *normalized*, that is: squeezed into the range `[0, 1]` by dividing each weight by the sum of them all. Finally, each weight is multiplied by the rule's assigned weight. This way each adjustable has a weight assigned to each rule.

When rules are evaluated, they are given a quality in range `[0, 1]`.  This way, for each adjustable, all values can be put into pairs `(quality, weight)` for each rule. From that a weighed mean is calculated (sum of the products `quality * weight` divided by the sum of weights). The result is the final quality of an adjustable.

The final quality affects how much an adjustable is adjusted when making corrections.

In short:
* Expression weights affect how much a given adjustable is affected by the rule's quality in comparison to other adjustables.
* Rule weights affect how much a given adjustable is affected by the rule's quality in comparison to other rules.

## How to modify weights?

In general, weights are modified by adding a `number`-type `weight` property in square brackets:

```
[weight = 2]
AB = BC [weight = 1.5];
```

Specifics regarding how do weight properties affect certain expressions or rules are in their respective documentations.