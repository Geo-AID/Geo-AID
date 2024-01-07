# Rule operators

## Comparison

The operators `<`, `<=`, `>`, `>=` are only allowed between [Scalars](./types/primitives.md#scalar) of the same unit. They are simple comparison operators with their rules evaluated based on the relative difference between the two values.

The operator `=` (and its negation, `!=`) is allowed between [Scalars](./types/primitives.md#scalar) of the same unit and [Points](./types/primitives.md#point). Its rule is evaluated based on the absolute distance between the two values.

## `lies_on`

All uses accept `weight` property.

* `P: Point lies_on k: Line`

Tells Geo-AID that point `P` lies on (has zero distance) from line `k`. Note: zero distance rules do not have any impact on the distance variable and decrease figure stability much less than other distance rules.

* `P: Point lies_on k: Segment`

Tells Geo-AID that point `P` lies on (has zero distance) from the line of segment `k` and between its ends. Note: zero distance rules do not have any impact on the distance variable and decrease figure stability much less than other distance rules.

* `P: Point lies_on omega: Circle`

Tells Geo-AID that point `P` lies on (has zero distance) from circle `omega`. Note: zero distance rules do not have any impact on the distance variable and decrease figure stability much less than other distance rules.

* `col: 0-P lies_on omega: Circle`

Tells Geo-AID that points in the collection `col` lie on (have zero distance) from circle `omega` *in exactly the given order*. Note: zero distance rules do not have any impact on the distance variable and decrease figure stability much less than other distance rules.

**Note**: When negated, creates rules for the points not to be on the circle. Points that are on the circle, just not in the given order will not satisfy this rule.
