GeoScript features a standard library with constructs and operators to make most figures possible to generate easily.

## Types

### Point
A point on the standard plane.

### Line
A continuous set of points, where the relation between y coordinate difference and x coordinate difference of two points - also known as slope - is constant.

### Scalar
Scalars are real numbers with a defined or not defined unit. More info in syntax.md.

### Point Collection
A collection of points is a series of points that can be transformed into various types.

## Operators
GeoScript allows addition, subtraction, multiplication and division in terms of binary operators (+, -, *, /).
When it comes to unary operators, it allows negation (-).

### Rule operators
All comparison operators (<, >, =, <=, >=) are supported.

## Functions

### Point()
Returns a free point that is later adjusted by the generator.

### dst(dst-convertible)

**Params**
1. Any one argument that can be implicitly converted into a distance scalar

**Returns**
The converted input.

#### dst(point, point)
**Params**
Two points.

**Returns**
The distance between the inputs.

### dst(point, line), dst(line, point)
**Params**
A line and a point, any order.

**Returns**
The distance between them.