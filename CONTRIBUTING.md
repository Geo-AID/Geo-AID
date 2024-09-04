# Contributing to Geo-AID

First and foremost, check out our [code of conduct](CODE_OF_CONDUCT.md). It is crucial that all development is done in a
nice and welcoming community.

## How is Geo-AID structured?

Geo-AID is divided into four main modules:

### The Compiler

GeoScript is a figure description language specifically designed for Geo-AID. The first module is responsible for
compiling that language into a format understood by the generator. Apart from the standard submodules of compilers - the
lexer and parser, Geo-AID has two very important steps before compilation:

#### **Unroll step**

Unrolling is responsible for removing most language abstractions. The only ones it leaves are bundles and point
collections (see: language documentation). At this point, specific description of what is actually displayed on the
figure is extracted and separated from figure definitions and rules. It also collects weighing information.

#### **Math step**

The math step is responsible for removing all remaining abstractions and performing optimizations on the processed
script. At this point the script should be as simplified as possible to increase figure stability and reduce required
computation. After this step, the figure can also be saved without further processing (not currently possible through
CLI).

### The Engine

The engine is responsible for actually generating the figure. Geo-AID currently supports two engines: *RAGE* - *Random
Adjustment Generation Engine* and *GLIDE* - *Gradient-Led Iterative Descent Engine*. The way they work is explained
below.

#### **Rage**

First, the math stage result is compiled into a critic program calculating the qualities of rules (how well each rule
has been satisfied) and a figure program calculating everything required for actually rendering the figure.

The generator is where the magic happens. The generation process consists of multiple cycles of the following form:

1. Adjust all adjustables
2. Evaluate adjustables' qualities

Adjustables are numbers representing certain objects of the figure (e.g. points). They are adjusted based on their
previously evaluated qualities. The quality influences magnitude, the direction is random.

Evaluation is done by first executing the evaluation script and then calculating the weighed sum of rule qualities for
each of the adjustables.

This process is performed by multiple workers (threads) and each step multiple samples are checked. After evaluation
each sample gets submitted with its adjusted values and the evaluation. The best one is picked and used as a base for
the next generation cycle. When the overall quality meets a certain criteria, the process stops and outputs the final
values for adjustables.

#### *Glide*

Glide is based on gradient descent. The math stage result is compiled into an error function calculating the overall
error of the figure as a value from 0 to positive infinity. The closer to 0, the better the figure.

A single descent consists of picking a random point in the optimisation space (random adjustable values) and finding the
gradient (multidimensional derivative) of the error function at that point. Then, the point is moved along the
gradient (in the opposite direction, we're trying to find the minimum) and the process repeats until no further
improvements can be made.

A single descent is fallible to local minima. However, since the process is fast and quite precise, it can be run many
times across thousands of samples, from which the best one is picked.

### The math backend

The compilation of mathematical expressions into programs takes place in the `geo-aid-math` subcrate.
It leverages cranelift to compile them into machine code to achieve near native speeds.

### The Projector

After generation, the projector is responsible for preparing the figure for drawing. Its job is to execute the figure
script and figure out the positions of all figure objects on the drawing canvas.

### Rendering in the desired format

Geo-AID has support for multiple formats as its output. Each format has a separate module where the code for rendering
it is.

## How can You contribute?

1. Battle testing

A very important part of Geo-AID development is testing it against different figures. You can for example test edge
cases or write tests for problems from different olympiads/books. Test writing is extremely helpful, as well as bug
reports.

2. Documentation

Geo-AID needs proper documentation for its language as one of its most important goals is to be as intuitive and easy to
use as possible. Help in finding documentation issues and unclarities, along with actually writing the docs is going to
be very appreciated.

3. Code contribution

Code contributions of any kind are welcome. We especially encourage bug fixes and drawer writing. Remember to always
attach a description of what you've changed, why you've changed it and why that is an improvement.
There are many places where Geo-AID needs your help. The language, the compiler, the generator(s), the projector, the
drawers. You can also focus on external projects. Do something of your own or contribute to already existing projects,
like the Geo-AID debugger.

When submitting a request, remember to always test if the code builds and to format the code. Also, please test the
correctness of your code. It can be done by running `test.py` with `python`. It runs all tests located in the `tests`
catalog.

4. Math

If you think you know how to improve or implement something from a mathematical standpoint, go ahead and contact me.
Geo-AID needs a very strong mathematical base to be able to serve its purpose.

5. Website

It would be great to have a website for Geo-AID. If you think you can help with that, go ahead. The repository hosting
the site is at https://github.com/Geo-AID/Geo-AID.github.io.

## How to write bug reports?

When submitting a bug report, always remember to include *the figure script* and *the command you ran Geo-AID with*.
Attaching what OS you ran Geo-AID on might also be helpful, especially with performance issues.