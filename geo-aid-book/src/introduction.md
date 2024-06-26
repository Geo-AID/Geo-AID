# Introduction

Geo-AID is a tool to generate figures based off of rules given in the script file. Its main purpose is to minimize the pain related to drawing figures for certain mathematical problems or theorems. It's currently in the early development.

> **Note:** Geo-AID is not designed to produce *perfect* output. It is designed to produce *best* output. This means that it might sacrifice partial accuracy in favor of better readability. It may also make other compromises and produce unexpected results at times. If you're having trouble with this kind of behavior, visit [Dealing with complicated figures](./guide/complicated-figures.md) 

As an entry point, Geo-AID uses GeoScript - a language used to describe a figure. Aside from that, special parameters can be set as command line arguments. This book is meant to serve as a guide to anyone starting to use Geo-AID and as a reference to anyone who wants to know more.

> **Note:** This book is held up-do-date with the latest released version on [crates.io](https://crates.io/crates/geo-aid).

## How does Geo-AID work?

First, the input script is compiled into a series of rules and definitions. All type-checking happens here. Geo-AID will also try to apply certain optimizations, if possible. Then, the compiled rules are fed to the generator. The generator itself consists of two core parts - the "Magic Box" (which got its name when Geo-AID was just an idea on a piece of paper) and the critic. The image data (point locations etc.) is then generated over tens of thousands of generation cycles. A single cycle consists the following steps:

* The current version of the data passes through the Magic Box, which adjusts the points/reals (adjustables) by a random vector based on the adjustable's quality.
* The adjusted data is then rated by the critic, which checks how well the new figure meets the given requirements (rules) and gives each adjustable a score in 0-1 range.
* If the new data is considered better than the previous one, it replaces the current version. Otherwise, it gets discarded.

The above cycle runs for each of the 512 (by default) threads and after each cycle a new version gets chosen (or all new versions get discarded). The generation process lasts until a certain requirement is met, e.g. the average overall figure quality increase over the last few thousands of cycles does not exceed a certain value.

After an image is generated, the data goes to the projector, which designs how it should be rendered and passed on to the selected renderer, which creates the final output.