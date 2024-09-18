<picture>
  <source media="(prefers-color-scheme: dark)" srcset="images/logo-dark-mode.svg">
  <img alt="Geo-AID Logo" src="images/logo-light-mode.png">
</picture>

# Introduction

Geo-AID is a tool to generate figures based off of rules given in the script file. Its main purpose is to minimize the
pain related to drawing figures for certain mathematical problems or theorems. It's currently in the early development.

> **Note:** Geo-AID is not designed to produce *perfect* output. It is designed to produce *best* output. This means
> that it might sacrifice partial accuracy in favor of better readability. It may also make other compromises and produce
> unexpected results at times. If you're having trouble with this kind of behavior,
> visit [Dealing with complicated figures](./guide/complicated-figures.md)

As an entry point, Geo-AID uses GeoScript - a language used to describe a figure. Aside from that, special parameters
can be set as command line arguments. This book is meant to serve as a guide to anyone starting to use Geo-AID and as a
reference to anyone who wants to know more.

> **Note:** This book is held up-do-date with the latest released version
> on [crates.io](https://crates.io/crates/geo-aid).
