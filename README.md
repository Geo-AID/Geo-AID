![Geo-AID Logo](logo/full-white.svg#gh-dark-mode-only)
![Geo-AID Logo](logo/full-black.svg#gh-light-mode-only)

# Geo-AID

Geo-AID is a tool aiming to aid people at drawing geometry figures. It uses a figure script with criteria definitions to generate a figure meeting them and draw it.

Usually tools for figure generation are based on constructions (e.g. GeoGebra). In many cases they're more than enough to get a nice-looking figure, but there are problems out there with figures that are either difficult or straight up impossible to get right in those tools, let alone on paper. Geo-AID addresses that issue. Instead of limiting the user to constructive expressions, it provides a wide range of constructive functions aside a rule system, using which a user can enter rules that a figure must meet (e. g. angle equality, point collinearity etc.). Being an optimization engine at its core, Geo-AID can then take such a description and generate a figure based on it as best as it can. With some problems it does very well, with others - not so much, but the whole tool is under heavy development with constant improvements.

## Installation
To use and possibly install Geo-AID, you will need Rust with cargo. You can get those here: https://www.rust-lang.org/.

Once done, you can either clone the repository with git and run Geo-AID using
```
cargo run --release -- <geo-aid arguments here, type --help for help>
```

Or install Geo-AID with
```
cargo install geo-aid
```

and run it with
```
geo-aid <arguments here, type --help for help>.
```

## Useful resources

The Geo-AID book is hosted at https://geo-aid.github.io/. It's also available under the `geo-aid-book/` directory. You can display it as HTML using [mdbook](https://github.com/rust-lang/mdBook):

```
cd geo-aid-book
mdbook serve --open
```

## Goals

The ultimate goal of Geo-AID is to reduce the pain related to figure drawing to bare minimum. It aims to allow writing and generating complicated figures and rendering them in various formats the user may find useful (primarily LaTeX, SVG, GGB (WIP)). It should enable article and book writers to put figures in their works without too much of a hassle, aid teachers in conducting helpful geometry lessons and lectures, and help students (especially those starting in olympiads) learn their craft.

## How to contribute?

If You're interested in contributing to Geo-AID, feel free to check out the [contribution guide](CONTRIBUTING.md).