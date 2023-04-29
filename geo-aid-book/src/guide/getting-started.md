# Getting started

## Installation

Before you start using Geo-AID, you'll need to install it. Unfortunately, it does not come in the form of precompiled binaries and you'll need some other tools to build it. First, [install Rust and Cargo](https://www.rust-lang.org/). Once you're done, there are two ways of setting up Geo-AID:

The first way is to simply use the `cargo install` method:

```shell
cargo install geo-aid
```

This has the advantage of installing Geo-AID globally, so that you can run it from anywhere. It will take care of all dependencies for you. Building may take some time, but once it's done, it's done for good (unless you'll want to update it).

The second way is to clone/download the [GitHub repository](https://github.com/DragonGamesStudios/Geo-AID/tree/v0.2) (remember to get the last vX.X version) and build Geo-AID yourself. To download the repo, you'll need to download the .zip file and unpack it somewhere. If you want to clone it (recommended), you'll need [git](https://git-scm.com/). The clone way is shown below

```shell
git clone https://github.com/DragonGamesStudios/Geo-AID.git
cd Geo-AID
git checkout v0.2
```

Then, either build it with `cargo build --release` and use the produced executable or run it with `cargo run --release -- <geo-aid arguments here>`.

Run the program with the `--version` flag to check if it works properly. You can also run `geo-aid --help` (replace `geo-aid` with `cargo run --release -- --version` if using the second way) if you want to see how to use the tool CLI (you can also check the [CLI reference](../cli.md)).

The rest of this book will assume you have the command globally available (as if it was installed).

## Your first figure

In order to use Geo-AID, we have to tell it exactly what we want. We can do this with a script file. Geo-AID uses a special language called GeoScript that lets us give specific instructions. Create the following file with a name of your choice, say `figure.geo`:

```
let A = Point();
let B = Point();
let C = Point();

AB = AC;
AB = BC;
```

and run it with the following command:

```shell
geo-aid figure.geo figure.svg
```

After a short wait a new file `figure.svg` should show up. Open it in any SVG previewer (could even be your browser) and gaze at your first figure in awe:

<p><svg height="500" width="500" xmlns="http://www.w3.org/2000/svg"><circle cx="25" cy="475" fill="currentColor" r="2"/>
                <text stroke="currentColor" x="5" y="455">B</text><circle cx="454.2741962125423" cy="318.52307811824994" fill="currentColor" r="2"/>
                <text stroke="currentColor" x="434.2741962125423" y="298.52307811824994">C</text><circle cx="104.12383512354145" cy="25" fill="currentColor" r="2"/>
                <text stroke="currentColor" x="84.12383512354145" y="5">A</text></svg></p>

Ok, but what exactly happened here?
