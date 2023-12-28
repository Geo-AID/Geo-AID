# Geo-AID

Geo-AID is a tool aiming to aid people at drawing geometry figures. It uses a figure script with criteria definitions to generate a figure meeting them and draw it. 

## Installation
To use and possibly install Geo-AID, you will need Rust with cargo. You can get those here: https://www.rust-lang.org/
Note: If you compile from source, you will also need to download the source for the geo_aid_derive crate (https://www.github.com/DragonGamesStudios/geo_aid_derive), main branch. The source should be in the parent folder of geo-aid:

```
| some_folder:
    | geo-aid
    | geo_aid_derive
```

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

The Geo-AID book is accessible under the `geo-aid-book/` directory. You can display it as HTML using [mdbook](https://github.com/rust-lang/mdBook):

```
cd geo-aid-book
mdbook serve --open
```