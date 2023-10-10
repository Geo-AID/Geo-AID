# Command-Line Interface of Geo-AID

**Command Overview:**

* [`Geo-AID`](#Geo-AID)

## `Geo-AID`

**Usage:** `Geo-AID [OPTIONS] <INPUT> <OUTPUT>`

###### **Arguments:**

* `<INPUT>` — The input script file
* `<OUTPUT>` — The output target

###### **Options:**

* `-d`, `--delta-max-mean <DELTA_MAX_MEAN>` — The maximum mean quality delta. Geo-AID will keep doing generation cycles until the average quality delta over the last `m` cycles gets below `d`.

  Default value: `0.0001`
* `-c`, `--count-of-workers <COUNT_OF_WORKERS>` — The count of threads to use for generation

  Default value: `512`
* `-m`, `--mean-count <MEAN_COUNT>` — The count of last deltas to include in mean calculation. Geo-AID will keep doing generation cycles until the average quality delta over the last `m` cycles gets below `d`.

  Default value: `128`
* `-a`, `--adjustment-max <ADJUSTMENT_MAX>` — Maximal adjustment of an adjustable during generation. Treated differently for different adjustables.

  Default value: `0.5`
* `-r`, `--renderer <RENDERER>` — What renderer to use.

  Default value: `latex`

  Possible values:
  - `latex`:
    The LaTeX + tikz renderer
  - `svg`:
    The SVG format renderer
  - `json`:
    The JSON (machine-readable) format renderer
  - `raw`:
    The raw (human-readable) format renderer

* `--width <WIDTH>` — Canvas width (treated very differently for LaTeX)

  Default value: `500`
* `--height <HEIGHT>` — Canvas height (treated very differently for LaTeX)

  Default value: `500`
* `-l`, `--log <LOG>` — Where to put the log output. Geo-AID has a logging feature for concise information about the rendering process (quality and time).

<hr/>

<small><i>
    This document was generated with the help of
    <a href="https://crates.io/crates/clap-markdown"><code>clap-markdown</code></a>.
</i></small>
