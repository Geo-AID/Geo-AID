# Command-Line Interface of Geo-AID

By default, whenever run, Geo-AID will generate a file in the same location with a different extension matching the requested format. If multiple formats were provided,
multiple files with different extensions are generated. This behavior can be modified with the `--output option`.

**Command Overview:**

* [`Geo-AID`](#Geo-AID)

## `Geo-AID`

**Usage:** `Geo-AID [OPTIONS] <INPUT> <OUTPUT>`

###### **Arguments:**

* `<INPUT>` — The input script file

###### **Options:**

* `-o`, `--output <DIR>` - Where the output file(s) should be created. Must point to an existing directory.

* `-d`, `--delta-max-mean <DELTA_MAX_MEAN>` — The maximum mean quality delta. Geo-AID will keep doing generation cycles
  until the average quality delta over the last `m` cycles gets below `d`.

  Default value: `0.0001`
* `-w`, `--worker-count <WORKER_COUNT>` — The count of threads to use for generation

  Default value: `32`
* `-s`, `--samples` - The count of samples to use for generation. Each engine interprets it differently

  Default value: `512`
* `-e`, `--engine` - The generation engine to use.

  Default value: `glide`

  Possible values:
    - `glide`:
      The gradient descent engine
    - `rage`:
      Random adjustment based engine
* `-m`, `--mean-count <MEAN_COUNT>` — The count of last deltas to include in mean calculation. Geo-AID will keep doing
  generation cycles until the average quality delta over the last `m` cycles gets below `d`.

  Default value: `128`
* `-s`, `--strictness <STRICTNESS>` — How strict the generator treats the rules. The higher, the more strict. Can't be
  zero.

  Default value: `2.0`
* `-a`, `--adjustment-max <ADJUSTMENT_MAX>` — Maximal adjustment of an adjustable during generation. Treated differently
  for different adjustables.

  Default value: `0.5`
* `-f`, `--format <FORMAT>` — What format to generate.

  Default value: `svg`

  Possible values:
    - `latex`:
      The LaTeX + tikz format
    - `svg`:
      The SVG format renderer
    - `json`:
      The JSON (machine-readable) format
    - `plaintext`:
      The plaintext (human-readable) format
    - `geogebra`:
      The GeoGebra workspace format (*.ggb)
  
  Multiple formats can be provided, each after a `-f` or `--format.`

* `--width <WIDTH>` — Canvas width (treated very differently for LaTeX)

  Default value: `500`
* `--height <HEIGHT>` — Canvas height (treated very differently for LaTeX)

  Default value: `500`
* `-l`, `--log <LOG>` — Where to put the log output. Geo-AID has a logging feature for concise information about the
  rendering process (quality and time).

<hr/>

<small><i>
This document was generated with the help of
<a href="https://crates.io/crates/clap-markdown"><code>clap-markdown</code></a>.
</i></small>
