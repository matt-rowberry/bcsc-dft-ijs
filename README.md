# Hidden thermal signatures reveal the microclimatic influence of scheduled cave tourism

Code and data for the analysis of air-temperature records from six
monitoring points in Poole's Cavern (Buxton, Derbyshire, UK), 16 October
2019 to 15 October 2021. The pipeline reproduces Figures 6–11 of the
manuscript from the raw 10-minute temperature records.

> **Citation.** If you use this code or data, please cite the manuscript
> and this archive (see `CITATION.cff`). <!-- TODO: add manuscript
> reference and DOI once assigned -->

## Study design in brief

Each monitoring point's temperature series is modelled as a local-level
state-space model with fixed 24-, 12-, 8- and 6-hour harmonics. The
residuals (what the model does not explain) are then examined in the
frequency domain for recurring structure within the day, separately
for daytime (09:30–17:30, when the cave is open) and night, and across
five access regimes:

| Regime | From | To | Description |
|---|---|---|---|
| 1 | 2019-10-16 | 2020-03-18 | Normal pre-pandemic operation |
| 2 | 2020-03-19 | 2020-07-19 | First pandemic closure |
| 3 | 2020-07-20 | 2020-11-04 | Reopening between lockdowns |
| 4 | 2020-11-05 | 2021-05-17 | Second pandemic closure |
| 5 | 2021-05-18 | 2021-10-15 | Phased reopening (group limits of 6, then 20 from 19 July 2021) |

The analysis focuses on the 45.5–50.5 cycles-per-day band (periods of
about 28.5–31.6 minutes).

## Repository layout

```
measured/        Six temperature records as downloaded, one per monitoring point (input)
data/            Intermediate .rds files (created by the pipeline)
figs/            Figures 6–11 as 600 dpi .tif files (created by the pipeline)
logs/            Per-script logs from run_all.R (created by the pipeline)

00_ … 11_*.R     Pipeline scripts, run in the order below
analysis_functions.R   Shared band-pass and reconstruction functions
plot_helpers.R         Shared plotting theme, labels and layout
run_all.R              Runs the whole pipeline in order
session_info.txt       R and package versions used for the published figures
```

## Pipeline

```
measured/*.csv
  └─ 00_preprocess.R                regular 10-min series in UTC, sensor faults removed
      └─ 01_imputing.R              impute gaps of ≤ 6 h (imputeTS::na_kalman)
          └─ 02_kalman_filtering.R  state-space model, recursive residuals (KFAS)
              └─ 03_long_gaps.R     zero out calendar days with longer gaps
                  ├─ 04_day_night_split.R ──────────── Figure 6, Figure 7
                  ├─ 05_multiband_reconstruction.R ─── Figures 9, 10, 11
                  └─ (used directly) ───────────────── Figure 8, Figure 11
```

Scripts 00–05 are shared preprocessing. From Figure 7 onwards, each
figure has an `_analysis.R` script that writes an `.rds` to `data/`
and a `_plot.R` script that draws it to `figs/`.

| Figure | Content | Analysis script | Plot script |
|---|---|---|---|
| 6 | Residual temperature time series | – | `06_ts_plot.R` |
| 7 | Day and night amplitude spectra, 42–54 cpd | `07_fft_analysis.R` | `07_fft_plot.R` |
| 8 | Spectrograms, 45–51 cpd | `08_spectrogram_analysis.R` | `08_spectrogram_plot.R` |
| 9 | Reconstructed 45.5–50.5 cpd signal | `09_reconstruction_analysis.R` | `09_reconstruction_plot.R` |
| 10 | Range of the reconstructed signal per day/night segment | `10_segment_analysis.R` | `10_segment_plot.R` |
| 11 | RMS amplitude and variance explained | `11_variance_analysis.R` | `11_variance_plot.R` |

Figures 1–5 of the manuscript are not produced by this code.

## Running the analysis

1. Install R (the figures in this archive were produced with R 4.6.0
   on Windows 11) and the packages below.
2. Open `bcsc-dft-ijs.Rproj`, or set the working directory to the
   repository root.
3. Run:

   ```r
   source("run_all.R")
   ```

   or, from a terminal in the repository root, `Rscript run_all.R`.

`run_all.R` runs every script in its own fresh R session, in the order
shown above, starting from `measured/` alone. It writes a log for each
script to `logs/`. With default settings the whole run takes 5–6 hours
on a laptop, almost all of it in `08_spectrogram_analysis.R` (see
Parallel processing below); every other script takes seconds. To rerun only some scripts against existing intermediate
files, set `run_only` at the top of `run_all.R`.

Individual scripts can also be run on their own, in file-name order,
provided the scripts before them have been run first.

### Software requirements

R packages: `dplyr`, `extrafont`, `future`, `future.apply`,
`ggplot2`, `imputeTS`, `KFAS`, `lubridate`, `patchwork`, `phonTools`,
`purrr`, `readr`, `seewave`, `tidyr`.

```r
install.packages(c("dplyr", "extrafont", "future", "future.apply",
                   "ggplot2", "imputeTS", "KFAS", "lubridate", "patchwork",
                   "phonTools", "purrr", "readr", "seewave", "tidyr"))
```

The exact versions used for the published figures are listed in
`session_info.txt`. `phonTools` and `seewave` (Figure 8) are the most
likely to change behaviour between versions.

### Parallel processing

Scripts 01 and 02 process the six monitoring points in parallel with
`future::plan(multisession)`, using all but one available core.
`multisession` is used rather than `multicore` because `multicore` does
not work on Windows. The number of workers changes only the run time,
not the results.

`08_spectrogram_analysis.R` computes its six spectrograms one at a time
by default, because each needs several GB of memory and running all six
together can exhaust RAM on a laptop. On a machine with more memory,
raising `spectrogram_workers` at the top of that script runs several
at once and shortens the run roughly in proportion.

Each spectrogram is saved to `data/spectrogram_cache/` as soon as it is
finished. If the run is interrupted, running `run_all.R` again redoes
the quick scripts and then continues from the next unfinished
spectrogram. A saved spectrogram is used only if its input data, code
and package versions are unchanged; the folder is deleted when script
08 completes.

### Fonts

The figures use the Aptos font. On Windows, `plot_helpers.R` registers
it with `extrafont` the first time a plotting script runs, which can
take several minutes once. On other platforms this step is skipped and
ggplot2 falls back to its default font, so the figures differ in
typeface only.

The figures produced here plot exactly the same data as the published
figures, but their layout differs slightly: headings and axis labels now
have space of their own, so the panels are a few millimetres smaller.

## Data

The raw data are described in `measured/README.md`, and known
non-obvious details of the analysis in `NOTES.md`.

## Licence

<!-- TODO: code licence (e.g. MIT) in LICENSE; data terms in measured/README.md -->
