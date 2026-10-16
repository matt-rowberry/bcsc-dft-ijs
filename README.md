# Hidden thermal signatures reveal the microclimatic influence of scheduled cave tourism

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23187187.svg)](https://doi.org/10.5281/zenodo.23187187)

Code and data for the analysis of air-temperature records from six
monitoring points in Poole's Cavern (Buxton, Derbyshire, UK), 16 October
2019 to 15 October 2021. The pipeline reproduces Figures 6–11 of the
article from the raw 10-minute temperature records.

> **Citation.** If you use this code or data, please cite the article
> and this archive:
>
> Rowberry, M., Gunn, J., Smith, A., 2026. Hidden thermal signatures
> reveal the microclimatic influence of scheduled cave tourism.
> *International Journal of Speleology*, 55(3), ijs2630.
> <https://doi.org/10.5038/1827-806X.ijs2630>
>
> Rowberry, M., Gunn, J., Smith, A., 2026. Code and data for "Hidden
> thermal signatures reveal the microclimatic influence of scheduled
> cave tourism" (Version 1.0.0). Zenodo.
> <https://doi.org/10.5281/zenodo.23187187>

## Study site

Poole's Cavern is a show cave and Site of Special Scientific Interest in
Buxton, Derbyshire (entrance at 53°15'0.67"N, 1°55'36.74"W). Its passages
extend for at least 240 m, and the confined entrance passage is the only
known route for cave ventilation. About 50,000 visitors a year are
guided along a tourist path with seven stops. Since 2018 the British Cave
Science Centre has run a network of Tinytag Connect data loggers in the
cave, recording every 10 minutes; the six air-temperature records used
here are described in `measured/README.md`.

## Study design in brief

The record runs from 16 October 2019 to 15 October 2021 and spans five
access regimes created by the COVID-19 restrictions:

| Regime | From | To | Visitor access |
|---|---|---|---|
| 1 | 2019-10-16 | 2020-03-18 | Normal pre-pandemic operation: guided tours on an ad hoc basis, according to demand and guide availability |
| 2 | 2020-03-19 | 2020-07-19 | First closure: occasional inspections and lighting maintenance only |
| 3 | 2020-07-20 | 2020-11-04 | Reopening between lockdowns: guided tours every 30 minutes, at most 12 visitors per group |
| 4 | 2020-11-05 | 2021-05-17 | Second closure: no routine visitor access |
| 5 | 2021-05-18 | 2021-10-15 | Phased reopening: guided tours every 30 minutes, at most 6 visitors per group until 18 July 2021 and at most 20 from 19 July |

For each monitoring point, the analysis:

1. aligns the logger readings to a common 10-minute UTC time axis and
   imputes gaps of up to 6 hours by Kalman smoothing (scripts 00–01);
2. models the temperature as a stochastic local level plus the first
   four harmonics of the 24-hour cycle, and keeps the one-step-ahead
   prediction errors as the residual short-timescale variability
   (02–03); these act as a high-pass filter of the temperature (see
   `NOTES.md`);
3. splits the residuals by access regime and into daytime (09:30–17:20
   UK local time) and night-time, and examines their spectra (04, 07),
   spectrograms (08) and the band-limited inverse-Fourier
   reconstruction of the 45.5–50.5 cycles-per-day band, i.e. periods of
   about 28.5–31.6 minutes, matching tours every 30 minutes (05, 09);
4. quantifies the reconstructed signal by its range in each day or
   night segment, its RMS amplitude and the share of residual variance
   it explains (10–11).

## Repository layout

```
measured/        Six temperature records as downloaded, one per monitoring point (input)
data/            Intermediate .rds files (created by the pipeline)
figs/            Figures 6–11 as 600 dpi .tif files, the values of Figure 11
                 as 11_variance_values.csv, and the fitted model parameters
                 as 02_kalman_parameters.csv (created by the pipeline)
logs/            Per-script logs from run_all.R (created by the pipeline)

00_ … 11_*.R     Pipeline scripts, run in the order below
analysis_functions.R   Shared band-pass and reconstruction functions
plot_helpers.R         Shared plotting theme, labels and layout
run_all.R              Runs the whole pipeline in order
session_info.txt       R and package versions used for the figures in the paper
```

## Pipeline

```
measured/*.csv
  └─ 00_preprocess.R                regular 10-min series in UTC, sensor faults removed
      └─ 01_imputing.R              impute gaps of ≤ 6 h (imputeTS::na_kalman)
          └─ 02_kalman_filtering.R  state-space model, recursive residuals and
                                    fitted parameters (KFAS)
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
| 6 | Residual temperature series after removal of the diurnal cycle and the stochastic baseline, day and night | – | `06_ts_plot.R` |
| 7 | Day and night FFT amplitude spectra, 42–54 cpd, normalised for each monitoring point | `07_fft_analysis.R` | `07_fft_plot.R` |
| 8 | Short-time Fourier transform spectrograms, 45–51 cpd, in dB (clipped to −60 to 0) | `08_spectrogram_analysis.R` | `08_spectrogram_plot.R` |
| 9 | Inverse-FFT reconstruction of the 45.5–50.5 cpd band | `09_reconstruction_analysis.R` | `09_reconstruction_plot.R` |
| 10 | Minimum–maximum range of the reconstruction in each day and night segment, open regimes (1, 3, 5) | `10_segment_analysis.R` | `10_segment_plot.R` |
| 11 | RMS amplitude of the reconstruction and percentage of residual variance it explains; values also in `figs/11_variance_values.csv` | `11_variance_analysis.R` | `11_variance_plot.R` |

Figures 1–5 of the article (location map, cave map, photographs and
workflow diagrams) are not produced by this code.


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
script to `logs/`. With default settings the whole run takes 4–5 hours
on a laptop, almost all of it in `08_spectrogram_analysis.R` (see
Parallel processing below); every other script takes seconds. 
To rerun only some scripts against existing intermediate
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

The exact versions used for the figures in the paper are listed in
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
ggplot2 falls back to its default font, so the figures differ from
those in the paper in typeface only.

## Data

The raw data are described in `measured/README.md`, and known
non-obvious details of the analysis in `NOTES.md`.

## Licence

The code, documentation and figures in this repository are released
under the MIT licence (see `LICENSE`).

The temperature records in `measured/` are not covered by that licence.
They come from the British Cave Science Centre, which makes them openly
available; please cite the BCSC as their source (see
`measured/README.md`).
