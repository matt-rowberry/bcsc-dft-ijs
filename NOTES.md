# Notes on the analysis

Details that are easy to misread as bugs. Each matches the published
figures; changing any of them would change the results.

## 1. Two day/night definitions

Day is 09:30–17:30 throughout, but the end point is handled in two ways:

| Used by | Script | Day | 17:30 sample is |
|---|---|---|---|
| Figure 6 | `04_day_night_split.R` | 09:30 ≤ t ≤ 17:30 | day |
| Figures 7, 9, 10, 11 | `analysis_functions.R` (`bandpass_by_daynight()`, `reconstruct_multiband_segments()`) | 09:30 ≤ t < 17:30 | night |

So the two definitions differ by one 10-minute sample per day. Neither
drops any samples: every timestamp is either day or night. (The comment
in `04_day_night_split.R` about the 09:20–09:30 and 17:30–17:40
intervals being "excluded" refers to the gaps between samples, not to
any samples being dropped.) Figure 7 uses only the full-period series
from script 04 and applies its own day/night split.

## 2. Zero marks a day with a long gap

`03_long_gaps.R` does not interpolate gaps longer than 6 hours. It sets
every residual on each affected calendar day to exactly `0`. It also
sets the first residual of each series to `0`, because that value
reflects the model's initialisation.

These zeros pass through the band-pass filter. A day or night segment
that is entirely zero comes out as exactly zero, since the FFT of a
zero vector is zero. `11_variance_analysis.R` relies on this: it treats
exact zeros in the reconstructed signal as missing (`na_if(ifft, 0)`),
which removes those days from the RMS and variance summaries.

One consequence: a day segment (09:30–17:20) always lies within one
calendar day, so it is either entirely zeroed or not at all. A night
segment runs across midnight, so a zeroed calendar day can cover half
of it. The band-passed values in that zeroed half are small but not
exactly zero, so they stay in the Figure 11 summaries.

## 3. Access-regime boundaries are written out in three places

Each is derived from the same regime start dates (see
`measured/README.md`), counted as `d` days from 2019-10-16:

| Script | Form | Values |
|---|---|---|
| `01_imputing.R` | dates in `case_when()` | 2020-03-19, 2020-07-20, 2020-11-05, 2021-05-18 |
| `08_spectrogram_analysis.R` | `cut()` breaks on the spectrogram time axis, `d / 1000` | 0, 0.155, 0.278, 0.386, 0.580, 0.731 |
| `10_segment_analysis.R` | day/night segment numbers, `2d + 1` | 311, 557, 773, 1161 (end 1463) |

The spectrogram treats each residual series as a sound sampled at
144,000 Hz (144 samples per day × 1000). One day of data therefore
lasts 0.001 s on its time axis, and frequencies are read directly in
cycles per day.

Segments alternate night/day starting with the night of 2019-10-16.
Segment `2d + 1` is the night spanning the midnight at which the regime
changed, so script 10 assigns it to both adjacent regimes. Figure 11
instead assigns each sample to a regime by its own timestamp (via the
`period` column from script 01), so it splits that night between the
two regimes.

## 4. Smaller points

- **Figure 7 normalisation.** Each monitoring point's day and night
  spectra are divided by that point's single largest amplitude across
  all five regimes, day and night. Amplitudes are therefore comparable
  across regimes and between day and night for the same point, but not
  between points.
- **Missing values in the FFT.** `bandpass_filter_fft()` treats `NA` as
  zero and zero-pads to the next power of two. In Figure 7 the day
  (or night) series has the other half of each day set to `NA`, so
  each spectrum is that of a gated series.
- **Unused bands.** `reconstruct_multiband_segments()` keeps other
  candidate bands commented out as a record of the original analysis;
  only `nb3` (45.5–50.5 cpd) is used.
- **Console messages.** `07_fft_analysis.R` prints a
  `New names: ...1 ...` message from building a data frame from
  unnamed columns; the columns are renamed on the next line and the
  message can be ignored.
