# Notes on the analysis

Details that are easy to misread as bugs. Each matches the published
figures; changing any of them would change the results.

## 1. Day and night are defined in UK local time

All timestamps are UTC: the loggers do not change their clocks for
British Summer Time. Opening hours follow local time, so `is_daytime()`
in `analysis_functions.R` converts each timestamp to Europe/London time
before classifying it:

| | Local time | UTC during GMT (late Oct–late Mar) | UTC during BST |
|---|---|---|---|
| Day | 09:30–17:20 | 09:30–17:20 | 08:30–16:20 |
| Night | 17:30–09:20 | 17:30–09:20 | 16:30–08:20 |

Times are those of the 10-minute samples, so every sample is either day
or night. The same function is used for Figure 6 (`04_day_night_split.R`)
and for Figures 7, 9, 10 and 11 (`bandpass_by_daynight()` and
`reconstruct_multiband_segments()`).

The published figures (before October 2026) applied a fixed
09:30–17:30 window in UTC, so during BST the first hour of opening was
counted as night. They also treated the 17:30 sample as day in
Figure 6 but as night elsewhere.

The 09:30–17:20 window is a best estimate: opening hours changed during
the COVID-19 restrictions, and the data suggest shorter opening hours
in winter (visitor-related fluctuations from about 10:00 to 16:00).
The window is set in one place (`day_first_local`, `day_last_local`).

Access-regime boundaries (`01_imputing.R`) fall at midnight UTC, i.e.
01:00 local time during BST, which is within the night and does not
affect any day.

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

One consequence: a day segment always lies within one calendar day
(UTC), so it is either entirely zeroed or not at all. A night
segment runs across midnight, so a zeroed calendar day can cover half
of it. The band-passed values in that zeroed half are small but not
exactly zero, so they stay in the Figure 11 summaries.

## 3. Access-regime boundaries are written out in three places

Each is derived from the same regime start dates (see
`measured/README.md`), counted as `d` days from 2019-10-16 (UTC):

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
