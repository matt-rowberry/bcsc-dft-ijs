# Temperature records

Six CSV files of air temperature, one per monitoring point in Poole's
Cavern, exactly as downloaded. Loggers record every 10 minutes; the
study period is 2019-10-16 00:00 to 2021-10-15 23:50 UTC (731 days).

## Source

The monitoring network is run by the British Cave Science Centre (BCSC)
at Poole's Cavern. Readings were downloaded from the British Cave
Research Association (BCRA) live data archive, which is linked from the
BCSC data page:

- BCSC data page: <https://www.cave-science.org.uk/data>
- BCRA archive: <https://bcra.org.uk/data/all_readings/>

<!-- TODO: download date -->

The BCSC describes its data as "completely open access". No formal
licence is stated; the files are redistributed here on that basis, and
users should cite the BCSC as the data source alongside this archive.

## Files and monitoring points

| File | Code | Monitoring point in manuscript | Readings | Location |
|---|---|---|---|---|
| `01_temp_ent_2019_2021.csv` | `ent` | MP1 | 100,800 | <!-- TODO --> |
| `02_temp_ten_2019_2021.csv` | `ten` | MP2 | 100,800 | <!-- TODO --> |
| `03_temp_sin_2019_2021.csv` | `sin` | MP3 | 105,261 | <!-- TODO --> |
| `04_temp_brg_2019_2021.csv` | `brg` | MP4 | 78,596 | <!-- TODO --> |
| `05_temp_mqs_2019_2021.csv` | `mqs` | MP5 | 95,127 | <!-- TODO --> |
| `06_temp_top_2019_2021.csv` | `top` | MP6 | 36,359 (from 2020-08-16) | <!-- TODO --> |

A complete record would have 105,264 readings. Logger positions are
shown on the survey available from the BCSC monitoring page
(<https://www.cave-science.org.uk/monitoring>).

## Format

- Two columns: `dttm` and `temp`.
- `dttm` is the logger's timestamp in **UTC**, in ISO 8601 form (e.g.
  `2019-10-16T00:01:06Z`). The loggers do not change their clocks for
  British Summer Time, so from late March to late October local clock
  time is `dttm` + 1 hour. Readings are a few seconds to a few minutes
  off the 10-minute grid, and drift slowly.
- `temp` is air temperature in °C. <!-- TODO: confirm; logger model and resolution -->
- A few readings are sensor faults, with impossible values between
  -34 and -104 °C (40 readings: 2020-12-09 `brg`, 2021-03-26 `mqs`,
  2021-04-26 `ent` and `ten`).

## From these files to the analysis

`00_preprocess.R` turns each file into a regular 10-minute series on a
common UTC time axis: each reading is assigned to the nearest 10-minute
timestamp (none is more than 5 minutes away), the first reading is kept
where two fall in the same slot (twice in the whole dataset), readings
below -20 °C are set to `NA` as sensor faults, and slots with no
reading are `NA`. Gaps are then filled in `01_imputing.R`.

## Access regimes

The five access regimes (see the top-level `README.md`) begin on:

| Regime | Start date | Days from start of record |
|---|---|---|
| 1 | 2019-10-16 | 0 |
| 2 | 2020-03-19 | 155 |
| 3 | 2020-07-20 | 278 |
| 4 | 2020-11-05 | 386 |
| 5 | 2021-05-18 | 580 |
| (end) | 2021-10-16 | 731 |
