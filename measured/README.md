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

The files were downloaded on or before 15 July 2023 (the modification
date of the original downloads; the exact download date was not
recorded).

The monitoring network uses the Tinytag Connect system (Gemini Data
Loggers): battery-powered radio data loggers with thermistor probes in
flexible, waterproof encapsulated housings on cables of up to 10 m,
linked by a radio mesh network to a receiver at the cave entrance. The
thermistor sensors cover -40 to +125 °C, and the loggers record every
10 minutes. Readings are exported from the system as CSV files over
HTTP. See Blewett (2019) for a description of the system at Poole's
Cavern.

The BCSC describes its data as "completely open access". No formal
licence is stated; the files are redistributed here on that basis, and
users should cite the BCSC as the data source alongside this archive.

## Files and monitoring points

| File | Code | Monitoring point | Readings | Logger | Site in Gunn et al. (2021) | Location |
|---|---|---|---|---|---|---|
| `01_temp_ent_2019_2021.csv` | `ent` | MP1 | 100,800 | 840300 | Entrance | About 1 m inside the entrance gate, head height |
| `02_temp_ten_2019_2021.csv` | `ten` | MP2 | 100,800 | 840300 | 10m | About 10 m inside the gate, floor level |
| `03_temp_sin_2019_2021.csv` | `sin` | MP3 | 105,261 | 840301 | 75m | Main Chamber, about 75 m along the show-cave path; floor level, west side of the passage about 10 m from the stream sink (The Sink, tour stop 3) |
| `04_temp_brg_2019_2021.csv` | `brg` | MP4 | 78,596 | 840308 | 125m | Main Chamber, about 125 m along the path, close to The Bridge (tour stop 4); about 1 m above head height |
| `05_temp_mqs_2019_2021.csv` | `mqs` | MP5 | 95,127 | 844555 | 150m | About 150 m along the path, close to the Mary Queen of Scots Pillar (tour stop 6) where the passage narrows markedly; floor level |
| `06_temp_top_2019_2021.csv` | `top` | MP6 | 36,359 (from 2020-08-16) | – | – | Near the top of the boulder choke; the only monitoring point well away from the tourist path |

A complete record would have 105,264 readings. MP1 and MP2 are two
inputs of the same logger, so their records share identical
timestamps. Logger numbers, distances and descriptions for MP1–MP5 are
from Gunn et al. (2021, Table 1), which uses distance along the show-cave
path to name the sites; MP6 was not part of that study. The positions of
the monitoring points are shown in the map figure of the manuscript and
on the survey available from the BCSC monitoring page
(<https://www.cave-science.org.uk/monitoring>).

## Format

- Two columns: `dttm` and `temp`.
- `dttm` is the logger's timestamp in **UTC**, in ISO 8601 form (e.g.
  `2019-10-16T00:01:06Z`). The loggers do not change their clocks for
  British Summer Time, so from late March to late October local clock
  time is `dttm` + 1 hour. Readings are a few seconds to a few minutes
  off the 10-minute grid, and drift slowly.
- `temp` is air temperature in °C.
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

## References

Blewett, P. (2019). The Tinytag Connect System and its application in
the British Cave Monitoring Centre. *CREG Journal* 108, 17–20.

Gunn, J., Rowberry, M. and Smith, A. (2021). Exploring the impact of
tourists on cave air temperatures: a Covid-19 case study from Poole's
Cavern, Derbyshire, UK. *Cave and Karst Science* 48, 121–128.
