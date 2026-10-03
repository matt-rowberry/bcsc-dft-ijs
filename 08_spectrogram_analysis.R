#####

## CALCULATING SPECTROGRAMS FOR POOLE'S CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(dplyr)
library(phonTools)
library(seewave)
library(future)
library(future.apply)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/kalman_residuals_full_imp.rds"
output_file <- "data/figure_08_spectrograms.rds"

# Number of spectrograms computed at once. Each one needs several GB of
# memory, so running all six in parallel can exhaust RAM on a laptop
# (the run then fails with "cannot allocate vector"). The default of 1
# computes them one after another and takes 5-6 hours on a laptop;
# with 2 or 3 the run time falls roughly in proportion, at 2 or 3
# times the peak memory. The results do not depend on this setting.
spectrogram_workers <- 1

# Each spectrogram is saved here as soon as it is finished, so a run
# that is interrupted (lack of memory, the computer sleeping) resumes
# from the next one instead of starting again. A saved spectrogram is
# reused only if its input series, the code of
# spectrogram_for_sensor() and the seewave and phonTools versions all
# match exactly; otherwise it is recomputed. The folder is deleted
# once all six are finished and the output file has been written.
cache_dir <- "data/spectrogram_cache"

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

df <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Create spectrogram input dataframe
# ---------------------------------------------------------------------------

# The six monitoring-point residual series are used as the input
# signals for the spectrogram analysis.

df_spec <- data.frame(
    ent = df$ent_residual,
    ten = df$ten_residual,
    sin = df$sin_residual,
    brg = df$brg_residual,
    mqs = df$mqs_residual,
    top = df$top_residual
)

# Missing values are replaced with zero before calculating the
# spectrograms, as in the original analysis.

df_spec[is.na(df_spec)] <- 0

# ---------------------------------------------------------------------------
# Convert spectrogram output to a dataframe
# ---------------------------------------------------------------------------

#' Reshape a `seewave::spectro()` result into long form.
#'
#' @param spec Output of `spectro()`.
#' @return A data frame with `time`, `frequency` and `amplitude`.
spectro_to_df <- function(spec) {
    data.frame(
        time = rep(spec$time, each = length(spec$freq)),
        frequency = rep(spec$freq, times = length(spec$time)),
        amplitude = as.vector(spec$amp)
    )
}

# ---------------------------------------------------------------------------
# Calculate spectrograms
# ---------------------------------------------------------------------------

# The residual series are treated as sound objects so that the
# spectrogram functions can be used. A sampling frequency of 144000 is
# used, i.e. 144 observations per day (144 cpd) multiplied by 1000.
# Frequencies are therefore reported in cycles per day (cpd), and one
# day of data lasts 0.001 s on the time axis of the sound object.
#
# Each sensor's spectrogram (zp = length * 4 on a ~105,000-row series)
# is expensive in time and memory and independent of the others. The
# six sensors can therefore be processed in parallel, subject to
# memory (see spectrogram_workers above).

#' Spectrogram for one sensor, as a long data frame.
#'
#' @param sensor Column name in `df_spec`.
#' @param df_spec Data frame of per-sensor residual series.
#' @return A data frame with `time`, `frequency`, `amplitude` and
#'   `sensor`, with amplitude clipped to `[-60, 0]` dB.
spectrogram_for_sensor <- function(sensor, df_spec) {
    sound_obj <- makesound(df_spec[[sensor]], fs = 144000)

    spec <- spectro(
        sound_obj,
        wl = 54,
        wn = "bartlett",
        zp = length(df_spec[[sensor]]) * 4,
        ovlp = 0,
        dB = "max0",
        flimd = c(45, 51),
        plot = FALSE
    )

    spec_df <- spectro_to_df(spec)

    # Limit amplitudes to the range used in the original spectrogram analysis.
    spec_df$amplitude <- pmin(pmax(spec_df$amplitude, -60), 0)
    spec_df$sensor <- sensor
    spec_df
}

#' Spectrogram for one sensor, reusing a saved copy if still valid.
#'
#' Wraps `spectrogram_for_sensor()`. The result is saved to
#' `<cache_dir>/<sensor>.rds` together with everything it depends on
#' (the input series, the function's code and package versions), and
#' a saved copy is used only if all of these are identical.
#'
#' @param sensor Column name in `df_spec`.
#' @param df_spec Data frame of per-sensor residual series.
#' @param cache_dir Folder for saved spectrograms.
#' @return As for `spectrogram_for_sensor()`.
cached_spectrogram <- function(sensor, df_spec, cache_dir) {
    cache_file <- file.path(cache_dir, paste0(sensor, ".rds"))
    key <- list(
        input = df_spec[[sensor]],
        code = deparse(spectrogram_for_sensor),
        packages = c(
            seewave = as.character(packageVersion("seewave")),
            phonTools = as.character(packageVersion("phonTools"))
        )
    )

    if (file.exists(cache_file)) {
        cached <- readRDS(cache_file)
        if (identical(cached$key, key)) {
            message(format(Sys.time(), "%H:%M:%S"), "  ", sensor, ": reused saved spectrogram")
            return(cached$spec_df)
        }
        message(sensor, ": saved spectrogram is out of date; recomputing")
    }

    start <- Sys.time()
    spec_df <- spectrogram_for_sensor(sensor, df_spec)

    # Write to a temporary file first, so an interruption during saving
    # cannot leave a truncated file behind.
    tmp_file <- paste0(cache_file, ".tmp")
    saveRDS(list(key = key, spec_df = spec_df), tmp_file)
    file.rename(tmp_file, cache_file)

    message(sprintf("%s  %s: computed in %.0f min",
                    format(Sys.time(), "%H:%M:%S"), sensor,
                    as.numeric(difftime(Sys.time(), start, units = "mins"))))
    spec_df
}

#' Compute every sensor's spectrogram, optionally in parallel.
#'
#' With `workers = 1` the sensors are processed one after another in
#' this R session. With more, they are distributed across worker
#' processes with `future_lapply()`.
#'
#' @param df_spec Data frame of per-sensor residual series.
#' @param cache_dir Folder for saved spectrograms (see
#'   `cached_spectrogram()`).
#' @param workers Number of spectrograms computed at once.
#' @return A single long data frame (all sensors row-bound).
spectrograms_parallel <- function(df_spec, cache_dir, workers = 1) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    if (workers <= 1) {
        spectrogram_list <- lapply(names(df_spec), cached_spectrogram,
                                   df_spec = df_spec, cache_dir = cache_dir)
    } else {
        # Restore the previous future plan when this function returns.
        oplan <- plan(multisession, workers = min(workers, ncol(df_spec)))
        on.exit(plan(oplan), add = TRUE)

        spectrogram_list <- future_lapply(
            names(df_spec),
            cached_spectrogram,
            df_spec = df_spec,
            cache_dir = cache_dir
        )
    }

    bind_rows(spectrogram_list)
}

all_sensors_df <- spectrograms_parallel(df_spec, cache_dir = cache_dir, workers = spectrogram_workers)

# ---------------------------------------------------------------------------
# Assign access regimes to spectrogram time
# ---------------------------------------------------------------------------

# The spectrogram time axis is measured in seconds of the sound object
# (see section "Calculate spectrograms" above), so it equals the
# number of days since the start of the record (2019-10-16) divided by
# 1000. The breakpoints below are the access-regime boundaries
# expressed on this axis:
#
#   0.000  start of record            (2019-10-16)
#   0.155  regime 1 -> 2, 155 days in (2020-03-19)
#   0.278  regime 2 -> 3, 278 days in (2020-07-20)
#   0.386  regime 3 -> 4, 386 days in (2020-11-05)
#   0.580  regime 4 -> 5, 580 days in (2021-05-18)
#   0.731  end of record, 731 days    (2021-10-15)

all_sensors_df$period <- cut(
    all_sensors_df$time,
    breaks = c(0, 0.155, 0.278, 0.386, 0.580, 0.731),
    labels = 1:5,
    include.lowest = TRUE,
    right = TRUE
) %>%
    as.integer()

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------

saveRDS(all_sensors_df, output_file)

# The saved per-sensor spectrograms are only needed to resume an
# interrupted run.
unlink(cache_dir, recursive = TRUE)
