#####

## ANALYSIS FUNCTIONS
## POOLE'S CAVERN TEMPERATURE DATA
## LAST MODIFIED 4 OCTOBER 2026

#####

# Shared helpers used by the data analysis scripts for Figures 6, 7, 9,
# 10 and 11 (04_day_night_split.R, 05_multiband_reconstruction.R and
# 07_fft_analysis.R). No external packages are required; only base R
# is used.
#
# Contents:
#   is_daytime()                      day/night classification (UK local time)
#   steady_state_gain()               Kalman gain of the local-level model
#   innovation_response()             frequency response of the residuals
#   bandpass_filter_fft()             FFT band-pass filter for one series
#   bandpass_by_daynight()            band-pass, split by day and night (Figure 7)
#   reconstruct_multiband_segments()  band-pass within each day/night segment
#                                     (Figures 9, 10 and 11)

# ---------------------------------------------------------------------------
# Day and night
# ---------------------------------------------------------------------------

# Day is the period when the cave is open to visitors, taken as 09:30
# to 17:20 UK local time (the timestamp of the last 10-minute sample
# of the day); night is 17:30 to 09:20. Every sample is either day or
# night.
#
# The loggers record in UTC and do not change their clocks for
# British Summer Time, whereas opening hours follow local time.
# Timestamps are therefore converted to Europe/London time before
# being classified: during BST, day is 08:30 to 16:20 UTC.

day_first_local <- "09:30"
day_last_local <- "17:20"

#' Classify timestamps as day (cave open) or night.
#'
#' @param dttm POSIXct timestamps. Their time-zone attribute does not
#'   matter: POSIXct values are absolute instants, and they are
#'   converted to UK local time here.
#' @param tz Time zone of the opening hours.
#' @param first,last First and last daytime sample, "HH:MM" local time.
#' @return Logical vector, `TRUE` for day.
is_daytime <- function(
        dttm,
        tz = "Europe/London",
        first = day_first_local,
        last = day_last_local
) {
    stopifnot(inherits(dttm, "POSIXct"))
    local_time <- format(dttm, "%H:%M", tz = tz)
    local_time >= first & local_time <= last
}

# ---------------------------------------------------------------------------
# Frequency response of the residuals
# ---------------------------------------------------------------------------

# The residuals saved by 02_kalman_filtering.R are one-step-ahead
# prediction errors of the local-level model. Once the Kalman filter
# has settled, these are a fixed linear filter of the temperature,
#
#   v_t = (1 - B) / (1 - (1 - K) B) y_t,
#
# where B is the backshift operator and K the steady-state Kalman gain.
# The filter removes slow variability but amplifies high frequencies:
# at 48 cpd the residual is 1.2-1.7 times the temperature oscillation,
# depending on the monitoring point (see NOTES.md).

#' Steady-state Kalman gain of a local-level model.
#'
#' @param Q Variance of the level disturbance.
#' @param H Variance of the observation disturbance.
#' @return The gain K, between 0 and 1.
steady_state_gain <- function(Q, H) {
    P <- (Q + sqrt(Q^2 + 4 * Q * H)) / 2 # steady-state prediction variance
    P / (P + H)
}

#' Frequency response of the prediction-error filter.
#'
#' @param freq_cpd Frequencies, in cycles per day (may be negative).
#' @param K Steady-state Kalman gain.
#' @param fs_cpd Sampling frequency, in cycles per day.
#' @return Complex response; `Mod()` gives the gain.
innovation_response <- function(freq_cpd, K, fs_cpd) {
    z <- exp(-2i * pi * freq_cpd / fs_cpd)
    (1 - z) / (1 - (1 - K) * z)
}

# ---------------------------------------------------------------------------
# Core FFT band-pass filter
# ---------------------------------------------------------------------------

#' Band-pass filter a single series via FFT.
#'
#' Zero-pads `x` to the next power of two, masks the FFT to
#' `[freq_min_cpd, freq_max_cpd]` (and its mirror image, to keep the
#' spectrum Hermitian for a real inverse FFT), and inverts. Missing
#' values in `x` are treated as zero.
#'
#' Called internally by `bandpass_by_daynight()` and
#' `reconstruct_multiband_segments()`; not normally called directly.
#'
#' @param x Numeric vector to filter.
#' @param fs_cpd Sampling frequency, in cycles per day.
#' @param freq_min_cpd,freq_max_cpd Pass-band, in cycles per day.
#' @param K Optional steady-state Kalman gain. If given, the retained
#'   coefficients are divided by `innovation_response()`, which undoes
#'   the prediction-error filter and returns the band in the units and
#'   phase of the temperature itself ("recolouring"). `NULL` leaves the
#'   series as it is.
#' @return A list with `filtered_signal` (same length as `x`),
#'   `freqs_cpd`, `amplitude`, `power` (one-sided spectrum) and `N`
#'   (the length of `x`). `NULL` if `x` has length zero.
bandpass_filter_fft <- function(x, fs_cpd, freq_min_cpd, freq_max_cpd, K = NULL) {
    x_clean <- ifelse(is.na(x), 0, x)
    N_orig <- length(x_clean)
    if (N_orig == 0) return(NULL)

    # Zero-padding to next power of 2.
    N_fft <- 2^ceiling(log2(N_orig))
    x_pad <- c(x_clean, rep(0, N_fft - N_orig))

    # FFT and frequency vector (centred around 0).
    fft_vals <- fft(x_pad)
    freqs_cpd <- seq(0, N_fft - 1) * fs_cpd / N_fft
    freqs_cpd_shifted <- ifelse(freqs_cpd <= fs_cpd / 2, freqs_cpd, freqs_cpd - fs_cpd)

    # Bandpass mask.
    band_mask <- abs(freqs_cpd_shifted) >= freq_min_cpd & abs(freqs_cpd_shifted) <= freq_max_cpd
    fft_vals_filtered <- fft_vals
    fft_vals_filtered[!band_mask] <- 0

    # Optional recolouring: undo the prediction-error filter within the
    # pass-band. The band never contains 0 cpd, where the response is 0.
    if (!is.null(K)) {
        fft_vals_filtered[band_mask] <- fft_vals_filtered[band_mask] /
            innovation_response(freqs_cpd_shifted[band_mask], K, fs_cpd)
    }

    # Enforce Hermitian symmetry for a real inverse FFT.
    if (N_fft %% 2 == 0) {
        fft_vals_filtered[(N_fft / 2 + 2):N_fft] <- Conj(rev(fft_vals_filtered[2:(N_fft / 2)]))
    } else {
        fft_vals_filtered[((N_fft + 1) / 2 + 1):N_fft] <- Conj(rev(fft_vals_filtered[2:((N_fft + 1) / 2)]))
    }

    # Inverse FFT, truncated back to the original length.
    filtered_signal_full <- Re(fft(fft_vals_filtered, inverse = TRUE) / N_fft)
    filtered_signal <- filtered_signal_full[1:N_orig]

    # One-sided FFT for amplitude/power.
    half_N <- floor(N_fft / 2)
    freqs_pos <- seq(0, half_N) * (fs_cpd / N_fft)
    fft_filtered_vals <- fft_vals_filtered[1:(half_N + 1)]
    amplitude <- Mod(fft_filtered_vals) / N_fft
    power <- amplitude^2

    # Proper amplitude scaling (avoid doubling Nyquist).
    if (N_fft > 2) {
        if (N_fft %% 2 == 0) {
            amplitude[2:half_N] <- 2 * amplitude[2:half_N]
            power[2:half_N] <- 2 * power[2:half_N]
        } else {
            amplitude[2:(half_N + 1)] <- 2 * amplitude[2:(half_N + 1)]
            power[2:(half_N + 1)] <- 2 * power[2:(half_N + 1)]
        }
    }

    list(
        filtered_signal = filtered_signal,
        freqs_cpd = freqs_pos,
        amplitude = amplitude,
        power = power,
        N = N_orig
    )
}

# ---------------------------------------------------------------------------
# Band-pass a dataframe of signals, split by day/night
# ---------------------------------------------------------------------------

#' Band-pass filter each column of a data frame, split by day/night.
#'
#' For each signal column, filters the full series and the day-only
#' and night-only series (the excluded half set to `NA` before
#' filtering) with `bandpass_filter_fft()`. Used by
#' 07_fft_analysis.R (Figure 7).
#'
#' @param df Data frame with a timestamp column and one or more
#'   signal columns.
#' @param timestamp_col Name of the timestamp column.
#' @param fs_cpd,freq_min,freq_max Passed to `bandpass_filter_fft()`.
#' @param signal_cols Columns to filter; defaults to every column
#'   except `timestamp_col`.
#' @return A named list (one entry per signal column) of lists with
#'   `full`, `day`, `night` (each a `bandpass_filter_fft()` result)
#'   and the logical masks `is_day`, `is_night`.
bandpass_by_daynight <- function(
        df,
        timestamp_col,
        fs_cpd,
        freq_min,
        freq_max,
        signal_cols = NULL
) {
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Day and night as defined by is_daytime() (UK local time).
    is_day <- is_daytime(df[[timestamp_col]])
    is_night <- !is_day

    results <- list()

    for (col in signal_cols) {
        x <- df[[col]]

        x_day <- rep(NA_real_, length(x))
        x_day[is_day] <- x[is_day]

        x_night <- rep(NA_real_, length(x))
        x_night[is_night] <- x[is_night]

        results[[col]] <- list(
            full = bandpass_filter_fft(x, fs_cpd, freq_min, freq_max),
            day = bandpass_filter_fft(x_day, fs_cpd, freq_min, freq_max),
            night = bandpass_filter_fft(x_night, fs_cpd, freq_min, freq_max),
            is_day = is_day,
            is_night = is_night
        )
    }

    message("Bandpass filtering complete. Full/day/night series processed.")
    results
}

# ---------------------------------------------------------------------------
# Multiband reconstruction by continuous day/night segment
# ---------------------------------------------------------------------------

#' Reconstruct named frequency bands within each day/night segment.
#'
#' For each signal column and each named band, filters every
#' continuous day or night segment of the record separately with
#' `bandpass_filter_fft()`, and records the per-segment minimum and
#' maximum of the reconstructed signal. Used once, by
#' 05_multiband_reconstruction.R, whose two outputs feed Figures 9,
#' 10 and 11.
#'
#' Only the `nb3` band (45.5-50.5 cpd) is used; the others are kept,
#' commented out, as a record of the wider bands examined before the
#' analysis focused on 45.5-50.5 cpd.
#'
#' @param df Data frame with a timestamp column and one or more
#'   signal columns.
#' @param timestamp_col Name of the timestamp column.
#' @param signal_cols Columns to filter; defaults to every column
#'   except `timestamp_col`.
#' @param fs_cpd Sampling frequency, in cycles per day.
#' @param bands Named list of `c(freq_min, freq_max)` pairs.
#' @param K Optional named vector of steady-state Kalman gains, one per
#'   signal column, passed to `bandpass_filter_fft()` to recolour the
#'   reconstruction. `NULL` leaves it as the residuals are.
#' @return A list with:
#'   * `full_series` - a data frame with `timestamp`, `daynight` and
#'     one reconstructed `<column>_<band>` series per column/band
#'     combination.
#'   * `segment_summary` - a data frame with one row per
#'     column/band/segment, giving the segment's `type` (day/night)
#'     and the reconstructed signal's `ymin`/`ymax` within it.
reconstruct_multiband_segments <- function(
        df,
        timestamp_col = "dttm",
        signal_cols = NULL,
        fs_cpd,
        bands = list(
            # fullband = c(0, 72),
            # nb1 = c(24, 72),
            # nb2 = c(45, 51),
            nb3 = c(45.5, 50.5)
        ),
        K = NULL
) {
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Day and night as defined by is_daytime() (UK local time).
    type_vec <- ifelse(is_daytime(df[[timestamp_col]]), "day", "night")

    out_series <- list(timestamp = df[[timestamp_col]], daynight = type_vec)
    out_segments <- list()

    # Consecutive day/night segments.
    r <- rle(type_vec)
    starts <- cumsum(c(1, head(r$lengths, -1)))
    ends <- cumsum(r$lengths)
    seg_type <- r$values

    for (col in signal_cols) {
        x <- df[[col]]

        for (bname in names(bands)) {
            freq_range <- bands[[bname]]
            filtered_signal <- rep(NA_real_, length(x))
            seg_min <- numeric(length(r$lengths))
            seg_max <- numeric(length(r$lengths))

            # Process each day/night segment.
            for (i in seq_along(r$lengths)) {
                idx <- starts[i]:ends[i]
                seg_signal <- x[idx]

                if (length(na.omit(seg_signal)) > 0) {
                    filtered_signal[idx] <- bandpass_filter_fft(
                        seg_signal, fs_cpd,
                        freq_range[1], freq_range[2],
                        K = if (is.null(K)) NULL else K[[col]]
                    )$filtered_signal
                }

                seg_min[i] <- min(filtered_signal[idx], na.rm = TRUE)
                seg_max[i] <- max(filtered_signal[idx], na.rm = TRUE)
            }

            col_name <- paste0(col, "_", bname)
            out_series[[col_name]] <- filtered_signal

            out_segments[[col_name]] <- data.frame(
                column = col,
                band = bname,
                segment = seq_along(r$lengths),
                type = seg_type,
                ymin = seg_min,
                ymax = seg_max
            )
        }
    }

    list(
        full_series = as.data.frame(out_series),
        segment_summary = do.call(rbind, out_segments)
    )
}
