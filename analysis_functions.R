#####
##
## ANALYSIS FUNCTIONS
## Poole's Cavern temperature data
##
## Shared signal-processing helpers used by the data-analysis
## scripts for Figures 7, 9, 10 and 11.
##
## Renamed from bandpass_functions.R and pruned down to the three
## functions actually called anywhere in the pipeline. No
## computation has been altered - only names, comments and the
## removal of dead code.
##
## RENAME MAP (old name -> new name):
##   bandpass_fft_cpd            -> bandpass_filter_fft
##   bandpass_fft_daynight       -> bandpass_by_daynight
##   process_day_night_multiband -> reconstruct_multiband_segments
##
## REMOVED (confirmed unused anywhere in the reviewed pipeline):
##   bandpass_fft_daynight_export
##   reconstruct_day_night_process   (both definitions - the first
##                                     was already dead, shadowed by
##                                     the second, which itself was
##                                     never called by anything)
##   reconstruct_day_night_chrono
##   process_day_night_nb3
##
#####


## 01. CORE FFT BAND-PASS FILTER --------------------------------------------
##
## Band-pass filters a single numeric vector via FFT. Called by both
## of the functions below; not normally called directly.

bandpass_filter_fft <- function(x, fs_cpd, freq_min_cpd, freq_max_cpd) {
    x_raw <- x
    x_clean <- ifelse(is.na(x_raw), 0, x_raw)  # Replace NAs with 0
    N_orig <- length(x_clean)
    if (N_orig == 0) return(NULL)

    # ---- Zero-padding to next power of 2 ----
    N_fft <- 2^ceiling(log2(N_orig))
    x_pad <- c(x_clean, rep(0, N_fft - N_orig))

    # ---- FFT ----
    fft_vals <- fft(x_pad)

    # ---- Frequency vector (centered around 0) ----
    freqs_cpd <- seq(0, N_fft - 1) * fs_cpd / N_fft
    freqs_cpd_shifted <- ifelse(freqs_cpd <= fs_cpd / 2, freqs_cpd, freqs_cpd - fs_cpd)

    # ---- Bandpass mask ----
    band_mask <- abs(freqs_cpd_shifted) >= freq_min_cpd & abs(freqs_cpd_shifted) <= freq_max_cpd
    fft_vals_filtered <- fft_vals
    fft_vals_filtered[!band_mask] <- 0

    # ---- Enforce Hermitian symmetry for real inverse FFT ----
    if (N_fft %% 2 == 0) {
        fft_vals_filtered[(N_fft / 2 + 2):N_fft] <- Conj(rev(fft_vals_filtered[2:(N_fft / 2)]))
    } else {
        fft_vals_filtered[((N_fft + 1) / 2 + 1):N_fft] <- Conj(rev(fft_vals_filtered[2:((N_fft + 1) / 2)]))
    }

    # ---- Inverse FFT and truncate back to original length ----
    filtered_signal_full <- Re(fft(fft_vals_filtered, inverse = TRUE) / N_fft)
    filtered_signal <- filtered_signal_full[1:N_orig]

    # ---- One-sided FFT for amplitude/power ----
    half_N <- floor(N_fft / 2)
    freqs_pos <- seq(0, half_N) * (fs_cpd / N_fft)
    fft_filtered_vals <- fft_vals_filtered[1:(half_N + 1)]
    amplitude <- Mod(fft_filtered_vals) / N_fft
    power <- amplitude^2

    # Proper amplitude scaling (avoid doubling Nyquist)
    if (N_fft > 2) {
        if (N_fft %% 2 == 0) {
            amplitude[2:half_N] <- 2 * amplitude[2:half_N]
            power[2:half_N] <- 2 * power[2:half_N]
        } else {
            amplitude[2:(half_N + 1)] <- 2 * amplitude[2:(half_N + 1)]
            power[2:(half_N + 1)] <- 2 * power[2:(half_N + 1)]
        }
    }

    # ---- Return ----
    list(
        filtered_signal = filtered_signal,
        freqs_cpd = freqs_pos,
        amplitude = amplitude,
        power = power,
        N = N_orig
    )
}


## 02. BAND-PASS A DATAFRAME OF SIGNALS, SPLIT BY DAY/NIGHT -----------------
##
## Used by the Figure 7 data-analysis script.
## For each signal column, returns full/day/night FFT results
## (filtered signal, frequency vector, amplitude, power).

bandpass_by_daynight <- function(df, timestamp_col, fs_cpd,
                                 freq_min, freq_max,
                                 day_start = 9.5, day_end = 17.5,
                                 signal_cols = NULL) {
    # Ensure timestamp is POSIXct
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Use all non-timestamp columns if not specified
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Compute fractional hour
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60
    is_day <- hour >= day_start & hour < day_end
    is_night <- !is_day

    # Output container
    results <- list()

    for (col in signal_cols) {
        x <- df[[col]]

        # Full series
        r_full <- bandpass_filter_fft(x, fs_cpd, freq_min, freq_max)

        # Day series
        x_day <- rep(NA_real_, length(x))
        x_day[is_day] <- x[is_day]
        r_day <- bandpass_filter_fft(x_day, fs_cpd, freq_min, freq_max)

        # Night series
        x_night <- rep(NA_real_, length(x))
        x_night[is_night] <- x[is_night]
        r_night <- bandpass_filter_fft(x_night, fs_cpd, freq_min, freq_max)

        results[[col]] <- list(
            full  = r_full,
            day   = r_day,
            night = r_night,
            is_day = is_day,
            is_night = is_night
        )
    }

    message("Bandpass filtering complete. Full/day/night series processed.")
    return(results)
}


## 03. MULTIBAND RECONSTRUCTION BY CONTINUOUS DAY/NIGHT SEGMENT -------------
##
## Used by the canonical Figure 9 data-analysis script, whose two
## outputs (full_series, segment_summary) in turn feed Figures 9,
## 10 and 11. Reconstructs named frequency bands (only nb3 is
## currently active - see the `bands` default below) separately
## within each continuous day or night segment.

reconstruct_multiband_segments <- function(df, timestamp_col = "dttm",
                                           signal_cols = NULL,
                                           fs_cpd,
                                           bands = list(
                                               #fullband = c(0, 72),
                                               #nb1 = c(24, 72),
                                               #nb2 = c(45, 51),
                                               nb3 = c(45.5, 50.5)
                                           ),
                                           day_start = 9.5, day_end = 17.5) {
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Ensure POSIXct
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Compute day/night vector
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60
    type_vec <- ifelse(hour >= day_start & hour < day_end, "day", "night")

    # Initialize output lists
    out_series <- list(timestamp = df[[timestamp_col]], daynight = type_vec)
    out_segments <- list()

    # Consecutive day/night segments
    r <- rle(type_vec)
    starts <- cumsum(c(1, head(r$lengths, -1)))
    ends   <- cumsum(r$lengths)
    seg_type <- r$values

    # Loop over numeric columns
    for (col in signal_cols) {
        x <- df[[col]]

        for (bname in names(bands)) {
            freq_range <- bands[[bname]]
            filtered_signal <- rep(NA_real_, length(x))
            seg_min <- numeric(length(r$lengths))
            seg_max <- numeric(length(r$lengths))

            # Process each day/night segment
            for (i in seq_along(r$lengths)) {
                idx <- starts[i]:ends[i]
                seg_signal <- x[idx]
                seg_signal_clean <- na.omit(seg_signal)
                if (length(seg_signal_clean) > 0) {
                    filtered_signal[idx] <- bandpass_filter_fft(
                        seg_signal, fs_cpd,
                        freq_range[1], freq_range[2]
                    )$filtered_signal
                }
                # Compute min/max for the segment
                seg_min[i] <- min(filtered_signal[idx], na.rm = TRUE)
                seg_max[i] <- max(filtered_signal[idx], na.rm = TRUE)
            }

            # Save full series
            col_name <- paste0(col, "_", bname)
            out_series[[col_name]] <- filtered_signal

            # Save min/max per segment
            seg_df <- data.frame(
                column = col,
                band = bname,
                segment = seq_along(r$lengths),
                type = seg_type,
                ymin = seg_min,
                ymax = seg_max
            )
            out_segments[[paste0(col, "_", bname)]] <- seg_df
        }
    }

    # Combine all segment summaries
    segment_summary <- do.call(rbind, out_segments)

    # Return as a list: full series + segment min/max
    return(list(
        full_series = as.data.frame(out_series),
        segment_summary = segment_summary
    ))
}
