#####

## 01. BANDPASS SIGNALS
bandpass_fft_cpd <- function(x, fs_cpd, freq_min_cpd, freq_max_cpd) {
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
    freqs_cpd_shifted <- ifelse(freqs_cpd <= fs_cpd/2, freqs_cpd, freqs_cpd - fs_cpd)

    # ---- Bandpass mask ----
    band_mask <- abs(freqs_cpd_shifted) >= freq_min_cpd & abs(freqs_cpd_shifted) <= freq_max_cpd
    fft_vals_filtered <- fft_vals
    fft_vals_filtered[!band_mask] <- 0

    # ---- Enforce Hermitian symmetry for real inverse FFT ----
    if (N_fft %% 2 == 0) {
        fft_vals_filtered[(N_fft/2 + 2):N_fft] <- Conj(rev(fft_vals_filtered[2:(N_fft/2)]))
    } else {
        fft_vals_filtered[((N_fft + 1)/2 + 1):N_fft] <- Conj(rev(fft_vals_filtered[2:((N_fft + 1)/2)]))
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

#####

## 02.

bandpass_fft_daynight <- function(df, timestamp_col, fs_cpd,
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
        r_full <- bandpass_fft_cpd(x, fs_cpd, freq_min, freq_max)

        # Day series
        x_day <- rep(NA_real_, length(x))
        x_day[is_day] <- x[is_day]
        r_day <- bandpass_fft_cpd(x_day, fs_cpd, freq_min, freq_max)

        # Night series
        x_night <- rep(NA_real_, length(x))
        x_night[is_night] <- x[is_night]
        r_night <- bandpass_fft_cpd(x_night, fs_cpd, freq_min, freq_max)

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

#####

## 03.

bandpass_fft_daynight_export <- function(results, df, timestamp_col, out_dir) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    for (col in names(results)) {
        r <- results[[col]]

        # Full series
        write_xlsx(
            data.frame(
                timestamp = df[[timestamp_col]],
                filtered = r$full$filtered_signal
            ),
            path = file.path(out_dir, paste0(col, "_full.xlsx"))
        )

        # Day series
        write_xlsx(
            data.frame(
                timestamp = df[[timestamp_col]][r$is_day],
                filtered = r$day$filtered_signal[r$is_day]
            ),
            path = file.path(out_dir, paste0(col, "_day.xlsx"))
        )

        # Night series
        write_xlsx(
            data.frame(
                timestamp = df[[timestamp_col]][r$is_night],
                filtered = r$night$filtered_signal[r$is_night]
            ),
            path = file.path(out_dir, paste0(col, "_night.xlsx"))
        )
    }

    message("Excel export complete. Files saved to: ", normalizePath(out_dir))
}

#####

## 04. NARROWBAND RECONSTRUCTION

reconstruct_day_night_process <- function(df, timestamp_col = "dttm",
                                          signal_cols = NULL,
                                          fs_cpd,
                                          freq_min = 47, freq_max = 49) {
    # Ensure timestamp is POSIXct
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Use all columns except timestamp if signal_cols not specified
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Compute fractional hour
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60
    is_night <- hour >= 17.5 | hour < 9.5
    is_day <- !is_night

    results <- list()

    for (col in signal_cols) {
        # ---- Day reconstruction ----
        df_day <- df[is_day, ]
        df_day$day_date <- as.Date(df_day[[timestamp_col]])
        day_list <- split(df_day, df_day$day_date)

        filtered_day <- rep(NA_real_, nrow(df))
        for (day_name in names(day_list)) {
            day_df <- day_list[[day_name]]
            filtered <- bandpass_fft_cpd(day_df[[col]], fs_cpd, freq_min, freq_max)$filtered_signal
            day_hours_mask <- is_day & as.Date(df[[timestamp_col]]) == as.Date(day_name)
            filtered_day[day_hours_mask] <- filtered
        }

        # ---- Night reconstruction ----
        df_night <- df[is_night, ]
        df_night$night_date <- as.Date(df_night[[timestamp_col]])
        night_list <- split(df_night, df_night$night_date)

        filtered_night <- rep(NA_real_, nrow(df))
        for (night_name in names(night_list)) {
            night_df <- night_list[[night_name]]
            filtered <- bandpass_fft_cpd(night_df[[col]], fs_cpd, freq_min, freq_max)$filtered_signal
            night_hours_mask <- is_night & as.Date(df[[timestamp_col]]) == as.Date(night_name)
            filtered_night[night_hours_mask] <- filtered
        }

        # ---- Combine day and night ----
        full_filtered <- rep(NA_real_, nrow(df))
        full_filtered[!is.na(filtered_day)] <- filtered_day[!is.na(filtered_day)]
        full_filtered[!is.na(filtered_night)] <- filtered_night[!is.na(filtered_night)]

        # ---- Save results ----
        results[[col]] <- list(
            filtered_day = filtered_day,
            filtered_night = filtered_night,
            full_filtered = full_filtered,
            is_day = is_day,
            is_night = is_night
        )
    }

    message("Day/night reconstruction complete for all columns.")
    return(results)
}

#####

##

reconstruct_day_night_process <- function(df, timestamp_col = "dttm",
                                          signal_cols = NULL,
                                          fs_cpd,
                                          freq_min = 47, freq_max = 49) {
    # Ensure timestamp is POSIXct
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Use all columns except timestamp if signal_cols not specified
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Compute fractional hour
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60
    is_night <- hour >= 17.5 | hour < 9.5
    is_day <- !is_night

    results <- list()

    for (col in signal_cols) {

        # ---- Day reconstruction (optional: split by date) ----
        df_day <- df[is_day, ]
        df_day$day_date <- as.Date(df_day[[timestamp_col]])
        day_list <- split(df_day, df_day$day_date)

        filtered_day <- rep(NA_real_, nrow(df))
        for (day_name in names(day_list)) {
            day_df <- day_list[[day_name]]
            filtered <- bandpass_fft_cpd(day_df[[col]], fs_cpd, freq_min, freq_max)$filtered_signal
            day_hours_mask <- is_day & as.Date(df[[timestamp_col]]) == as.Date(day_name)
            filtered_day[day_hours_mask] <- filtered
        }

        # ---- Night reconstruction (group consecutive TRUEs) ----
        night_rle <- rle(is_night)
        night_group <- rep(seq_along(night_rle$lengths), night_rle$lengths)

        filtered_night <- rep(NA_real_, nrow(df))
        night_list <- split(df[is_night, ], night_group[is_night])

        for (group_id in names(night_list)) {
            night_df <- night_list[[group_id]]
            filtered <- bandpass_fft_cpd(night_df[[col]], fs_cpd, freq_min, freq_max)$filtered_signal

            # Only assign to actual night points
            night_hours_mask <- is_night & night_group == as.integer(group_id)
            filtered_night[night_hours_mask] <- filtered
        }

        # ---- Combine day and night ----
        full_filtered <- rep(NA_real_, nrow(df))
        full_filtered[!is.na(filtered_day)] <- filtered_day[!is.na(filtered_day)]
        full_filtered[!is.na(filtered_night)] <- filtered_night[!is.na(filtered_night)]

        # ---- Save results ----
        results[[col]] <- list(
            filtered_day = filtered_day,
            filtered_night = filtered_night,
            full_filtered = full_filtered,
            is_day = is_day,
            is_night = is_night
        )
    }

    message("Day/night reconstruction complete for all columns (nights now continuous across midnight).")
    return(results)
}

#####

reconstruct_day_night_chrono <- function(df, timestamp_col = "dttm",
                                         signal_cols = NULL,
                                         fs_cpd,
                                         freq_min = 47, freq_max = 49,
                                         day_start = 9.5, day_end = 17.5) {
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60
    type_vec <- ifelse(hour >= day_start & hour < day_end, "day", "night")

    results <- list()

    for(col in signal_cols){
        x <- df[[col]]
        full_filtered <- rep(NA_real_, length(x))

        r <- rle(type_vec)
        starts <- cumsum(c(1, head(r$lengths, -1)))
        ends   <- cumsum(r$lengths)

        for(i in seq_along(r$lengths)){
            idx <- starts[i]:ends[i]
            seg_signal <- x[idx]
            seg_signal_clean <- na.omit(seg_signal)
            if(length(seg_signal_clean) > 0){
                full_filtered[idx] <- bandpass_fft_cpd(seg_signal, fs_cpd, freq_min, freq_max)$filtered_signal
            }
        }

        results[[col]] <- list(
            full_filtered = full_filtered,
            type_vec = type_vec
        )
    }

    return(results)
}

#####

process_day_night_multiband <- function(df, timestamp_col = "dttm",
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
    for(col in signal_cols){
        x <- df[[col]]

        for(bname in names(bands)){
            freq_range <- bands[[bname]]
            filtered_signal <- rep(NA_real_, length(x))
            seg_min <- numeric(length(r$lengths))
            seg_max <- numeric(length(r$lengths))

            # Process each day/night segment
            for(i in seq_along(r$lengths)){
                idx <- starts[i]:ends[i]
                seg_signal <- x[idx]
                seg_signal_clean <- na.omit(seg_signal)
                if(length(seg_signal_clean) > 0){
                    filtered_signal[idx] <- bandpass_fft_cpd(seg_signal, fs_cpd,
                                                             freq_range[1], freq_range[2])$filtered_signal
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
    return(list(full_series = as.data.frame(out_series),
                segment_summary = segment_summary))
}

process_day_night_nb3 <- function(df,
                                  timestamp_col = "dttm",
                                  signal_cols = NULL,
                                  fs_cpd = 1440,
                                  freq_min = 45.5,
                                  freq_max = 50.5,
                                  day_start = 9.5,
                                  day_end = 17.5) {

    # Ensure timestamp is POSIXct
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    # Signal columns
    if (is.null(signal_cols)) {
        signal_cols <- setdiff(names(df), timestamp_col)
    }

    # Day/night classification
    hour <- as.numeric(format(df[[timestamp_col]], "%H")) +
        as.numeric(format(df[[timestamp_col]], "%M")) / 60

    type_vec <- ifelse(
        hour >= day_start & hour < day_end,
        "day",
        "night"
    )

    # Identify consecutive day/night segments
    r <- rle(type_vec)

    starts <- cumsum(c(1, head(r$lengths, -1)))
    ends   <- cumsum(r$lengths)

    # Output starts with metadata only
    output <- data.frame(
        dttm = df[[timestamp_col]],
        period = NA_integer_,
        daynight = type_vec
    )

    # Assign period number
    output$period <- rep(seq_along(r$lengths), r$lengths)

    # Process each signal
    for (col in signal_cols) {

        filtered_signal <- rep(NA_real_, nrow(df))

        # Process each continuous day/night segment
        for (i in seq_along(r$lengths)) {

            idx <- starts[i]:ends[i]

            x <- df[[col]][idx]

            # Only process segments containing actual data
            if (all(is.na(x))) {
                next
            }

            filtered <- bandpass_fft_cpd(
                x = x,
                fs_cpd = fs_cpd,
                freq_min_cpd = freq_min,
                freq_max_cpd = freq_max
            )$filtered_signal

            filtered_signal[idx] <- filtered
        }

        # Store ONLY the 45.5–50.5 cpd reconstruction
        output[[paste0(col, "_nb3")]] <- filtered_signal
    }

    message(
        "45.5–50.5 cpd day/night reconstruction complete."
    )

    return(output)
}
