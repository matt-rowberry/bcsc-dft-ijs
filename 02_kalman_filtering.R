#####

## KALMAN FILTERING THE TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 28 SEPTEMBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(KFAS)
library(future)
library(future.apply)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/all_sites_sr_imp.rds"
output_file <- "data/kalman_results_residuals.rds"

# ---------------------------------------------------------------------------
# Parallelised state-space model function
# ---------------------------------------------------------------------------

#' Fit a structural time-series model to each monitoring-point series.
#'
#' Each series in `data_df` is modelled independently with the Kalman
#' filter and smoother, in parallel across worker processes.
#'
#' @param data_df Data frame of monitoring-point series (one column
#'   per site).
#' @param model_fn Function taking one series and returning an
#'   SSModel.
#' @param dttm,period Timestamp and access-regime vectors to attach
#'   to the results.
#' @param inits Starting values passed to `fitSSM()`.
#' @param workers Number of parallel workers.
#' @return A list with two data frames:
#'   * `all` - for each monitoring point: original observations,
#'     fitted values, recursive residuals, smoothed signal, estimated
#'     observation disturbances and estimated state disturbances.
#'   * `residuals` - dttm, period and the recursive residual for each
#'     monitoring point only. This is the only part used downstream
#'     (see section "Write residuals" below).
kalman_filter_wide_parallel <- function(
    data_df,
    model_fn,
    dttm,
    period,
    inits = log(rep(1e-4, 4)),
    workers = max(1, availableCores() - 1)
) {
    if (!is.data.frame(data_df)) {
        stop("Input must be a data frame")
    }

    data_matrix <- as.matrix(data_df)
    n_series <- ncol(data_matrix)
    time_info <- data.frame(dttm = dttm, period = period)

    # Restore the previous future plan when the function exits.
    oplan <- plan(multisession, workers = workers)
    on.exit(plan(oplan), add = TRUE)

    results_list <- future_lapply(seq_len(n_series), function(i) {
        y <- data_matrix[, i]
        name <- colnames(data_matrix)[i]

        model <- model_fn(y)
        fit <- fitSSM(model, inits = inits, method = "BFGS")
        kfs <- KFS(fit$model, smoothing = c("signal", "disturbance"))

        # residuals() returns a time-series (ts) object; it is
        # converted to a plain numeric vector so that the saved data
        # frame holds ordinary numeric columns.
        residual <- as.numeric(residuals(kfs, type = "recursive"))
        fitted <- y - residual
        smoothed <- as.numeric(signal(kfs)$signal)
        epshat <- as.numeric(kfs$epshat)
        etahat <- kfs$etahat

        series_df <- data.frame(
            original = y,
            fitted = fitted,
            residual = residual,
            smoothed = smoothed,
            epshat = epshat
        )

        # Add estimated state disturbances.
        if (!is.null(etahat)) {
            for (j in seq_len(ncol(etahat))) {
                series_df[[paste0("etahat", j)]] <- etahat[, j]
            }
        }

        # Prefix each output column with the monitoring-point name.
        colnames(series_df) <- paste0(name, "_", colnames(series_df))
        series_df
    })

    all_results <- cbind(time_info, do.call(cbind, results_list))

    # Extract residuals for subsequent analysis.
    residual_cols <- grep("_residual$", colnames(all_results), value = TRUE)
    residuals_only <- all_results[, c("dttm", "period", residual_cols)]

    list(all = all_results, residuals = residuals_only)
}

# ---------------------------------------------------------------------------
# Import imputed temperature data
# ---------------------------------------------------------------------------

df <- readRDS(input_file)
dttm <- df$dttm
period <- df$period
df <- df[, -c(1, 2)] # the remaining columns are the six site series

# ---------------------------------------------------------------------------
# Create 24-hour harmonic regressors
# ---------------------------------------------------------------------------

# Temperature data are sampled every 10 minutes, giving 144
# observations per 24 hours. Four harmonics therefore represent
# periods of 24, 12, 8 and 6 hours.

t <- seq_len(nrow(df))

X <- cbind(
    sin(2 * pi * t / 144), cos(2 * pi * t / 144),
    sin(4 * pi * t / 144), cos(4 * pi * t / 144),
    sin(6 * pi * t / 144), cos(6 * pi * t / 144),
    sin(8 * pi * t / 144), cos(8 * pi * t / 144)
)

# ---------------------------------------------------------------------------
# Define state-space model
# ---------------------------------------------------------------------------

# Each monitoring-point series is modelled using a local-level
# structural component and four pairs of fixed harmonic regressors
# representing recurring 24-, 12-, 8- and 6-hour structure. The
# state and observation variances are estimated by fitSSM().

model_fn <- function(y) SSModel(y ~ SSMtrend(degree = 1, Q = NA) + X, H = NA)

# ---------------------------------------------------------------------------
# Fit, filter and smooth the six monitoring-point series
# ---------------------------------------------------------------------------

# Initial values for the variance parameters estimated by fitSSM()
# and the number of parallel workers are defined in
# kalman_filter_wide_parallel(). Parallel processing is used because
# each monitoring-point series is modelled independently.

kalman_results <- kalman_filter_wide_parallel(
    df,
    model_fn,
    dttm = dttm,
    period = period
)

# ---------------------------------------------------------------------------
# Write residuals
# ---------------------------------------------------------------------------

# Only the residuals are needed by the subsequent scripts, so only
# these are saved. The complete set of fitted quantities remains
# available in kalman_results$all if it is required for inspection.

saveRDS(kalman_results$residuals, output_file)
