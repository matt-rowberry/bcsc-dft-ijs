#####

## PREPARING SEGMENT DATA FOR FIGURE 10
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 27 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(dplyr)

#####

## 02. LOAD DATA

df <- readRDS(
    "data/kalman_filtered_multiband_segments.rds"
)

#####

## 03. PREPARE DATA

figure_10 <- bind_rows(
    df %>%
        filter(
            band == "nb3",
            segment <= 311
        ) %>%
        mutate(
            period = 1
        ),

    df %>%
        filter(
            band == "nb3",
            segment >= 311,
            segment <= 557
        ) %>%
        mutate(
            period = 2
        ),

    df %>%
        filter(
            band == "nb3",
            segment >= 557,
            segment <= 773
        ) %>%
        mutate(
            period = 3
        ),

    df %>%
        filter(
            band == "nb3",
            segment >= 773,
            segment <= 1161
        ) %>%
        mutate(
            period = 4
        ),

    df %>%
        filter(
            band == "nb3",
            segment >= 1161,
            segment <= 1463
        ) %>%
        mutate(
            period = 5
        )
) %>%
    mutate(
        band_type = factor(
            paste(
                band,
                type,
                sep = "_"
            ),
            levels = c(
                "nb3_day",
                "nb3_night"
            )
        )
    )

#####

## 04. SAVE RESULTS

saveRDS(
    figure_10,
    "data/figure_10_segments.rds"
)
