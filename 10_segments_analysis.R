#####

## PREPARING SEGMENT DATA FOR FIGURE 10
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 29 SEPTEMBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

# Per-segment minimum and maximum of the reconstructed signal, produced
# by 05_multiband_reconstruction.R.

input_file <- "data/kalman_filtered_multiband_segments.rds"
output_file <- "data/figure_10_segments.rds"
df <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Assign access regimes to segments
# ---------------------------------------------------------------------------

# Each access regime is identified by the range of day/night segments
# it contains. The boundary segments (311, 557, 773 and 1161) belong
# to both of the adjacent regimes, as in the original analysis.

figure_10 <- bind_rows(
    df %>% filter(band == "nb3", segment <= 311) %>% mutate(period = 1),
    df %>% filter(band == "nb3", segment >= 311, segment <= 557) %>% mutate(period = 2),
    df %>% filter(band == "nb3", segment >= 557, segment <= 773) %>% mutate(period = 3),
    df %>% filter(band == "nb3", segment >= 773, segment <= 1161) %>% mutate(period = 4),
    df %>% filter(band == "nb3", segment >= 1161, segment <= 1463) %>% mutate(period = 5)
) %>%
    mutate(band_type = factor(paste(band, type, sep = "_"), levels = c("nb3_day", "nb3_night")))

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------

saveRDS(figure_10, output_file)
