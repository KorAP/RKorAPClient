#!/usr/bin/env Rscript
#
# Plot corpus sizes by year for Stern magazine
#
library(RKorAPClient)
library(ggplot2)
library(scales)  # For comma_format()

# Define years to analyze
years <- 1990:2024

# Create virtual corpus definitions for each year, restricted to Stern
vcs <- paste("corpusTitle=Stern & pubDate in", years)

# Connect to KorAP
kco <- KorAPConnection(verbose = TRUE)

# Get corpus statistics for each year
cat("Retrieving corpus sizes for Stern magazine from", min(years), "to", max(years), "...\n")
corpus_data <- corpusStats(kco, vc = vcs, as.df = TRUE)

# Add year column for plotting
corpus_data$year <- years

# Create ggplot column plot
g <- ggplot(corpus_data, aes(x = year, y = tokens)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1990, 2024, 5)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Corpus Size by Year - Stern Magazine",
    subtitle = "Number of tokens in DeReKo",
    x = "Year",
    y = "Number of Tokens"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(g)

# Print summary statistics
cat("\nSummary of corpus sizes:\n")
cat("Total years with data:", sum(corpus_data$tokens > 0, na.rm = TRUE), "\n")
cat("Peak year:", corpus_data$year[which.max(corpus_data$tokens)],
    "with", format(max(corpus_data$tokens, na.rm = TRUE), big.mark = ","), "tokens\n")
cat("Total tokens across all years:", format(sum(corpus_data$tokens, na.rm = TRUE), big.mark = ","), "\n")

# Return the data for further analysis
corpus_data
