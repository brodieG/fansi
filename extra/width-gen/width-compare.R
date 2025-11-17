#!/usr/bin/env Rscript
# Compare Unicode width calculations between fansi::nchar_ctl, base R nchar, and utf8::utf8_width
# Tests all valid Unicode codepoints (excluding surrogates)

library(fansi)
library(utf8)

# Define surrogate range
SURROGATE_START <- 0xD800
SURROGATE_END <- 0xDFFF
surrogate_range <- SURROGATE_START:SURROGATE_END

# Generate all valid Unicode codepoints as characters
# Unicode range is 0x0000 to 0x10FFFF, excluding surrogate range
cat("Generating all Unicode codepoints (excluding surrogates)...\n")

# Create vector of all codepoints, excluding surrogates
all_codepoints <- setdiff(0:0x10FFFF, surrogate_range)

# Convert to characters (this may take a moment)
all_chars <- intToUtf8(all_codepoints, multiple = TRUE)

cat(sprintf("Generated %d codepoints (excluding %d surrogates)\n",
            length(all_chars), length(surrogate_range)))

# Compute widths using all three methods
cat("Computing widths with fansi::nchar_ctl...\n")
fansi_widths <- nchar_ctl(all_chars, type = 'width')

cat("Computing widths with base nchar...\n")
base_widths <- nchar(all_chars, type = 'width')

cat("Computing widths with utf8::utf8_width...\n")
utf8_widths <- utf8_width(all_chars, encode = FALSE)

# Create pairwise comparison tables
create_comparison_table <- function(width1, width2, name1, name2) {
  # Get all unique values including NA
  all_values <- sort(unique(c(width1, width2, na.omit(c(width1, width2)))))
  if (any(is.na(width1)) || any(is.na(width2))) {
    all_values <- c(all_values[!is.na(all_values)], NA)
  }

  # Create table
  tbl <- table(
    factor(width1, levels = all_values, exclude = NULL),
    factor(width2, levels = all_values, exclude = NULL),
    dnn = c(name1, name2)
  )

  return(tbl)
}

cat("\n=== PAIRWISE COMPARISON TABLES ===\n")

cat("\n--- fansi vs base ---\n")
fansi_base_table <- create_comparison_table(fansi_widths, base_widths, "fansi", "base")
print(fansi_base_table)

cat("\n--- fansi vs utf8 ---\n")
fansi_utf8_table <- create_comparison_table(fansi_widths, utf8_widths, "fansi", "utf8")
print(fansi_utf8_table)

cat("\n--- base vs utf8 ---\n")
base_utf8_table <- create_comparison_table(base_widths, utf8_widths, "base", "utf8")
print(base_utf8_table)

# Calculate agreement statistics
fansi_vs_base <- (!is.na(fansi_widths) & !is.na(base_widths) & fansi_widths != base_widths) |
                 (is.na(fansi_widths) != is.na(base_widths))
fansi_vs_utf8 <- (!is.na(fansi_widths) & !is.na(utf8_widths) & fansi_widths != utf8_widths) |
                 (is.na(fansi_widths) != is.na(utf8_widths))
base_vs_utf8 <- (!is.na(base_widths) & !is.na(utf8_widths) & base_widths != utf8_widths) |
                (is.na(base_widths) != is.na(utf8_widths))
any_diff <- fansi_vs_base | fansi_vs_utf8 | base_vs_utf8

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat(sprintf("Total codepoints tested: %d\n", length(all_chars)))
cat(sprintf("fansi vs base differences: %d (%.2f%%)\n",
            sum(fansi_vs_base),
            100 * sum(fansi_vs_base) / length(all_chars)))
cat(sprintf("fansi vs utf8 differences: %d (%.2f%%)\n",
            sum(fansi_vs_utf8),
            100 * sum(fansi_vs_utf8) / length(all_chars)))
cat(sprintf("base vs utf8 differences: %d (%.2f%%)\n",
            sum(base_vs_utf8),
            100 * sum(base_vs_utf8) / length(all_chars)))
cat(sprintf("Any differences: %d (%.2f%%)\n",
            sum(any_diff),
            100 * sum(any_diff) / length(all_chars)))

# Additional statistics
cat("\n=== WIDTH DISTRIBUTION ===\n")
cat("fansi::nchar_ctl:\n")
print(table(fansi_widths, useNA = "ifany"))
cat("\nbase nchar:\n")
print(table(base_widths, useNA = "ifany"))
cat("\nutf8::utf8_width:\n")
print(table(utf8_widths, useNA = "ifany"))
