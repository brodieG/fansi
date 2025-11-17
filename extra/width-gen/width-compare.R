# Compare Unicode width calculations between fansi::nchar_ctl and base R nchar
# Tests all valid Unicode codepoints

library(fansi)

# Generate all valid Unicode codepoints as characters
# Unicode range is 0x0000 to 0x10FFFF
cat("Generating all Unicode codepoints...\n")

# Create vector of all codepoints
all_codepoints <- 0:0x10FFFF

# Convert to characters (this may take a moment)
all_chars <- intToUtf8(all_codepoints, multiple = TRUE)

cat(sprintf("Generated %d codepoints\n", length(all_chars)))

# Compute widths using both methods
cat("Computing widths with fansi::nchar_ctl...\n")
fansi_widths <- nchar_ctl(all_chars, type = 'width')

cat("Computing widths with base nchar...\n")
base_widths <- nchar(all_chars, type = 'width')

# Compare results
cat("Comparing results...\n")
differences <- fansi_widths != base_widths

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat(sprintf("Total codepoints tested: %d\n", length(all_chars)))
cat(sprintf("Number of differences: %d (%.2f%%)\n", 
            sum(differences), 
            100 * sum(differences) / length(all_chars)))

if (sum(differences) > 0) {
  cat("\n=== DIFFERENCE BREAKDOWN ===\n")
  
  # Get indices where they differ
  diff_idx <- which(differences)
  diff_codepoints <- all_codepoints[diff_idx]
  diff_fansi <- fansi_widths[diff_idx]
  diff_base <- base_widths[diff_idx]
  
  # Create summary table
  diff_summary <- data.frame(
    codepoint = sprintf("U+%04X", diff_codepoints),
    char = all_chars[diff_idx],
    fansi_width = diff_fansi,
    base_width = diff_base,
    stringsAsFactors = FALSE
  )
  
  # Show first 20 differences
  cat("\nFirst 20 differences:\n")
  print(head(diff_summary, 20))
  
  # Categorize by width difference
  cat("\n=== WIDTH DIFFERENCE PATTERNS ===\n")
  width_diff <- diff_fansi - diff_base
  diff_pattern <- table(sprintf("fansi=%d base=%d", diff_fansi, diff_base))
  print(diff_pattern)
  
  # Show codepoint ranges where differences occur
  cat("\n=== CODEPOINT RANGES WITH DIFFERENCES ===\n")
  
  # Find contiguous ranges
  if (length(diff_codepoints) > 0) {
    ranges <- list()
    start <- diff_codepoints[1]
    end <- diff_codepoints[1]
    
    for (i in 2:length(diff_codepoints)) {
      if (diff_codepoints[i] == end + 1) {
        end <- diff_codepoints[i]
      } else {
        ranges[[length(ranges) + 1]] <- c(start, end)
        start <- diff_codepoints[i]
        end <- diff_codepoints[i]
      }
    }
    ranges[[length(ranges) + 1]] <- c(start, end)
    
    # Show first 20 ranges
    cat("First 20 ranges:\n")
    for (i in 1:min(20, length(ranges))) {
      r <- ranges[[i]]
      if (r[1] == r[2]) {
        cat(sprintf("  U+%04X\n", r[1]))
      } else {
        cat(sprintf("  U+%04X..U+%04X (%d codepoints)\n", r[1], r[2], r[2] - r[1] + 1))
      }
    }
    
    if (length(ranges) > 20) {
      cat(sprintf("  ... and %d more ranges\n", length(ranges) - 20))
    }
  }
  
  # Save full results to CSV if there are differences
  # csv_file <- "width_differences.csv"
  # write.csv(diff_summary, csv_file, row.names = FALSE)
  # cat(sprintf("\nFull difference list saved to: %s\n", csv_file))
  
} else {
  cat("\nNo differences found! fansi and base R agree on all codepoints.\n")
}

# Additional statistics
cat("\n=== WIDTH DISTRIBUTION ===\n")
cat("fansi::nchar_ctl:\n")
print(table(fansi_widths))
cat("\nbase nchar:\n")
print(table(base_widths))
