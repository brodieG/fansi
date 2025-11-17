#!/usr/bin/env Rscript

## Copyright (C) Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

# Generate C lookup tables for Unicode character display widths.
# Reads UCD data from a local directory containing the unzipped database.
# Uses base R only - no external packages required.

#' Read file from UCD directory
read_ucd_file <- function(ucd_dir, filename) {
  filepath <- file.path(ucd_dir, filename)
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }
  cat("Reading", filepath, "...\n")
  readLines(filepath, warn = FALSE)
}

#' Extract UCD version from ReadMe.txt or DerivedAge.txt
get_ucd_version <- function(ucd_dir) {
  # Try ReadMe.txt first
  readme_path <- file.path(ucd_dir, "ReadMe.txt")
  ucd_version <- NA_character_
  if (file.exists(readme_path)) {
    lines <- readLines(readme_path, warn = FALSE)
    version_lines <- grep("Version [0-9]+\\.[0-9]+\\.[0-9]+", lines, value = TRUE)
    if (length(version_lines) > 0) {
      match <- regmatches(version_lines[1], regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", version_lines[1]))
      if (length(match) > 0) ucd_version <- match[1]
    }
  }
  if(anyNA(ucd_version)) stop("Could not parse UCD version.")
  ucd_version
}

#' Check if file is complete (contains EOF marker or high codepoint)
check_file_complete <- function(lines, filename) {
  # Check for EOF marker
  if (any(grepl("# *EOF", lines))) {
    return(TRUE)
  }

  # Check for high codepoint (10FFFD or nearby)
  if (any(grepl("10FFF[D-F]", lines, ignore.case = TRUE))) {
    return(TRUE)
  }

  warning(sprintf("File %s may be incomplete (no EOF marker or high codepoint found)", filename))
  return(FALSE)
}

#' Parse a Unicode range like '1F300..1F5FF' or single codepoint '0000'
parse_range <- function(range_str) {
  if (grepl("\\.\\.", range_str)) {
    parts <- strsplit(range_str, "\\.\\.")[[1]]
    start <- strtoi(parts[1], 16L)
    end <- strtoi(parts[2], 16L)
    return(c(start, end))
  } else {
    cp <- strtoi(range_str, 16L)
    return(c(cp, cp))
  }
}

#' Parse EastAsianWidth.txt and return data frame
parse_east_asian_width <- function(lines) {
  # Check file completeness
  check_file_complete(lines, "EastAsianWidth.txt")

  # Strip comments
  lines <- sub("#.*", "", lines)

  # Remove empty lines
  lines <- lines[nchar(trimws(lines)) > 0]

  # Use read.csv with semicolon delimiter (handles trailing empty fields)
  con <- textConnection(lines)
  fields_df <- read.csv(con, sep = ";", header = FALSE, stringsAsFactors = FALSE,
                        strip.white = TRUE, fill = TRUE, blank.lines.skip = TRUE)
  close(con)

  # Confirm all lines have the same number of fields
  field_counts <- ncol(fields_df)
  if (field_counts < 2) {
    stop(sprintf("EastAsianWidth.txt: Expected at least 2 fields, found %d", field_counts))
  }

  # Extract range and width category
  ranges <- fields_df[, 1]
  widths <- fields_df[, 2]

  # Expand ranges into individual codepoints
  codepoints <- integer()
  width_values <- character()

  for (i in seq_along(ranges)) {
    range_vals <- parse_range(ranges[i])
    start <- range_vals[1]
    end <- range_vals[2]

    cps <- start:end
    codepoints <- c(codepoints, cps)
    width_values <- c(width_values, rep(widths[i], length(cps)))
  }

  data.frame(codepoint = codepoints, ea_width = width_values, stringsAsFactors = FALSE)
}

#' Parse UnicodeData.txt and return data frame
parse_unicode_data <- function(lines) {
  # Check file completeness
  check_file_complete(lines, "UnicodeData.txt")

  # Strip comments
  lines <- sub("#.*", "", lines)

  # Remove empty lines
  lines <- lines[nchar(trimws(lines)) > 0]

  # Use read.csv with semicolon delimiter (handles trailing empty fields)
  con <- textConnection(lines)
  fields_df <- read.csv(con, sep = ";", header = FALSE, stringsAsFactors = FALSE,
                        strip.white = TRUE, fill = TRUE, blank.lines.skip = TRUE)
  close(con)

  # Confirm we have at least 3 fields
  field_counts <- ncol(fields_df)
  if (field_counts < 3) {
    stop(sprintf("UnicodeData.txt: Expected at least 3 fields, found %d", field_counts))
  }

  # Extract codepoint and category (columns 1 and 3)
  codepoints <- strtoi(fields_df[, 1], 16L)
  categories <- fields_df[, 3]

  data.frame(codepoint = codepoints, category = categories, stringsAsFactors = FALSE)
}

#' Parse emoji-data.txt and return vector of emoji codepoints
parse_emoji_data <- function(lines) {
  # Check file completeness
  check_file_complete(lines, "emoji-data.txt")

  # Strip comments
  lines <- sub("#.*", "", lines)

  # Remove empty lines
  lines <- lines[nchar(trimws(lines)) > 0]

  # Use read.csv with semicolon delimiter (handles trailing empty fields)
  con <- textConnection(lines)
  fields_df <- read.csv(con, sep = ";", header = FALSE, stringsAsFactors = FALSE,
                        strip.white = TRUE, fill = TRUE, blank.lines.skip = TRUE)
  close(con)

  # Confirm we have at least 2 fields
  field_counts <- ncol(fields_df)
  if (field_counts < 2) {
    stop(sprintf("emoji-data.txt: Expected at least 2 fields, found %d", field_counts))
  }

  # Extract range and property
  ranges <- fields_df[, 1]
  props <- fields_df[, 2]

  # Filter for Emoji_Presentation (characters that default to emoji rendering)
  # These should be width 2
  emoji_pres_idx <- which(props == "Emoji_Presentation")

  # Expand ranges into individual codepoints
  emoji_cps <- integer()

  for (i in emoji_pres_idx) {
    range_vals <- parse_range(ranges[i])
    start <- range_vals[1]
    end <- range_vals[2]
    emoji_cps <- c(emoji_cps, start:end)
  }

  unique(emoji_cps)
}

#' Compress width assignments into contiguous ranges
compress_ranges <- function(width_map) {
  # Only compress non-default widths (0 and 2)
  # Width 1 is the default and doesn't need a lookup table
  ranges <- list()

  for (w in c(0, 2)) {
    subset_map <- width_map[width_map$width == w, ]

    if (nrow(subset_map) == 0) {
      next
    }

    # Sort by codepoint
    subset_map <- subset_map[order(subset_map$codepoint), ]

    start <- subset_map$codepoint[1]
    current_width <- w
    range_list <- list()

    for (i in 2:nrow(subset_map)) {
      cp <- subset_map$codepoint[i]

      # If contiguous, continue range
      if (cp == subset_map$codepoint[i-1] + 1) {
        next
      }

      # Otherwise, save the range and start new one
      range_list[[length(range_list) + 1]] <- c(start, subset_map$codepoint[i-1])
      start <- cp
    }

    # Don't forget the last range
    range_list[[length(range_list) + 1]] <- c(start, subset_map$codepoint[nrow(subset_map)])

    # Convert to data frame
    ranges[[as.character(w)]] <- data.frame(
      start = sapply(range_list, `[`, 1),
      end = sapply(range_list, `[`, 2),
      stringsAsFactors = FALSE
    )
  }

  ranges
}

#' Generate C code for width lookup tables
generate_c_code <- function(ranges, ucd_version) {
  c_code <- c(
    "/*",
    " * Copyright (C) Brodie Gaslam",
    " *",
    " * This file is part of \"fansi - ANSI Control Sequence Aware String Functions\"",
    " *",
    " * This program is free software: you can redistribute it and/or modify",
    " * it under the terms of the GNU General Public License as published by",
    " * the Free Software Foundation, either version 2 or 3 of the License.",
    " *",
    " * This program is distributed in the hope that it will be useful,",
    " * but WITHOUT ANY WARRANTY; without even the implied warranty of",
    " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
    " * GNU General Public License for more details.",
    " *",
    " * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.",
    " */",
    "",
    "#include \"fansi.h\"",
    "",
    "/*",
    " * Unicode Character Display Width Lookup Tables",
    sprintf(" * Generated from Unicode Character Database version %s", ucd_version),
    " * ",
    " * Width meanings:",
    " *   0 = Zero width (control characters, combining marks)",
    " *   1 = Normal width (most ASCII and Latin characters)",
    " *   2 = Double width (CJK characters, emoji)",
    " *",
    " * Special cases and exceptions:",
    " * - Regional Indicator Symbols (U+1F1E6..U+1F1FF) are width 1",
    " *   These are letter symbols A-Z used to compose flag emojis when paired.",
    " *",
    " * - Hangul Jamo medial vowels and final consonants (U+1160..U+11FF) are width 0",
    " *   These combine to form Hangul syllables and don't occupy their own column.",
    " *",
    " * - SOFT HYPHEN (U+00AD) is width 1",
    " *   Despite being a format character (Cf), it has visible representation.",
    " *",
    " * Additionally fansi adds some further postprocessing in read_utf8_until()",
    " *",
    " * Reference: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c",
    " */",
    "",
    c("// UNICODE_VERSION is guaranteed to be only numbers and period."),
    sprintf("#define UNICODE_VERSION \"%s\"", ucd_version),
    "",
    "typedef struct {",
    "    int start;",
    "    int end;",
    "} unicode_range_t;",
    ""
  )

  # Generate tables for each width
  width_keys <- sort(as.integer(names(ranges)))

  for (width in width_keys) {
    range_df <- ranges[[as.character(width)]]
    c_code <- c(c_code, sprintf("/* Width %d ranges: %d entries */", width, nrow(range_df)))
    c_code <- c(c_code, sprintf("static const unicode_range_t width_%d_ranges[] = {", width))

    for (i in 1:nrow(range_df)) {
      c_code <- c(c_code, sprintf("    {0x%04X, 0x%04X},", range_df$start[i], range_df$end[i]))
    }

    c_code <- c(c_code, "};")
    c_code <- c(c_code, sprintf("#define WIDTH_%d_COUNT %d", width, nrow(range_df)))
    c_code <- c(c_code, "")
  }

  # Generate lookup function
  c_code <- c(c_code,
    "/* Binary search in a range table */",
    "static int in_range_table(int cp, const unicode_range_t *table, int count) {",
    "    int left = 0, right = count - 1;",
    "    while (left <= right) {",
    "        int mid = (left + right) / 2;",
    "        if (cp < table[mid].start) {",
    "            right = mid - 1;",
    "        } else if (cp > table[mid].end) {",
    "            left = mid + 1;",
    "        } else {",
    "            return 1;",
    "        }",
    "    }",
    "    return 0;",
    "}",
    "",
    "/* Get display width for a Unicode codepoint */",
    "int FANSI_unicode_width(int cp) {",
    "    /* Fast path for printable ASCII */",
    "    if (cp >= 0x20 && cp < 0x7F)",
    "        return 1;"
  )

  # Check each width in order (0, 2, then default to 1)
  if ("0" %in% names(ranges)) {
    c_code <- c(c_code,
      "",
      "    /* Check zero-width characters */",
      "    if (in_range_table(cp, width_0_ranges, WIDTH_0_COUNT))",
      "        return 0;"
    )
  }

  if ("2" %in% names(ranges)) {
    c_code <- c(c_code,
      "",
      "    /* Check double-width characters */",
      "    if (in_range_table(cp, width_2_ranges, WIDTH_2_COUNT))",
      "        return 2;"
    )
  }

  c_code <- c(c_code,
    "",
    "    /* Default to width 1 */",
    "    return 1;",
    "}"
  )

  c_code <- c(c_code,
    "SEXP FANSI_unicode_version(void) {",
    "    return mkString(UNICODE_VERSION);",
    "}"
  )
  paste(c_code, collapse = "\n")
}

# Main execution
main <- function(ucd_dir = "") {
  # Parse command line arguments if not provided
  if (!nzchar(ucd_dir)) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) < 1) {
      cat("Usage: Rscript unicode_width_generator.r <ucd_directory>\n")
      cat("\n")
      cat("Example: Rscript unicode_width_generator.r /path/to/UCD\n")
      cat("\n")
      cat("For interactive use: source('unicode_width_generator.r'); main('/path/to/UCD')\n")
      cat("\n")
      cat("The UCD directory should contain the unzipped Unicode Character Database\n")
      cat("with files like EastAsianWidth.txt, UnicodeData.txt, and emoji/emoji-data.txt\n")
      quit(status = 1)
    }

    ucd_dir <- args[1]
  }

  if (!dir.exists(ucd_dir)) {
    stop(sprintf("Directory does not exist: %s", ucd_dir))
  }

  cat("Reading UCD data from:", ucd_dir, "\n")

  # Get UCD version
  ucd_version <- get_ucd_version(ucd_dir)
  cat("UCD Version:", ucd_version, "\n\n")

  # Read and parse data files
  ea_data <- read_ucd_file(ucd_dir, "EastAsianWidth.txt")
  unicode_data <- read_ucd_file(ucd_dir, "UnicodeData.txt")
  emoji_data <- read_ucd_file(file.path(ucd_dir, "emoji"), "emoji-data.txt")

  cat("\nParsing data...\n")
  ea_widths <- parse_east_asian_width(ea_data)
  categories <- parse_unicode_data(unicode_data)
  emoji_cps <- parse_emoji_data(emoji_data)

  cat("Determining widths for all codepoints...\n")

  # Create data frame with all Unicode codepoints
  all_codepoints <- data.frame(
    codepoint = 0:0x10FFFF,
    width = 1L,  # Default width
    stringsAsFactors = FALSE
  )

  # Merge in categories
  all_codepoints <- merge(all_codepoints, categories, by = "codepoint", all.x = TRUE)

  # Merge in East Asian widths
  all_codepoints <- merge(all_codepoints, ea_widths, by = "codepoint", all.x = TRUE)

  # Add emoji flag
  all_codepoints$is_emoji <- all_codepoints$codepoint %in% emoji_cps

  # Sort by codepoint to maintain order
  all_codepoints <- all_codepoints[order(all_codepoints$codepoint), ]

  # Vectorized width assignment (in priority order, later assignments override earlier)

  # Start with default width 1 (already set)

  # East Asian Wide/Fullwidth → width 2
  ea_wide <- !is.na(all_codepoints$ea_width) & all_codepoints$ea_width %in% c("F", "W")
  all_codepoints$width[ea_wide] <- 2L

  # Emoji → width 2
  all_codepoints$width[all_codepoints$is_emoji] <- 2L

  # Regional Indicator Symbols (U+1F1E6..U+1F1FF) → width 1
  # These are the letter symbols A-Z used to compose flag emojis. Individual
  # Regional Indicators should be width 1 because they need to be paired to form
  # a visible flag emoji (which would be width 2 for the pair as a whole).
  # This overrides the Emoji_Presentation property which would make them width 2.
  # Internally in fansi we treat the first one that shows up as width 2, and the
  # second as width 0, so this is not strictly needed.
  regional_indicators <- all_codepoints$codepoint >= 0x1F1E6 &
                         all_codepoints$codepoint <= 0x1F1FF
  all_codepoints$width[regional_indicators] <- 1L

  # Control characters → width 0 (highest priority, applies last)
  # C0 and C1 control characters
  c0_c1 <- all_codepoints$codepoint < 0x20 |
           (all_codepoints$codepoint >= 0x7F & all_codepoints$codepoint < 0xA0)
  all_codepoints$width[c0_c1] <- 0L

  # Categories that are zero-width
  zero_width_cats <- !is.na(all_codepoints$category) &
                     all_codepoints$category %in% c("Cc", "Cf", "Mn", "Me")
  all_codepoints$width[zero_width_cats] <- 0L

  # SOFT HYPHEN (U+00AD) → width 1
  # This is a format character (Cf) which would normally be width 0, but it has
  # a visible representation and should be width 1.
  # Reference: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
  soft_hyphen <- all_codepoints$codepoint == 0x00AD
  all_codepoints$width[soft_hyphen] <- 1L

  # Hangul Jamo medial vowels and final consonants (U+1160..U+11FF) → width 0
  # These are combining characters used to compose Hangul syllables. Following
  # the wcwidth.c convention, they are treated as zero-width even though they
  # are general category "Lo" (Letter, other) rather than combining marks.
  # Reference: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
  hangul_jamo <- all_codepoints$codepoint >= 0x1160 &
                 all_codepoints$codepoint <= 0x11FF
  all_codepoints$width[hangul_jamo] <- 0L

  # Extract final width column
  width_map <- data.frame(
    codepoint = all_codepoints$codepoint,
    width = all_codepoints$width,
    stringsAsFactors = FALSE
  )

  cat("Compressing ranges...\n")
  ranges <- compress_ranges(width_map)

  cat("\nRange statistics:\n")
  for (width in sort(as.integer(names(ranges)))) {
    cat(sprintf("  Width %d: %d ranges\n", width, nrow(ranges[[as.character(width)]])))
  }

  cat("\nGenerating C code...\n")
  c_code <- generate_c_code(ranges, ucd_version)

  # Write to file
  writeLines(c_code, "width.c")

  cat("\nGenerated width.c\n")
  cat(sprintf("Unicode Version: %s\n", ucd_version))
  cat("Usage: int width = unicode_width(codepoint);\n")
}

# Run main function if not in interactive mode
if (!interactive()) main()
