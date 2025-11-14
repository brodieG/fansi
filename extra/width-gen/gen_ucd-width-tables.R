#!/usr/bin/env Rscript
# Generate C lookup tables for Unicode character display widths.
# Downloads UCD data and produces efficient range-based tables.
# Uses base R only - no external packages required.

# UCD data URLs
EAST_ASIAN_WIDTH_URL <- "https://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt"
UNICODE_DATA_URL <- "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt"
EMOJI_DATA_URL <- "https://www.unicode.org/Public/UCD/latest/ucd/emoji/emoji-data.txt"

#' Download and return content from URL
fetch_url <- function(url) {
  cat("Fetching", url, "...\n")
  con <- url(url, "r")
  data <- readLines(con, warn = FALSE)
  close(con)
  paste(data, collapse = "\n")
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
parse_east_asian_width <- function(data) {
  lines <- strsplit(data, "\n")[[1]]
  
  codepoints <- integer()
  widths <- character()
  
  for (line in lines) {
    # Remove comments
    line <- sub("#.*", "", line)
    line <- trimws(line)
    if (nchar(line) == 0) next
    
    parts <- strsplit(line, ";")[[1]]
    if (length(parts) < 2) next
    
    range_str <- trimws(parts[1])
    width_cat <- trimws(parts[2])
    
    range_vals <- parse_range(range_str)
    start <- range_vals[1]
    end <- range_vals[2]
    
    for (cp in start:end) {
      codepoints <- c(codepoints, cp)
      widths <- c(widths, width_cat)
    }
  }
  
  data.frame(codepoint = codepoints, width = widths, stringsAsFactors = FALSE)
}

#' Parse UnicodeData.txt and return data frame
parse_unicode_data <- function(data) {
  lines <- strsplit(data, "\n")[[1]]
  
  codepoints <- integer()
  categories <- character()
  
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0) next
    
    fields <- strsplit(line, ";")[[1]]
    if (length(fields) < 3) next
    
    cp <- strtoi(fields[1], 16L)
    category <- fields[3]
    
    codepoints <- c(codepoints, cp)
    categories <- c(categories, category)
  }
  
  data.frame(codepoint = codepoints, category = categories, stringsAsFactors = FALSE)
}

#' Parse emoji-data.txt and return vector of emoji codepoints
parse_emoji_data <- function(data) {
  lines <- strsplit(data, "\n")[[1]]
  
  emoji_cps <- integer()
  
  for (line in lines) {
    # Remove comments
    line <- sub("#.*", "", line)
    line <- trimws(line)
    if (nchar(line) == 0) next
    
    parts <- strsplit(line, ";")[[1]]
    if (length(parts) < 2) next
    
    range_str <- trimws(parts[1])
    prop <- trimws(parts[2])
    
    # Extended_Pictographic covers most emoji
    if (prop == "Extended_Pictographic") {
      range_vals <- parse_range(range_str)
      start <- range_vals[1]
      end <- range_vals[2]
      
      emoji_cps <- c(emoji_cps, start:end)
    }
  }
  
  unique(emoji_cps)
}

#' Determine display width for a codepoint
determine_width <- function(cp, ea_widths, categories, emoji_cps) {
  # Control characters (C0, C1) are zero width
  if (cp < 0x20 || (cp >= 0x7F && cp < 0xA0)) {
    return(0)
  }
  
  # Get category
  category <- categories$category[categories$codepoint == cp]
  if (length(category) == 0) category <- ""
  
  # Other control characters
  if (category %in% c("Cc", "Cf", "Mn", "Me")) {
    return(0)
  }
  
  # Emoji are typically width 2
  if (cp %in% emoji_cps) {
    return(2)
  }
  
  # East Asian Width
  ea_width <- ea_widths$width[ea_widths$codepoint == cp]
  if (length(ea_width) == 0) ea_width <- "N"
  
  # Fullwidth and Wide characters
  if (ea_width %in% c("F", "W")) {
    return(2)
  }
  
  # Default width
  return(1)
}

#' Compress width assignments into contiguous ranges
compress_ranges <- function(width_map) {
  # Sort by codepoint
  sorted_df <- width_map[order(width_map$codepoint), ]
  
  if (nrow(sorted_df) == 0) {
    return(list())
  }
  
  ranges_list <- list()
  
  start <- sorted_df$codepoint[1]
  current_width <- sorted_df$width[1]
  
  for (i in 2:nrow(sorted_df)) {
    cp <- sorted_df$codepoint[i]
    width <- sorted_df$width[i]
    
    # If contiguous and same width, continue range
    if (cp == sorted_df$codepoint[i-1] + 1 && width == current_width) {
      next
    }
    
    # Otherwise, save the range and start new one
    if (is.null(ranges_list[[as.character(current_width)]])) {
      ranges_list[[as.character(current_width)]] <- data.frame(
        start = integer(), end = integer(), stringsAsFactors = FALSE
      )
    }
    
    ranges_list[[as.character(current_width)]] <- rbind(
      ranges_list[[as.character(current_width)]],
      data.frame(start = start, end = sorted_df$codepoint[i-1])
    )
    
    start <- cp
    current_width <- width
  }
  
  # Don't forget the last range
  if (is.null(ranges_list[[as.character(current_width)]])) {
    ranges_list[[as.character(current_width)]] <- data.frame(
      start = integer(), end = integer(), stringsAsFactors = FALSE
    )
  }
  
  ranges_list[[as.character(current_width)]] <- rbind(
    ranges_list[[as.character(current_width)]],
    data.frame(start = start, end = sorted_df$codepoint[nrow(sorted_df)])
  )
  
  ranges_list
}

#' Generate C code for width lookup tables
generate_c_code <- function(ranges) {
  c_code <- c(
    "/*",
    " * Unicode Character Display Width Lookup Tables",
    " * Generated from Unicode Character Database",
    " * ",
    " * Width meanings:",
    " *   0 = Zero width (control characters, combining marks)",
    " *   1 = Normal width (most ASCII and Latin characters)",
    " *   2 = Double width (CJK characters, emoji)",
    " */",
    "",
    "#include <stdint.h>",
    "",
    "typedef struct {",
    "    uint32_t start;",
    "    uint32_t end;",
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
    "static int in_range_table(uint32_t cp, const unicode_range_t *table, int count) {",
    "    int left = 0, right = count - 1;",
    "    while (left <= right) {",
    "        int mid = (left + right) / 2;",
    "        if (cp < table[mid].start)",
    "            right = mid - 1;",
    "        else if (cp > table[mid].end)",
    "            left = mid - 1;",
    "        else",
    "            return 1;",
    "    }",
    "    return 0;",
    "}",
    "",
    "/* Get display width for a Unicode codepoint */",
    "int unicode_width(uint32_t cp) {"
  )
  
  # Check each width in order (0, 2, then default to 1)
  if ("0" %in% names(ranges)) {
    c_code <- c(c_code,
      "    /* Check zero-width characters first */",
      "    if (in_range_table(cp, width_0_ranges, WIDTH_0_COUNT))",
      "        return 0;",
      ""
    )
  }
  
  if ("2" %in% names(ranges)) {
    c_code <- c(c_code,
      "    /* Check double-width characters */",
      "    if (in_range_table(cp, width_2_ranges, WIDTH_2_COUNT))",
      "        return 2;",
      ""
    )
  }
  
  c_code <- c(c_code,
    "    /* Default to width 1 */",
    "    return 1;",
    "}"
  )
  
  paste(c_code, collapse = "\n")
}

# Main execution
main <- function() {
  cat("Downloading UCD data...\n")
  ea_data <- fetch_url(EAST_ASIAN_WIDTH_URL)
  unicode_data <- fetch_url(UNICODE_DATA_URL)
  emoji_data <- fetch_url(EMOJI_DATA_URL)
  
  cat("Parsing data...\n")
  ea_widths <- parse_east_asian_width(ea_data)
  categories <- parse_unicode_data(unicode_data)
  emoji_cps <- parse_emoji_data(emoji_data)
  
  cat("Determining widths...\n")
  # Process Unicode range 0x0000 to 0x10FFFF
  # For efficiency, process in chunks
  width_map <- data.frame(codepoint = integer(), width = integer(), stringsAsFactors = FALSE)
  
  chunk_size <- 0x10000
  for (chunk_start in seq(0, 0x10FFFF, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, 0x10FFFF)
    
    for (cp in chunk_start:chunk_end) {
      width <- determine_width(cp, ea_widths, categories, emoji_cps)
      width_map <- rbind(width_map, data.frame(codepoint = cp, width = width))
    }
    
    if (chunk_start %% 0x40000 == 0) {
      cat(sprintf("  Processed up to U+%06X\n", chunk_end))
    }
  }
  
  cat("Compressing ranges...\n")
  ranges <- compress_ranges(width_map)
  
  cat("\nRange statistics:\n")
  for (width in sort(as.integer(names(ranges)))) {
    cat(sprintf("  Width %d: %d ranges\n", width, nrow(ranges[[as.character(width)]])))
  }
  
  cat("\nGenerating C code...\n")
  c_code <- generate_c_code(ranges)
  
  # Write to file
  writeLines(c_code, "unicode_width.c")
  
  cat("\nGenerated unicode_width.c\n")
  cat("Usage: int width = unicode_width(codepoint);\n")
}

# Run main function
main()
