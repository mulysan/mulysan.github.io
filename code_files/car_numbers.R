library(data.table)
library(haven)

rm(list=ls())

# Load the Stata file
path <- "/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars"
cars <- as.data.table(read_dta(file.path(path, "cars.dta")))

#cars = head(cars,100000)

score_plates_dt <- function(DT, plate_col) {
  # Convert numbers to 8-digit strings
  DT[, num_str := sprintf("%08d", get(plate_col))]
  DT[, digits := lapply(strsplit(num_str, ""), as.numeric)]
  
  # 1. Special patterns (highest value cases)
  DT[, special_score := {
    sapply(num_str, function(n) {
      d <- as.numeric(strsplit(n, "")[[1]])
      
      # All same digits
      if(length(unique(d)) == 1) return(100)
      
      # Perfect pairs (11223344)
      if(all(d[seq(1,7,2)] == d[seq(2,8,2)]) && 
         length(unique(d[seq(1,7,2)])) == 4) return(90)
      
      # Repeated 4-digit pattern (12341234)
      if(identical(d[1:4], d[5:8])) return(85)
      
      return(0)
    })
  }]
  
  # 2. Sequence score (continuous)
  DT[, sequence_score := {
    sapply(digits, function(d) {
      # Calculate differences
      diffs <- diff(d)
      
      # Perfect sequence
      if(all(diffs == 1) || all(diffs == -1)) return(80)
      
      # Count sequential pairs and weight by position
      sequential_pairs <- sum(abs(diffs) == 1)
      
      # Additional weight for consecutive sequences
      streak_len <- 1
      max_streak <- 1
      for(i in 2:length(d)) {
        if(abs(d[i] - d[i-1]) == 1) {
          streak_len <- streak_len + 1
          max_streak <- max(max_streak, streak_len)
        } else {
          streak_len <- 1
        }
      }
      
      base_score <- (sequential_pairs / 7) * 30
      streak_bonus <- (max_streak / 8) * 20
      
      return(base_score + streak_bonus)
    })
  }]
  
  # 3. Repeating patterns score (continuous)
  DT[, repeating_score := {
    sapply(digits, function(d) {
      # Run length encoding
      runs <- rle(d)
      max_run <- max(runs$lengths)
      
      # Score based on longest run
      run_score <- (max_run / 8) * 60
      
      # Bonus for multiple medium runs
      medium_runs <- sum(runs$lengths >= 3)
      medium_bonus <- (medium_runs / 3) * 20
      
      return(run_score + medium_bonus)
    })
  }]
  
  # 4. Pattern continuity score
  DT[, pattern_score := {
    sapply(digits, function(d) {
      # Measure digit-to-digit changes
      diffs <- diff(d)
      smoothness <- 1 - (sd(diffs) / 4.5)
      
      # Look for repeating subsequences
      subseq_score <- 0
      for(len in 2:4) {
        parts <- sapply(1:(8-len+1), function(i) paste(d[i:(i+len-1)], collapse=""))
        unique_ratio <- 1 - (length(unique(parts)) - 1) / length(parts)
        subseq_score <- max(subseq_score, unique_ratio * len * 5)
      }
      
      return(max(0, smoothness * 20 + subseq_score))
    })
  }]
  
  # Calculate final score with special patterns taking precedence
  DT[, plate_score := {
    ifelse(special_score > 0,
           special_score,
           pmin(100,
                sequence_score * 0.7 +
                  repeating_score * 0.6 +
                  pattern_score * 0.3
           )
    )
  }]
  
  # Clean up temporary columns
  DT[, c("num_str", "digits", "special_score", 
         "sequence_score", "repeating_score", 
         "pattern_score") := NULL]
  
  return(DT)
}

# Apply scoring
cars <- score_plates_dt(cars, "mispar_rechev")

# Show results
cat("\nTop 30 highest scoring plates:\n")
print(cars[order(-plate_score)][1:30, .(mispar_rechev, plate_score)])

# Distribution
cat("\nScore distribution:\n")
print(summary(cars$plate_score))

# Visualization
library(ggplot2)
p <- ggplot(cars, aes(x=plate_score)) +
  geom_histogram(bins=50, fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="Distribution of License Plate Scores",
       x="Score", y="Count")
print(p)

# Test specific cases
test_plates <- data.table(
  mispar_rechev = c(
    11111111,  # all same
    12121212,  # alternating
    12341234,  # repeated pattern
    12344321,  # palindrome
    11112222,  # two blocks
    11223344,  # pairs
    12345678,  # sequence
    88880000,  # block pattern
    12312312   # repeating triplet
  )
)
test_results <- score_plates_dt(test_plates, "mispar_rechev")
print("\nTest plate scores:")
print(test_results[order(-plate_score)])

# Create bins and calculate means for all three relationships using data.table
cars[, bin := ntile(plate_score, 25)]

# Function to create and save binscatter plots
create_binscatter <- function(DT, y_var, y_lab) {
  binned_dt <- DT[!is.na(get(y_var)), .(
    ymean = mean(get(y_var)),
    xmean = mean(plate_score)
  ), by = bin]
  
  p <- ggplot(binned_dt, aes(x = xmean, y = ymean)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", size = 1.5) +
    theme_minimal(base_size = 16) +
    labs(
      x = "License Plate Score",
      y = y_lab
    ) +
    theme(
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 16),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      aspect.ratio = 1  # Make plot square
    )
  
  return(p)
}

# Create plots
p1 <- create_binscatter(cars, "disable", "Proportion with Disability Status") +
  scale_y_continuous(labels = scales::percent_format(size = 16))

p2 <- create_binscatter(cars, "mishkal_kolel", "Car Weight") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", size = 16))

p3 <- create_binscatter(cars, "mehir", "Price") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", size = 16))

p4 <- create_binscatter(cars, "shnat_yitzur", "Car Year") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",",size = 16))

# Save plots with square dimensions
ggsave("/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars/plate_score_disability.png", p1, width = 6, height = 6, bg = "white", dpi = 300)
ggsave("/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars/plate_score_weight.png", p2, width = 6, height = 6, bg = "white", dpi = 300)
ggsave("/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars/plate_score_price.png", p3, width = 6, height = 6, bg = "white", dpi = 300)
ggsave("/Users/shmuelsan/Dropbox/HUJI/misc/twitter/cars/plate_score_year.png", p4, width = 6, height = 6, bg = "white", dpi = 300)
