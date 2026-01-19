

# norm distances
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


