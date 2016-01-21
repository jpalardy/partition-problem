
source("_common.R")

sameCutoff.groups <- function (values, cutoff) {
  result <- rep(NA, length(values))
  k   <- 1
  acc <- 0
  for (i in seq_along(values)) {
    acc <- acc + values[i]
    if (acc > cutoff) {
      k <- k + 1
      acc <- values[i]
    }
    result[i] <- k
  }
  result
}

groups = sameCutoff.groups(tracks$seconds, 1680.5)
report(tracks$seconds, groups)

