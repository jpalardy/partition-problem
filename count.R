
source("_common.R")

sameCount.groups <- function (n, k) {
  breaks = cumsum(ceiling(rep(n/k, k)))
  findInterval(1:n, breaks) + 1
}

groups = sameCount.groups(58, 10)
report(tracks$seconds, groups)

