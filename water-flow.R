
source("_common.R")

#-------------------------------------------------
# simulated annealing, water-flow inspired
#-------------------------------------------------

anneal <- function (values, groups.initial) {
  # how good is this solution?
  fitness <- function (groups) {
    sd(tapply(values, groups, sum))
  }
  # solution neighbor solution
  # -----------------------------
  # find the biggest group
  # find its boundaries, exclude edges
  # pick one of the boundaries, change its value to the neighbor
  # thereby shrinking the group by 1
  neighbor <- function (groups) {
    max.group <- which.max(tapply(values, groups, sum))
    range.i   <- range(which(groups == max.group))
    change.m  <- matrix(c(range.i, -1, 1), nrow=2)
    change.m  <- change.m[range.i > 1 & range.i < length(values),,drop=FALSE]
    change    <- change.m[sample(1:nrow(change.m), 1),]
    groups[change[1]] <- max.group + change[2]
    groups
  }
  optim(groups.initial,
        fitness,
        neighbor,
        method="SANN",
        control=list(trace=1, temp=1000))
}

#-------------------------------------------------

# worst case, completely uneven groups, all in group-1
groups <- c(rep(1, 49), 2:10)

result <- anneal(tracks$seconds, groups)
report(tracks$seconds, result$par)

