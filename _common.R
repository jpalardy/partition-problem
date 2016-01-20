
library(ggplot2)
library(gridExtra)

# "07:03" --> 423
text2seconds <- function (text) {
  sapply(strsplit(text, ":"), function (x) {
    as.numeric(x) %*% c(60, 1)
  })
}

# 423 --> "07:03"
seconds2text <- function (seconds) {
  sapply(seconds, function (x) {
    x <- floor(x)
    sprintf("%02d:%02d", x %/% 60, x %% 60)
  })
}

# reads the data file
# adds column names
# adds seconds column
read.tracks <- function (filename) {
  d <- read.delim(filename, header=FALSE, stringsAsFactors=FALSE)
  names(d)  <- c("name", "duration")
  d$seconds <- text2seconds(d$duration)
  d
}

tracks <- read.tracks("tracks-58.delim")

#-------------------------------------------------

plot.groups <- function (values, groups) {
  no <- factor(seq_along(values))
  group.durations <- tapply(values, groups, sum)
  group.names     <- factor(names(group.durations), names(group.durations))
  grid.arrange(
    ggplot(data.frame(no=no, values, groups), aes(no, values, fill=factor(groups))) +
      geom_bar(stat="identity") +
      xlab("tracks") +
      ylab("duration") +
      scale_x_discrete(breaks=seq(0, 60, by=5)) +
      theme(legend.position="none", panel.grid.minor=element_blank()),
    ggplot(data.frame(group=group.names, duration=group.durations), aes(group, duration, fill=group)) +
      geom_bar(stat="identity") +
      xlab("groups") +
      coord_cartesian(ylim=c(0, 2500)) +
      theme(legend.position="none", panel.grid.minor=element_blank()),
    ncol=1)
}

#-------------------------------------------------

cost <- function (values, groups) {
  buckets <- tapply(values, groups, sum)
  list(buckets=buckets, range=range(buckets), sd=sd(buckets))
}

#-------------------------------------------------

report <- function (values, groups) {
  plot.groups(values, groups)
  cost(values, groups)
}

