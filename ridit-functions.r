# Convert vector of counts to ridits

to.ridit <- function(v) {
  (cumsum(v) - .5 * v) / sum(v)
}

# Calculate mean ridit for vector of counts relative to reference group

mean.ridit <- function(v, ref) {
  sum(to.ridit(ref) * v ) / sum(v)
}

# Calculate standard error of mean ridit for vector of counts relative to reference group

se.ridit <- function(v, ref) {
  N <- sum(ref)
  n <- sum(v)
  term1 <- (n + 1) / N
  term2 <- 1 / (N * (N + n - 1))
  term3 <- sum((ref + v) ^ 3) / (N * (N + n) * (N + n - 1))
  (1 / (2 * sqrt(3 * n))) * sqrt(1 + term1 + term2 - term3)
}

# Utility to determine reference group

ridits.refgroup <- function(x, margin, ref=NULL) {
  if (length(ref) > 1) {
    refgroup <- ref
  } else if (length(ref) == 1) {
    if (margin==1) {system.time(
      refgroup <- x[ref,]
    } else {
      refgroup <- x[, ref]
    }
  } else {
    refgroup <- apply(x, 3-margin, sum)
  }
}

# Calculate mean ridits for several groups
# x is matrix of counts
# margin is 1 for groups in rows, 2 for groups in columns
# If ref is omitted, totals across groups are used as reference group
# If ref is a vector of counts, it's used as reference group
# Otherwise, ref is the number (or name if it exists) of the group to use as reference

ridits <- function(x, margin, ref=NULL) {
  apply(x, margin, mean.ridit, ridits.refgroup(x,margin,ref))
}

# Calculate standard errors of mean ridits for several groups
# Arguments are same as for ridits()

ridits.se <- function(x, margin, ref=NULL) {
  apply(x, margin, se.ridit, ridits.refgroup(x,margin,ref))
}
