# Convert vector of counts to ridits

to.ridit <- function(v) {
  (cumsum(v) - .5 * v) / sum(v)
}

# Calculate mean ridit for vector of counts relative to reference group

mean.ridit <- function(v, ref) {
  sum(to.ridit(ref) * v ) / sum(v)
}

# Calculate mean ridits for several groups
# x is matrix of counts
# margin is 1 for groups in rows, 2 for groups in columns
# If ref is omitted, totals across groups are used as reference group
# If ref is a vector of counts, it's used as reference group
# Otherwise, ref is the number (or name if it exists) of the group to use as reference

ridits <- function(x, margin, ref=NULL) {
  if (length(ref) > 1) {
    refgroup <- ref
  } else if (length(ref) == 1) {
    if (margin==1) {
      refgroup <- x[ref,]
    } else {
      refgroup <- x[, ref]
    }
  } else {
    refgroup <- apply(x, 3-margin, sum)
  }
  apply(x, margin, mean.ridit, refgroup)
}
