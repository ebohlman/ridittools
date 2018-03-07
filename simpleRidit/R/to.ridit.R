to.ridit <-
function(v) {
  (cumsum(v) - .5 * v) / sum(v)
}
