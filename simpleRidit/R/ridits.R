ridits <-
function(x, margin, ref=NULL) {
  apply(x, margin, mean.ridit, ridits.refgroup(x,margin,ref))
}
