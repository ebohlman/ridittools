ridits.se <-
function(x, margin, ref=NULL) {
  apply(x, margin, se.ridit, ridits.refgroup(x,margin,ref))
}
