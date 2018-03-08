ridits <-
function(x, margin, ref=NULL) {
  apply(x, margin, meanridit, ridits.refgroup(x,margin,ref))
}
