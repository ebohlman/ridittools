riditsse <-
function(x, margin, ref=NULL) {
  apply(x, margin, seridit, riditsrefgroup(x,margin,ref))
}
