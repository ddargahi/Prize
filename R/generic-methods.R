
#gaggregate
setGeneric("AIJ", function(x) { standardGeneric("AIJ") })
setMethod("AIJ", signature(x="ANY"), function(x) {
  slot(x, "AIJ")
})

setGeneric("AIP", function(x) { standardGeneric("AIP") })
setMethod("AIP", signature(x="ANY"), function(x) {
  slot(x, "AIP")
})

setGeneric("GCR", function(x) { standardGeneric("GCR") })
setMethod("GCR", signature(x="ANY"), function(x) {
  slot(x, "GCR")
})

setGeneric("CI", function(x) { standardGeneric("CI") })
setMethod("CI", signature(x="ANY"), function(x) {
  slot(x, "CI")
})

setGeneric("ICR", function(x) { standardGeneric("ICR") })
setMethod("ICR", signature(x="ANY"), function(x) {
  slot(x, "ICR")
})

setGeneric("IP", function(x) { standardGeneric("IP") })
setMethod("IP", signature(x="ANY"), function(x) {
  slot(x, "IP")
})

#ahmatrix
setGeneric("ahp_matrix", function(x) { standardGeneric("ahp_matrix") })
setMethod("ahp_matrix", signature(x="ANY"), function(x) {
  slot(x, "ahp_matrix")
})

#ahp
setGeneric("weight", function(x) { standardGeneric("weight") })
setMethod("weight", signature(x="ANY"), function(x) {
  slot(x, "weight")
})

setGeneric("saaty_inconsistency", function(x) { standardGeneric("saaty_inconsistency") })
setMethod("saaty_inconsistency", signature(x="ANY"), function(x) {
  slot(x, "saaty_inconsistency")
})

#pipeline
setGeneric("ahp_plot", function(x) { standardGeneric("ahp_plot") })
setMethod("ahp_plot", signature(x="ANY"), function(x) {
  slot(x, "ahp_plot")
})

setGeneric("weight_plot", function(x) { standardGeneric("weight_plot") })
setMethod("weight_plot", signature(x="ANY"), function(x) {
  slot(x, "weight_plot")
})

setGeneric("rainbow_plot", function(x) { standardGeneric("rainbow_plot") })
setMethod("rainbow_plot", signature(x="ANY"), function(x) {
  slot(x, "rainbow_plot")
})

setGeneric("ahp_weights", function(x) { standardGeneric("ahp_weights") })
setMethod("ahp_weights", signature(x="ANY"), function(x) {
  slot(x, "ahp_weights")
})

setGeneric("simulation", function(x) { standardGeneric("simulation") })
setMethod("simulation", signature(x="ANY"), function(x) {
  slot(x, "simulation")
})

#rating
setGeneric("RM", function(x) { standardGeneric("RM") })
setMethod("RM", signature(x="ANY"), function(x) {
  slot(x, "RM")
})
