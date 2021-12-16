h_get_general_model <- function() {
  .GeneralModel(
    datanames = "x",
    datamodel = function(x) {
      x
    },
    priormodel = function(x) {
      x
    },
    modelspecs = function(x) {
      x
    },
    init = function(x) {
      x
    },
    sample = "param1"
  )
}
