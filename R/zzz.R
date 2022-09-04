# global reference to packages (will be initialized in .onLoad)
scipy <- NULL
numpy <- NULL
h5py <- NULL
undaqTools <- NULL



.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  h5py <<- reticulate::import("h5py", delay_load = TRUE)
  undaqTools <<- reticulate::import("undaqTools", delay_load = TRUE)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to DAQ R!\nUnofficial R interface to National Advanced Driving Simulator (NADS) Data AcQuisition (DAQ) files (based on the undaqTools by Roger Lew)")
}
