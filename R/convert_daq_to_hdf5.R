#' Convert a DAQ file to HDF4 using undaqTools package in Python
#'
#' @param file_name path to the daq file (string)
#'
#' @return saves the converted hdf5 file to disk with the same name as the original daq file
#' @export
#'
#' @examples
#' \dontrun{
#' library(reticulate)
#' use_virtualenv("r-reticulate")
#' read_daq_and_save_to_hdf5("path/to/daq/file/myfile.daq")
#' }
read_daq_and_save_to_hdf5 <- function(file_name){

  file_path_with_hdf5 <- paste0(strsplit(file_name, "[.]")[[1]][1], ".hdf5")

  if (file.exists(file_path_with_hdf5)){

    print(paste("The hdf5 file", basename(file_path_with_hdf5), "already exists!"))
  } else {

    daq <- undaqTools$Daq()

    daq$write_hd5(daq$read(file_name))

    print(paste("The hdf5 file", basename(file_path_with_hdf5), "successfully converted!"))
  }

}
