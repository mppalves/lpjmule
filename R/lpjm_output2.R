#' @title LPjmL output preparation
#'
#' @name lpjml_output2
#'
#' @description Function `lpjml_output` read binary with the output of LPJmL runs and processes them into `dataframes`.
#' @details This function must work with a folder organiyation of the output files of LPJmL
#'
#' @param dataset_info object list containing information about the files that will be used that are going to be extracted.
#' @param folder Folder path for the input files
#' for more information plese check @Run \code{vignette("lpjmule", package = "lpjmule")} .
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import lpjclass
#' @import magclass
#' @export lpjml_output2

lpjml_output2 <- function(folder, dataset_info, rel) {
  wyears     <- dataset_info["wyears"][[1]]
  cells      <- dataset_info["cells"][[1]]
  file_name  <- dataset_info["file_name"][[1]]
  crop       <- dataset_info["crop"][[1]]
  water      <- dataset_info["water"][[1]]
  years      <- dataset_info["years"][[1]]
  bands      <- dataset_info["bands"][[1]]
  soilcells  <- dataset_info["soilcells"][[1]]
  monthly    <- dataset_info["monthly"][[1]]
  syear      <- dataset_info["syear"][[1]]
  soilcells  <- TRUE
  inlevels   <- dataset_info["input_levels"][[1]]
  
  dirs       <- list.dirs(path = folder)[-1]
  if (length(inlevels) == length(dirs)) {
    y <- list()
    for (lsu in 1:length(dirs)) {
      x <- lpjclass::readLPJ(
        paste0("/", file_name),
        file_folder = dirs[lsu],
        wyears = wyears,
        syear = syear,
        monthly = monthly,
        years = years,
        bands = bands,
        ncells = cells,
        soilcells = soilcells
      )
      x <- as.magpie(x[, , crop, water])
      dimnames(x)[3] <- inlevels[lsu]
      y[[lsu]] <- x
    }
    u = mbind(y)
  } else {
    stop("Number of LSU inputs do not match the number of lpjml runs")
  }
  return(u)
}
