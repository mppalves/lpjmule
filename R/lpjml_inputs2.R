#' @title LPjmL input preparation
#' @name lpjml_inputs2
#'
#' @description Function `lpjml_output` read binary with the output of LPJmL runs and processes them into `dataframes`.
#' @details This function must work with a folder organiyation of the output files of LPJmL
#'
#' @param folder Folder path for the input files
#' @param dataset_info object list containing information about the files that will be used that are going to be extracted.
#' @param plotting \code{True} or \code{False} determing if plots from the data extracted will be plotted for checking.
#'
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import lpjclass
#' @import magclass
#' @import raster
#' @import abind
#' @importFrom dplyr left_join
#' @import grDevices
#' @importFrom utils read.table
#' @export lpjml_inputs2



lpjml_inputs2 <- function(folder, plotting = T, dataset_info) {
  data("sysdata",package="lpjmule")
  wyears <- dataset_info["wyears"][[1]]
  temp <- dataset_info["temp"][[1]]
  prec <- dataset_info["prec"][[1]]
  lwnet <- dataset_info["lwnet"][[1]]
  rsds <- dataset_info["rsds"][[1]]
  soil <- dataset_info["soil"][[1]]
  wet <- dataset_info["wet"][[1]]
  co2 <- dataset_info["co2"][[1]]

  temp_input <- function(folder, temp, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(temp, files_list)]
    x <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = 59199, namesum = T)
    x <- x / 12
    x <- as.magpie(x)
    dimnames(x)[3] <- "temperature"
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }

  prec_input <- function(folder, prec, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(prec, files_list)]
    x <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = 59199, namesum = T)
    x <- x / 12
    x <- as.magpie(x)
    dimnames(x)[3] <- "precipitation"
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }

  wet_input <- function(folder, wet, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(wet, files_list)]
    x <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = 59199, namesum = T)
    x <- x / 12
    x <- as.magpie(x)
    dimnames(x)[3] <-"wetdays"
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }

  lwnet_input <- function(folder, lwnet, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(lwnet, files_list)]
    x <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = 59199, namesum = T)
    x <- x/365
    x <- as.magpie(x)
    dimnames(x)[3] <-"lwnet"
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }


  rsds_input <- function(folder, rsds, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(rsds, files_list)]
    x <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = 59199, namesum = T)
    x <- x/365
    x <- as.magpie(x)
    dimnames(x)[3] <-"rsds"
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }

  co2_input <- function(folder, co2, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(co2, files_list)]

    co2t <- array(NaN, dim = c(59199, length(wyears),1), dimnames = list(1:59199, wyears, "co2"))
    temp <- read.table(file.path(folder, file))
    id <- match(wyears, temp[, 1])
    temp <- temp[id, 2]
    for (i in 1:length(wyears)) {
      co2t[, i, ] <- temp[i]
    }
    x = as.magpie(co2t, spatial = 1)
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    dimnames(x)[3] <-"co2"
    
    return(x)
  }

  soil_input <- function(folder, soil, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(soil, files_list)]

    file.path(folder, file)

    sk <- file(file.path(folder, file), "rb")
    y <- readBin(sk, integer(), n = 67460, size = 1)
    close(sk)
    y = y[which(magclassdata$grid_67420_59199 ==1)]
    y = y[magclassdata$cellbelongings$LPJ.Index]
    df.y = data.frame("soil" = y)
    soilpar <- data.frame(
      "soil"         = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
      "Ks"           = c(3.5, 4.8, 26, 8.8, 7.3, 16, 12.2, 10.1, 18.8, 10.1, 50.7, 167.8, 0.1),
      "Sf"           = c(243, 169, 51, 139, 324, 72, 192, 409, 77, 409, 20, 39, 1),
      "w_pwp"        = c(0.284, 0.259, 0.205, 0.214, 0.247, 0.143, 0.139, 0.177, 0.1, 0.177, 0.06, 0.022, 0.0001),
      "w_fc"         = c(0.398, 0.378, 0.295, 0.345, 0.387, 0.256, 0.292, 0.368, 0.228, 0.368, 0.149, 0.088, 0.005),
      "w_sat"        = c(0.468, 0.468, 0.406, 0.465, 0.464, 0.404, 0.439, 0.476, 0.434, 0.476, 0.421, 0.339, 0.006),
      "hsg"          = c(1, 2, 4, 3, 2, 1, 4, 2, 2, 2, 2, 2, 4),
      "tdiff_0"      = c(0.572, 0.502, 0.785, 0.65, 0.556, 0.78, 0.701, 0.637, 0.64, 0.637, 0.403, 0.201, 4.137),
      "tdiff_15"     = c(0.571, 0.503, 0.791, 0.656, 0.557, 0.808, 0.74, 0.657, 0.713, 0.657, 0.529, 0.196, 4.127),
      "tdiff_100"    = c(0.555, 0.491, 0.799, 0.653, 0.542, 0.867, 0.797, 0.661, 0.863, 0.661, 0.85, 0.896, 4.089),
      "cond_pwp"     = c(1.388, 1.177, 1.72, 1.369, 1.27, 1.498, 1.276, 1.219, 1.053, 1.219, 0.601, 0.303, 8.768),
      "cond_100"     = c(1.719, 1.516, 2.347, 1.967, 1.675, 2.527, 2.34, 1.999, 2.53, 1.999, 2.706, 3.431, 8.657),
      "cond_100_ice" = c(3.233, 2.853, 4.060, 3.685, 3.134, 4.360, 4.233, 3.803, 4.547, 3.803, 4.778, 5.423, 8.727)
    )
    
    y = left_join(df.y, soilpar, by ="soil")
    y = as.matrix(y)
    soil <- array(NaN, dim = c(59199, dim(y)[2], length(wyears)), dimnames = list(1:59199, colnames(soilpar), wyears))
    for (i in 1:length(wyears)) {
      soil[, , i] <- y
    }
    x <- as.magpie(soil,spatial = 1)
    getCells(x) <- paste(magclassdata$cellbelongings$region.code,magclassdata$cellbelongings$MAgPIE.Index, sep =".")
    return(x)
  }

  temp <- temp_input(folder, temp, wyears)
  prec <- prec_input(folder, prec, wyears)
  wet <- wet_input(folder, wet, wyears)
  lwnet <- lwnet_input(folder, lwnet, wyears)
  rsds <- rsds_input(folder, rsds, wyears)
  co2 <- co2_input(folder, co2, wyears)
  soil <- soil_input(folder, soil, wyears)

  res <- mbind(list(temp,prec,wet,lwnet,rsds,co2,soil))
  
  if (plotting) {
    dir.create("./lpjml_input_plots")
    setwd("./lpjml_input_plots")
    for (variable in dimnames(res)[[3]]) {
      print(variable)
      for (year in dimnames(res)[[2]]) {
        print(year)
        luplot::plotmap2(res[, year, variable], file = paste0(variable, "_", year, ".jpg"))
      }
    }
    setwd("./..")
  }
  return(res)
}


