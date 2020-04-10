#' @title LPjmL input preparation
#' @name lpjml_inputs
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
#' @import raster
#' @import abind
#' @importFrom dplyr left_join
#' @import grDevices
#' @importFrom utils read.table
#' @export lpjml_inputs

load("R/grid.rda")

lpjml_inputs <- function(folder, plotting = T, dataset_info , .grid = grid) {
  cells <- dataset_info["cells"][[1]]
  wyears <- dataset_info["wyears"][[1]]
  temp <- dataset_info["temp"][[1]]
  prec <- dataset_info["prec"][[1]]
  lwnet <- dataset_info["lwnet"][[1]]
  rsds <- dataset_info["rsds"][[1]]
  soil <- dataset_info["soil"][[1]]
  wet <- dataset_info["wet"][[1]]
  co2 <- dataset_info["co2"][[1]]

  temp_input <- function(folder, cells, temp, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(temp, files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = cells, namesum = T)
    tmp <- tmp@.Data / 12

    temperature <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "temperature", wyears))
    for (i in 1:length(wyears)) {
      temperature[, , i] <- tmp[, i, 1, 1]
    }
    return(temperature)
  }

  prec_input <- function(folder, cells, prec, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(prec, files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = cells, namesum = T)
    tmp <- tmp@.Data / 12

    precipitation <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "precipitation", wyears))
    for (i in 1:length(wyears)) {
      precipitation[, , i] <- tmp[, i, 1, 1]
    }
    return(precipitation)
  }

  wet_input <- function(folder, cells, wet, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(wet, files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = cells, namesum = T)
    tmp <- tmp@.Data / 12

    wetdays <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "wetdays", wyears))
    for (i in 1:length(wyears)) {
      wetdays[, , i] <- tmp[, i, 1, 1]
    }
    return(wetdays)
  }

  lwnet_input <- function(folder, cells, lwnet, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(lwnet, files_list)]

    temp <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = cells, namesum = T)
    # divide by 365 to take the average
    temp <- matrix(temp / 365, cells, length(wyears))

    lwnet <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "lwnet", wyears))
    for (i in 1:length(wyears)) {
      lwnet[, , i] <- temp[, i]
    }
    return(lwnet)
  }


  rsds_input <- function(folder, cells, rsds, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(rsds, files_list)]

    temp <- lpjclass::read.LPJ_input(file.path(folder, file), out_years = paste0("y", wyears), ncells = cells, namesum = T)
    # divide by 365 to take the average
    temp <- matrix(temp / 365, cells, length(wyears))

    rsds <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "rsds", wyears))
    for (i in 1:length(wyears)) {
      rsds[, , i] <- temp[, i]
    }
    return(rsds)
  }

  co2_input <- function(folder, cells, co2, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(co2, files_list)]

    co2t <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "co2", wyears))
    temp <- read.table(file.path(folder, file))
    id <- match(wyears, temp[, 1])
    temp <- temp[id, 2]
    for (i in 1:length(wyears)) {
      co2t[, , i] <- temp[i]
    }
    return(co2t)
  }

  soil_input <- function(folder, cells, soil, wyears) {
    files_list <- list.files(folder)
    file <- files_list[grep(soil, files_list)]

    file.path(folder, file)

    sk <- file(file.path(folder, file), "rb")
    temp <- readBin(sk, integer(), n = 67420, size = 1)
    close(sk)
    
    df.temp = data.frame("soil" = temp)
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
    
    temp = left_join(df.temp, soilpar)
    temp = as.matrix(temp)
    soil <- array(NaN, dim = c(cells, dim(temp)[2], length(wyears)), dimnames = list(1:cells[[1]], colnames(soilpar), wyears))
    for (i in 1:length(wyears)) {
      soil[, , i] <- temp
    }
    return(soil)
  }

  temp <- temp_input(folder, cells, temp, wyears)
  prec <- prec_input(folder, cells, prec, wyears)
  wet <- wet_input(folder, cells, wet, wyears)
  lwnet <- lwnet_input(folder, cells, lwnet, wyears)
  rsds <- rsds_input(folder, cells, rsds, wyears)
  co2 <- co2_input(folder, cells, co2, wyears)
  soil <- soil_input(folder, cells, soil, wyears)

  # res <- list("temp" = temp, "prec" = prec, "wet" = wet, "lwnet" = lwnet, "rsds" = rsds, "co2" = co2, "soil" = soil)
  res <- abind(temp,prec,wet,lwnet,rsds,co2,soil, along = 2)
  
  if (plotting) {
    dir.create("./lpjml_input_plots")
    setwd("./lpjml_input_plots")
    for (i in factor(wyears)) {
      png(paste0("Temperature",i,".jpg"),width = 800, height = 600) 
      plot(rasterFromXYZ(cbind(.grid, temp[, , i])), main = paste("Temperature", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      png(paste0("Preciptation",i,".jpg"),width = 800, height = 600) 
      plot(rasterFromXYZ(cbind(.grid, prec[, , i])), main = paste("Preciptation", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      png(paste0("Wet days",i,".jpg"),width = 800, height = 600)
      plot(rasterFromXYZ(cbind(.grid, wet[, , i])), main = paste("Wet days", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      png(paste0("Long wave radiation",i,".jpg"),width = 800, height = 600) 
      plot(rasterFromXYZ(cbind(.grid, lwnet[, , i])), main = paste("Long wave radiation", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      png(paste0("Short wave radiation",i,".jpg"),width = 800, height = 600) 
      plot(rasterFromXYZ(cbind(.grid, rsds[, , i])), main = paste("Short wave radiation", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      png(paste0("Co2 concentration",i,".jpg"),width = 800, height = 600) 
      plot(rasterFromXYZ(cbind(.grid, co2[, , i])), main = paste("Co2 concentration", i))
      dev.off()
    }

    for (i in factor(wyears)) {
      for (j in colnames(soil)){
      png(paste0("Soil", j , i ,".jpg"),width = 800, height = 600)
      plot(rasterFromXYZ(cbind(.grid, soil[, j, i])), main = paste("Soil",j, i))
      dev.off()
      }
    }
    setwd("./..")
  }
  return(res)
}
