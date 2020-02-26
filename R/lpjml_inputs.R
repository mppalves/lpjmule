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
#' @export lpjml_inputs


lpjml_inputs <- function(folder, plotting = F, dataset_info) {
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
    soil <- array(NaN, dim = c(cells, 1, length(wyears)), dimnames = list(1:cells[[1]], "soil", wyears))
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

  res <- list(temp, prec, wet, lwnet, rsds, co2, soil)

  if (plotting) {
    
    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, temp[, , i])), main = paste("Temperature", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, prec[, , i])), main = paste("Preciptation", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, wet[, , i])), main = paste("Wet days", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, lwnet[, , i])), main = paste("Long wave radiation", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, rsds[, , i])), main = paste("Short wave radiation", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, co2[, , i])), main = paste("Co2 concentration", i))
    }

    for (i in factor(wyears)) {
      plot(raster::rasterFromXYZ(cbind(grid, soil[, , i])), main = paste("Soil type", i))
    }
  }
  return(res)
}
