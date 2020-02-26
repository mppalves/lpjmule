#' Dataset information
#'
#'This list called \code{dataset_info} aims to be an oobject-like variable that stores general information used to read and process binary 
#'inputs and outputs from LPJmL. For consistency, it is advisible that this list be created only once and loaded in all functions that need
#'need it as an input (\code{\link{lpjml_inputs}}, \code{\link{lpjml_outputs}}, \code{\link{df_trainig}})
#'
#' @usage 
#' dataset_info = list(
#'  
#'      #file string parts names
#'      temp ="tas", 
#'      prec ="pr", 
#'      lwnet ="lwnet", 
#'      rsds ="rsds", 
#'      soil ="soil", 
#'      wet ="wet", 
#'      co2 = "CO2",
#'      
#'      #Shared variables
#'      wyears = seq(1951,2010,5), 
#'      cells = 67420, 
#'      
#'      #readLPJ specific variables more information on 
#'      \code{\link{readLPJ}}
#'      
#'      file_name = "pft_harvest.pft.bin",
#'      crop ="mgrass",
#'      water = "rainfed",
#'      syear = 1951,
#'      years = 149,
#'      bands = 32,
#'      soilcells = FALSE, #True (59199 cells), False (67420 cells)
#'      monthly = FALSE
#' )
#'
#' @param temp String that identify the temperature file
#' @param prec String that identify the temperature file
#' @param lwnet String that identify the temperature file
#' @param rsds String that identify the temperature file
#' @param soil String that identify the temperature file
#' @param wet String that identify the temperature file
#' @param co2 String that identify the temperature file
#' @param wyears vector of years that will be extracted from 
#' @param cells number os cells used in the simulation 
#' @param file_name output name of the file outcome of the Lpjml runs
#' @param crop crop name
#' @param water information about the water avaialability, either \code{irrigation} or \code{rainfed}
#' @param syear LPJml simulations starting year
#' @param years total number of simulated years (usually syear-2100)
#' @param bands The number of bands (e.g. crops, months). Will be 12 if monthly==TRUE
#' @param soilcells Bool. If 67420 cells are present in the file, should only the 59199 for MAgPIE be returned?
#' @param monthly \code{true} for monthly data and \code{false} for other intervals
#' 
#' @keywords datasets
#'

"dataset_info"