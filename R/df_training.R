#' @title Machine learning training dataset
#'
#' @name df_training
#'
#' @description Function `df.training` combine together the output of the `lpjml_output` and `Lpjml_input` functions into a `features:label` format.
#'
#' @param lpjml_inputs Output from function `Lpjml_input`
#' @param lpjml_output Output from function `lpjml_output`
#' @param dataset_info Object list containing information about the files that will be used that are going to be extracted.
#' @param input_levels The breakdown of the LSU leves in the data ser as a vector.
#'
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @export df_training


df_training <- function(lpjml_inputs, lpjml_output, input_levels = c(seq(0, 2.0, 0.2), 2.5), dataset_info) {
  cells <- dataset_info["cells"][[1]]
  df <- cbind(lpjml_output,rep(input_levels, times = 1, each = cells), lpjml_inputs[[1]], lpjml_inputs[[2]], lpjml_inputs[[3]], lpjml_inputs[[4]], lpjml_inputs[[5]], lpjml_inputs[[6]], lpjml_inputs[[7]] )
<<<<<<< HEAD
  colnames(df) <- c("lpjml_output", "lsu","temp", "prec", "wet", "lwnet", "rsds", "co2", "soil")
=======
  colnames(df) <- c("lpjml_output", "temp", "prec", "wet", "lwnet", "rsds", "co2", "soil", "lsu")
>>>>>>> b8e56fa91b1a6894971eae14817de237d83cdd44
  
  soilpar <- data.frame(
    "soil"         = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
    "Ks"           = c(3.5, 4.8, 26, 8.8, 7.3, 16, 12.2, 10.1, 18.8, 10.1, 50.7, 167.8, 0.1),
    "Sf"           = c(243, 169, 51, 139, 324, 72, 192, 409, 77, 409, 20, 39, 1),
    "w_pwp"        = c(0.284, 0.259, 0.205, 0.214, 0.247, 0.143, 0.139, 0.177, 0.1, 0.177, 0.06, 0.022, 0.0001),
    "w_fc"         = c(0.398, 0.378, 0.295, 0.345, 0.387, 0.256, 0.292, 0.368, 0.228, 0.368, 0.149, 0.088, 0.005),
    "w_sat"        = c(0.468, 0.468, 0.406, 0.465, 0.464, 0.404, 0.439, 0.476, 0.434, 0.476, 0.421, 0.339, 0.006),
    "hsg"          = c(1, 2, 4, 3, 2, 1, 4, 2, 2, 2, 2, 2, 4),
    "tdiff_0"      = c(0.572, 0.502, 0.785, 0.65, 0.556, 0.78, 0.701, 0.637, 0.64, 0.637, 0.403, 0.201, 4.137),
    "tdiff_15"     = c( 0.571, 0.503, 0.791, 0.656, 0.557, 0.808, 0.74, 0.657, 0.713, 0.657, 0.529, 0.196, 4.127),
    "tdiff_100"    = c(0.555, 0.491, 0.799, 0.653, 0.542, 0.867, 0.797, 0.661, 0.863, 0.661, 0.85, 0.896, 4.089),
    "cond_pwp"     = c( 1.388, 1.177, 1.72, 1.369, 1.27, 1.498, 1.276, 1.219, 1.053, 1.219, 0.601, 0.303, 8.768),
    "cond_100"     = c(1.719, 1.516, 2.347, 1.967, 1.675, 2.527, 2.34, 1.999, 2.53, 1.999, 2.706, 3.431, 8.657),
    "cond_100_ice" = c(3.233, 2.853, 4.060, 3.685, 3.134, 4.360, 4.233, 3.803, 4.547, 3.803, 4.778, 5.423, 8.727)
  )
  
  df = merge(df,soilpar, by = "soil")
  
  return(df)
}




