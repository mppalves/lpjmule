#' @title Machine learning training dataset
#' 
#' @name df_training
#' 
#' @description Function `df.training` combine together the output of the `lpjml_output` and `Lpjml_input` functions into a `features:label` format.
#' @param lpjml_inputs input from function `Lpjml_input`
#' @param lpjml_output output from function `lpjml_output`
#' @param input_levels The breakdown of the LSU leves in the data ser as a vector.
#' @param cells Number of grid cells (67420)
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @export df_training


df_training = function(lpjml_inputs, lpjml_output, input_levels= c(seq(0,2.0,0.2),2.5), cells = 67420) {
  df = cbind(lpjml_output, lpjml_inputs[[1]],lpjml_inputs[[2]],lpjml_inputs[[3]],lpjml_inputs[[4]],lpjml_inputs[[5]],lpjml_inputs[[6]], rep(c(seq(0,2,0.2),2.5), times = 1, each=cells))
  return(df)
}
