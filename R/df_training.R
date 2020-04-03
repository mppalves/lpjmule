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


df_training <-
  function(lpjml_inputs,
           lpjml_output,
           input_levels = c(seq(0, 2.0, 0.2), 2.5),
           dataset_info) {
    
    cells <- dataset_info["cells"][[1]]
    
    if(dim(lpjml_output)[2] != length(input_levels)){
      stop("Number of input_levels do not correspont to the number of columns in lpjml_outputs")
    }
    
    if(dim(lpjml_output)[3] != dim(lpjml_output)[3]){
      stop("lpjml_output and lpjm_input must have the same number of years in the 3rd array dimension")
    }
    
    res = cbind(lpjml_output, rep(input_levels, times = 1, each = cells))
    for (j in 1:dim(lpjml_inputs)[2]) {
      .df = NULL
      for (i in 1:dim(lpjml_inputs)[3]) {
        .df = append(.df, rep(lpjml_inputs[, j, i], times = dim(lpjml_output)[2], each = 1))
      }
      res = cbind(res, .df)
    }
    colnames(res) = c("lpjml_output","lsu", colnames(lpjml_inputs))
    res = data.frame(res)
    return(res)
  }




