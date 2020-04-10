#' @title Machine learning training dataset
#'
#' @name training_warper
#'
#' @description Function `training_warper` calls functions `lpjml_output` and `Lpjml_input` and `df_training` as appropriate to create and save the training dataset.
#'
#' @param dataset_info Object list containing information about the files that will be used that are going to be extracted.
#' @param input_levels The breakdown of the LSU leves in the data ser as a vector.
#' @param folder Folder path for the input and output files
#'
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import tidyr
#' @import luscale
#' @import magclass
#' @export training_warper2


training_warper2 <- function(folder, dataset_info, .plotting = F) {
  file_name  <- dataset_info["file_name"][[1]]
  rel <- dataset_info["rel"][[1]]
  y = lpjml_inputs2("C:/Users/pedrosa/github/Models/LPJmL-emulator/inputs", dataset_info = dataset_info, plotting = .plotting)
  z = lpjml_output2("C:/Users/pedrosa/github/Models/LPJmL-emulator/inputs", dataset_info = dataset_info)
  x = mbind(y,z)
  x = speed_aggregate(x, rel = rel)
  x = as.data.frame(x, rev = 2)
  x = pivot_wider(x, values_from = ".value", names_from= "data")
  x = pivot_longer(x, cols = matches("^[0-9.]*$"))
  colnames(x)[c(dim(x)[2]-1,dim(x)[2])] <- c("lsu","output")
  x = transform(x, lsu = as.numeric(lsu))
  saveRDS(x,paste0(file_name,"_training.rds"))
  return(x)
}