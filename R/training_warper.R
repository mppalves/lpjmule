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
#' @export training_warper


training_warper <- function(folder, input_levels = c(seq(0, 2.0, 0.2), 2.5), dataset_info) {
 
  file_name  <- dataset_info["file_name"][[1]]
  
  inputs = lpjml_inputs(folder = folder,plotting = T, dataset_info=dataset_info)
  outputs = lpjml_output(folder = folder, dataset_info = dataset_info)
  training = df_training(inputs, outputs, dataset_info = dataset_info)
  saveRDS(training,paste0(file_name,"_training.rds"))
  print(paste0("Training file generated was saved in folder: ",getwd()))
  return(training)
}