#' @title LPjmL output preparation
#'
#' @name lpjml_output
#'
#' @description Function `lpjml_output` read binary with the output of LPJmL runs and processes them into `dataframes`.
#' @details This function must work with a folder organiyation of the output files of LPJmL
#'
#' @param datasets_info object list containing information about the files that will be used that are going to be extracted.
#' @param folder Folder path for the input files
#' for more information plese check \link{dataset_info}.
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import lpjclass
#' @export lpjml_output

lpjml_output = function(folder,  datasets_info){
  
  wyears = datasets_info["wyears"][[1]]
  cells = datasets_info["cells"][[1]]
  file_name = datasets_info["file_name"][[1]]
  crop = datasets_info["crop"][[1]]
  water = datasets_info["water"][[1]]
  years = datasets_info["years"][[1]]
  bands = datasets_info["bands"][[1]]
  soilcells = datasets_info["soilcells"][[1]]
  monthly = datasets_info["monthly"][[1]]
  syear = datasets_info["syear"][[1]]
  
  dirs = list.dirs(path = folder)[-1]
  
  d.array = array(NaN,dim = c(cells,length(dirs),length(wyears)), dimnames = list(1:cells,basename(dirs),wyears))
  for (lsu in 1:length(dirs)) {
    tmp <- lpjclass::readLPJ(paste0("/",file_name),file_folder=dirs[lsu], wyears=wyears, syear=syear, monthly = monthly,
                             years=years, bands=bands, ncells = cells, soilcells= soilcells)
    d.array[,lsu,] = tmp[,,crop,water]@.Data
  }
  return(d.array)
}

