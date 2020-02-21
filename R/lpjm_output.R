#'@title LPjmL output preparation
#'
#'@name lpjml_output
#'
#'@description Function `lpjml_output` read binary with the output of LPJmL runs and processes them into `dataframes`.
#'@details This function must work with a folder organiyation of the output files of LPJmL
#'@param folder Folder path for the input files 
#'@param wyears Sequence of years to extracted
#'@param cells Number of grid cells (67420)
#'@param file_name Name of the binary files containing LPJmL results
#'@param crop Name of the crop to be extracted
#'@param water Source of water, eiter `rainfed` or `irrigated`. Currently only `rainfed` is available
#'@author Marcos Alves \email{mppalves@gmail.com}
#'@import lpjclass
#'@export lpjml_output

lpjml_output = function(folder,  wyears = seq(1951,2010,10), cells = 67420, file_name = "pft_harvest.pft.bin", crop ="mgrass", water = "rainfed"){
  
  dirs = list.dirs(path = folder)[-1]
  
  d.array = array(NaN,dim = c(cells,length(dirs),length(wyears)), dimnames = list(1:cells,basename(dirs),wyears))
  for (lsu in 1:length(dirs)) {
    tmp <- lpjclass::readLPJ(paste0("/",file_name),file_folder=dirs[lsu], wyears=wyears,syear=1951, monthly = F,
                             years=149,bands=32, ncells = cells, soilcells=FALSE)
    d.array[,lsu,] = tmp[,,crop,water]@.Data
  }
  return(d.array)
}

