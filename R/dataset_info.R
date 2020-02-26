dataset_info = list(
  
  #file string parts names
  temp ="tas", 
  prec ="pr", 
  lwnet ="lwnet", 
  rsds ="rsds", 
  soil ="soil", 
  wet ="wet", 
  co2 = "CO2",
  
  #Shared variables
  wyears = seq(1951,2010,5), 
  cells = 67420, 
  
  #readLPJ specific variables more information on 
  #\code{\link{readLPJ}}
  
  file_name = "pft_harvest.pft.bin",
  crop ="mgrass",
  water = "rainfed",
  syear = 1951,
  years = 149,
  bands = 32,
  soilcells = FALSE, #True (59199 cells), False (67420 cells)
  monthly = FALSE
)

