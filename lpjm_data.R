library(PIKTools)
library(LandMark)
library(readr)
library(raster)
library(lpjclass)

lpjml_output = function(folder,  wyears = seq(1951,2010,10), cells = 67420, file_name = "pft_harvest.pft.bin", crop ="mgrass", irrig = "rainfed", plotting = F){
  
  dirs = list.dirs(path = folder)[-1]
  
  d.array = array(NaN,dim = c(cells,length(dirs),length(wyears)), dimnames = list(1:cells,basename(dirs),wyears))
  for (lsu in 1:length(dirs)) {
    tmp <- lpjclass::readLPJ(paste0("/",file_name),file_folder=dirs[lsu], wyears=wyears,syear=1951, monthly = F,
                             years=149,bands=32, ncells = cells, soilcells=FALSE)
    d.array[,lsu,] = tmp[,,crop,irrig]@.Data
  }
  # dimnames(d.array)[[2]] = input_levels
  return(d.array)
}

lpjml_inputs = function(folder, cells = 67420, wyears = seq(1951,2010,5), temp="tas", prec ="pr", lwnet ="lwnet", rsds ="rsds", soil="soil", wet ="wet", co2 = "CO2"){

  temp_input = function(folder, cells, temp, wyears){
    files_list= list.files(folder)
    file = files_list[grep(temp,files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder,file),out_years = paste0("y",wyears),ncells=cells, namesum=T)
    tmp = tmp@.Data/12

    temperature = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"temperature",wyears))
    for (i in 1:length(wyears)) {
      temperature[,,i] = tmp[,i,1,1]
    }
    return(temperature)
  }
  
  prec_input = function(folder, cells, prec, wyears){
    files_list= list.files(folder)
    file = files_list[grep(prec,files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder,file),out_years = paste0("y",wyears),ncells=cells, namesum=T)
    tmp = tmp@.Data/12
    
    precipitation = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"precipitation",wyears))
    for (i in 1:length(wyears)) {
      precipitation[,,i] = tmp[,i,1,1]
    }
    return(precipitation)
  }
  
  wet_input = function(folder, cells, wet, wyears){
    files_list= list.files(folder)
    file = files_list[grep(wet,files_list)]
    tmp <- lpjclass::read.LPJ_input(file.path(folder,file),out_years = paste0("y",wyears),ncells=cells, namesum=T)
    tmp = tmp@.Data/12
    
    wetdays = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"wetdays",wyears))
    for (i in 1:length(wyears)) {
      wetdays[,,i] = tmp[,i,1,1]
    }
    return(wetdays)
  }
  
  lwnet_input = function(folder, cells, lwnet, wyears){
    files_list= list.files(folder)
    file = files_list[grep(lwnet,files_list)]
    
    temp <- lpjclass::read.LPJ_input(file.path(folder,file),out_years = paste0("y",wyears),ncells=cells, namesum=T)
    #divide by 365 to take the average
    temp <- matrix(temp/365, cells,length(wyears))
    
    lwnet = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"lwnet",wyears))
    for (i in 1:length(wyears)) {
      lwnet[,,i] = temp[,i]
    }
    return(lwnet)
  }

  
  rsds_input = function(folder, cells, rsds, wyears){
    files_list= list.files(folder)
    file = files_list[grep(rsds,files_list)]
    
    temp <- lpjclass::read.LPJ_input(file.path(folder,file),out_years = paste0("y",wyears),ncells=cells, namesum=T)
    #divide by 365 to take the average
    temp <- matrix(temp/365, cells,length(wyears))

    rsds = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"rsds",wyears))
    for (i in 1:length(wyears)) {
      rsds[,,i] = temp[,i]
    }
    return(rsds)
  }
  
  co2_input = function(folder, cells, co2, wyears){
    files_list= list.files(folder)
    file = files_list[grep(co2,files_list)]
    
    co2t = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"co2",wyears))
    temp <- read.table(file.path(folder,file))
    id <- match(wyears,temp[,1])
    temp <- temp[id,2]
    for (i in 1:length(wyears)) {
      co2t[,,i] <- temp[i]
    }
    return(co2t)
  }
  
  soil_input = function(folder, cells, soil, wyears) {
    
    files_list= list.files(folder)
    file = files_list[grep(soil,files_list)]
    
    file.path(folder,file)
    
    sk <- file(file.path(folder,file),"rb")
    temp <- readBin(sk,integer(),n=67420,size=1)
    close(sk)
    soil = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"soil",wyears))
    for (i in 1:length(wyears)) {
      soil[,,i] <- temp
    }
    return(soil)  
  }
  
  temp = temp_input(folder, cells, temp, wyears)
  prec = prec_input(folder, cells, prec, wyears)
  wet = wet_input(folder, cells, wet, wyears)
  lwnet = lwnet_input(folder, cells, lwnet, wyears)
  rsds = rsds_input(folder, cells, rsds, wyears)
  co2 = co2_input(folder, cells, co2, wyears)
  soil = soil_input(folder, cells, soil, wyears)
 
  res = list(temp,prec,wet,lwnet,rsds,co2,soil)

  if (plotting) {
    
  
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,temp[,,i])), main = paste("Temperature",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,prec[,,i])), main = paste("Preciptation",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,wet[,,i])), main = paste("Wet days",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,lwnet[,,i])), main = paste("Long wave radiation",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,rsds[,,i])), main = paste("Short wave radiation",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,co2[,,i])), main = paste("Co2 concentration",i))
  }
  
  for (i in wyears) {
    plot(rasterFromXYZ(cbind(grid,soil[,,i])), main = paste("Soil type",i))
  }
    
  }
  return(res)
}

df.training = function(lpjml_inputs, lpjml_output, input_levels= c(seq(0,2.0,0.2),2.5), cells) {
  df = cbind(lpjml_output, lpjml_inputs[[1]],lpjml_inputs[[2]],lpjml_inputs[[3]],lpjml_inputs[[4]],lpjml_inputs[[5]],lpjml_inputs[[6]], rep(c(seq(0,2,0.2),2.5), times = 1, each=cells))
  return(df)
}
