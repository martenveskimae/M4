## Märten 19. March 2018

library(tidyverse)
library(data.table)
library(forecast)

files = list.files("full/train/")
props = read_csv("full/M4-info.csv")

noise = function(residuals,type){
  if(type=="mse"){
    return(mean(sum(residuals^2)))
  }
}

ts_residiual = function(ts_data,metric="mse"){
  name = ts_data[1]
  ts_props = props[props$M4id==name,]
  ts_data = as.numeric(na.omit(ts_data[-1]))
  ts_data = scale(ts_data)
  ts_data = ts(ts_data,frequency=ts_props$Frequency)
  if(length(is.numeric(ts_data))==0) return(NA_real_)
  if(ts_props$Frequency==1){
    window = min(max(as.integer(length(ts_data)/30),5),30)
    ma_model = ma(ts_data,order=window)
    residuals = ts_data[!(is.na(ma_model))] - ma_model[!(is.na(ma_model))]
  } else {
    residuals = tryCatch({na.omit(decompose(ts_data,type="additive")$random)}, #type="multiplicative"
                         error=function(e) return(NA))
  }
  if(length(residuals[!(is.na(residuals))])==0) return(NA_real_)
  else return(noise(residuals,type=metric))
}

noise_levels = sapply(files,function(d){
  print(d)
  df = t(fread(paste0("full/train/",d),verbose=F))
  r = apply(df,2,ts_residiual)
  names(r) = df[1,]
  return(r)
})

results = data.frame(ts_name=gsub(".*\\.","",names(unlist(noise_levels))),
                     noise=unlist(noise_levels))
rownames(results) = 1:nrow(results)

write.csv(results,"noise_levels.csv",row.names=F)
