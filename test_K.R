make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("t_test.R")
source("make_legends.R")
source("graph_setting.R")
library(colorspace)

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

prefix <- "k_Gaus_"

load(paste(Gausian_prefix,"k_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
k_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"k_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
k_reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME

 
