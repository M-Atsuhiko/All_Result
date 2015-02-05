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
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

prefix <- "ca_liner_"

load(paste(Tsuishi_prefix,,"ca_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
ca_Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,,"ca_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
ca_reduced_Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_ca_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
k_ca_liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_ca_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
k_ca_reduced_liner_Data <- ALL_DATA_FRAME

data_list <- list(ca_liner_Data,ca_reduced_liner_Data,
                  k_ca_liner_Data,k_ca_reduced_liner_Data)

legends <- c("CaT","CaT-reduced",
             "Ka CaT","Ka CaT-reduced")
Colors <- c("blueviolet","brown1",
            "green","aquamarine")
margins <- c(-0.1,-0.5,0.5,0.1)

print(ca_Liner_Data)
