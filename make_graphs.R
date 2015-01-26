make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_datas.R")
source("barplot_datas.R")
library(colorspace)


Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t"))

vol_col <- expression(paste("[",mu,m^3,"]"))
length_col <- expression(paste("[",mu,"m]"))
N_col <- "[number]"
cond_amount_col <- expression(paste("[pS/c",m^2,"]",sep=""))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

typeName <- "passive"

load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa_05_75_0_All_Data_FRAME.xdr",sep=""))
alfa_data<- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
rerative_data <- ALL_DATA_FRAME

dataList <- list(alfa_data,rerative_data)

N_data <- length(dataList)
Colors <- rainbow_hcl(N_data)
SolidType <- rep("solid",N_data)

OutputFilename <- "test.eps"
OutputFilename <- paste(OutputDir,OutputFilename,sep="")

dataNames <- c("F",
               "TREE_length",
               "TREE_volume")

mainNames <-c("F",
              "Tree length",
              "Tree volume")

colNames <- c("",
              length_col,
              vol_col)

rowNames <- rep(dt_row,length(colNames))

par(mfcol=c(2,1))


mapply(function(data_name,mainName,rowname,colname){
  cat(mainName,"\n")

  data_list <- lapply(dataList,function(dataframe){
    return(make_matrix(dataframe,DELTA_T,data_name))})
  if(length(grep("amount",mainName))){
    data_list <- lapply(data_list,function(data_mat){
      return(data_mat*10^9)
    })
  }
  plot_datas(data_list,
             mainName,
             rowname,
             colname,
             legends,
             Colors,
             SolidType,
             DELTA_T,
             FALSE,
             FALSE
             )

},dataNames,mainNames,rowNames,colNames)

