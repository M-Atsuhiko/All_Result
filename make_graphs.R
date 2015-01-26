make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_one_graph.R")
source("barplot_datas.R")
source("t_test.R")
source("make_legends.R")
library(colorspace)

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

vol_col <- expression(paste("[",mu,m^3,"]"))
length_col <- expression(paste("[",mu,"m]"))
N_col <- "[number]"
cond_amount_col <- expression(paste("[pS/c",m^2,"]",sep=""))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

typeName <- "passive"
prefix <- "Tsuishi_Rerative_"

load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa_05_75_0_All_Data_FRAME.xdr",sep=""))
alfa_data<- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
rerative_data <- ALL_DATA_FRAME

dataList <- list(alfa_data,rerative_data)

N_data <- length(dataList)
Colors <- rainbow_hcl(N_data)
SolidType <- rep("solid",N_data)

legends <- c("Torben et al",
             "Rerative")

dataNames <- c("Upper_Diam","Lower_Diam")

mainNames <-c("Upper Dendrite diameter","Lower Dendrite diameter")

colNames <- c(expression(paste("Upper Stem diameter [",mu,"m]",sep="")),
              expression(paste("Lower Stem diameter [",mu,"m]",sep="")))

rowNames <- rep(dt_row,length(colNames))

par(lwd=3,
    cex=1.4,
    mex=1.2)


mapply(function(data_name,mainName,rowname,colname){
  cat(mainName,"\n")
  Filename <- paste(OutputDir,prefix,data_name,".eps",sep="")

  data_list <- lapply(dataList,function(dataframe){
    return(make_matrix(dataframe,DELTA_T,data_name))})
  if(length(grep("amount",mainName))){
    data_list <- lapply(data_list,function(data_mat){
      return(data_mat*10^9)
    })
  }

  test_result <- rbind(DELTA_T,
                      sapply(DELTA_T,function(dt){
                        t_test(subset(alfa_data,DT == dt)[[data_name]],
                               subset(rerative_data,DT == dt)[[data_name]],
                               0.05)
                      }))
  
  plot_one_graph(data_list,
                 mainName,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 SolidType,
                 DELTA_T,
                 FALSE,
                 test_result
                 )
  dev.copy2eps(file=Filename)
  cat("Output ->",Filename,"\n")
  cat("\n")
},dataNames,mainNames,rowNames,colNames)

cat("the legend \n")
Filename <- paste(OutputDir,prefix,"legend",".eps",sep="")
make_legends(legends,Colors,SolidType)
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
