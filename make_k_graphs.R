make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_one_graph.R")
source("barplot_datas.R")
source("graph_setting.R")
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

typeName <- "k"
prefix <- "k_test_"


load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME
#load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa_05_75_0_All_Data_FRAME.xdr",sep=""))
#Tsuishi_Data <- ALL_DATA_FRAME

dataList <- list(Gausian_Data,reduced_Gausian_Data,
                 Liner_Data,reduced_Liner_Data)
#                 Tsuishi_Data)

N_data <- length(dataList)
Colors <- color_fun(N_data)
LineType <- rep("solid",N_data)
#LineType <- c(rep("solid",N_data - 1),"dashed")

legends <- c("Gausian",
             "Gausian-reduced",
             "Liner",
             "Liner-reduced")
#             "Torben et al.")


dataNames <- c("F",
               "TREE_length",
               "TREE_volume",
               "TREE_K_ratio","Upper_K_ratio","Lower_K_ratio")

mainNames <-c("F",
              "Neuron length",
              "Neuron volume",
              "Neuron Ka ratio","Upper Dendrite Ka ratio","Lower Dendrite Ka ratio")

colNames <- c("F",
              expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]")),
              expression(paste("Neuron Ka ratio",sep="")),
              expression(paste("Upper Dendrite Ka ratio",sep="")),
              expression(paste("Lower Dendrite Ka ratio",sep="")))

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

  Gausian_test <- rbind(DELTA_T + 0.5,
                        sapply(DELTA_T,function(dt){
                          t_test(subset(Gausian_Data,DT == dt)[[data_name]],
                                 subset(reduced_Gausian_Data,DT == dt)[[data_name]],
                                 0.05)
                        }))
  
  Liner_test <- rbind(DELTA_T - 0.5,
                        sapply(DELTA_T,function(dt){
                          t_test(subset(Liner_Data,DT == dt)[[data_name]],
                                 subset(reduced_Liner_Data,DT == dt)[[data_name]],
                                 0.05)
                        }))

  print(Gausian_test)
  print(Liner_test)
  
  plot_one_graph(data_list,
                 mainName,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 LineType,
                 DELTA_T,
                 FALSE,
                 list(Gausian_test,Liner_test)
                 )
  
  dev.copy2eps(file=Filename)
  cat("Output ->",Filename,"\n")
  cat("\n")
},dataNames,mainNames,rowNames,colNames)

cat("the legend \n")
Filename <- paste(OutputDir,prefix,"legend",".eps",sep="")
make_legends(legends,Colors,LineType)
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
