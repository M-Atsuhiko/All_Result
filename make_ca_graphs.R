make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_one_graph.R")
source("barplot_datas.R")
source("t_test.R")
source("wilcox_test.R")
source("f_test.R")
source("make_legends.R")
source("graph_setting.R")
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

typeName <- "ca"
prefix <- "ca_test_"


load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_Gaus75_5_All_Data_FRAME.xdr",sep=""))
reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME
#load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa05__All_Data_FRAME.xdr",sep=""))
#Tsuishi <- ALL_DATA_FRAME

dataList <- list(Gausian_Data,reduced_Gausian_Data,
                 Liner_Data,reduced_Liner_Data)
#                 Tsuishi)

N_data <- length(dataList)
Colors <- color_fun(N_data)
SolidType <- c(rep("solid",N_data),"dashed")

legends <- c("Gausian",
             "TREE_length",
             "Gausian-reduced",
             "Liner",
             "Liner-reduced")
#             "Torben et al.")

dataNames <- c("F",
               "TREE_length",
               "TREE_volume",
               "TREE_Ca_ratio","Upper_Ca_ratio","Lower_Ca_ratio",
               "Upper_Diam","Lower_Diam")

mainNames <-c("F",
              "Neuron length",
              "Neuron volume",
              "Neuron CaT ratio","Upper Dendrite CaT ratio","Lower Dendrite CaT ratio",
              "Upper Dendrite diameter","Lower Dendrite diameter")

colNames <- c("F",
              expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]")),
              expression(paste("Neuron CaT ratio",sep="")),
              expression(paste("Upper Dendrite CaT ratio",sep="")),
              expression(paste("Lower Dendrite CaT ratio",sep="")),
              expression(paste("Upper Stem diameter [",mu,"m]",sep="")),
              expression(paste("Lower Stem diameter [",mu,"m]",sep="")))

rowNames <- rep(dt_row,length(colNames))

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
                       })
                      ##   sapply(DELTA_T,function(dt){
                      ##   wilcox_test(subset(Gausian_Data,DT == dt)[[data_name]],
                      ##          subset(reduced_Gausian_Data,DT == dt)[[data_name]],
                      ##          0.05)
                      ## }),
                      ## sapply(DELTA_T,function(dt){
                      ##   f_test(subset(Gausian_Data,DT == dt)[[data_name]],
                      ##          subset(reduced_Gausian_Data,DT == dt)[[data_name]],
                      ##          0.05)
                      ##   })
                        )
  
  Liner_test <- rbind(DELTA_T - 0.5,
                      sapply(DELTA_T,function(dt){
                         t_test(subset(Liner_Data,DT == dt)[[data_name]],
                                subset(reduced_Liner_Data,DT == dt)[[data_name]],
                                0.05)
                       })
                      ##   sapply(DELTA_T,function(dt){
                      ##   wilcox_test(subset(Liner_Data,DT == dt)[[data_name]],
                      ##          subset(reduced_Liner_Data,DT == dt)[[data_name]],
                      ##          0.05)
                      ## }),
                      ## sapply(DELTA_T,function(dt){
                      ##   f_test(subset(Liner_Data,DT == dt)[[data_name]],
                      ##          subset(reduced_Liner_Data,DT == dt)[[data_name]],
                      ##          0.05)
                      ##   })
                      )
  
  plot_one_graph(data_list,
                 mainName,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 SolidType,
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
make_legends(legends,Colors,SolidType)
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
