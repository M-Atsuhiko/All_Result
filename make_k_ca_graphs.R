make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_one_graph.R")
source("barplot_datas.R")
source("t_test.R")
source("graph_setting.R")
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

typeName <- "k_ca"
prefix <- "k_ca_test_"

dataNames <- c("F",
               "TREE_length",
               "TREE_volume",
               "TREE_K_ratio","Upper_K_ratio","Lower_K_ratio",
               "TREE_Ca_ratio","Upper_Ca_ratio","Lower_Ca_ratio",
               "Upper_Diam","Lower_Diam")

mainNames <-c("F",
              "Neuron length",
              "Neuron volume",
              "Neuron Ka ratio","Upper Dendrite Ka ratio","Lower Dendrite Ka ratio",
              "Neuron CaT ratio","Upper Dendrite CaT ratio","Lower Dendrite CaT ratio",
              "Upper Dendrite diameter","Lower Dendrite diameter")

colNames <- c("F",
              expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]")),
              expression(paste("Neuron Ka ratio",sep="")),
              expression(paste("Upper Dendrite Ka ratio",sep="")),
              expression(paste("Lower Dendrite Ka ratio",sep="")),
              expression(paste("Neuron CaT ratio",sep="")),
              expression(paste("Upper Dendrite CaT ratio",sep="")),
              expression(paste("Lower Dendrite CaT ratio",sep="")),
              expression(paste("Upper Stem diameter [",mu,"m]",sep="")),
              expression(paste("Lower Stem diameter [",mu,"m]",sep="")))


load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
Gausian_Data <- ALL_DATA_FRAME
#load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
#reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME

make_Conductance_graphs(Gausian_Data,reduced_Gausian_Data,
                        Liner_Data,reduced_Liner_Data,
                        prefix,
                        dt_row,
                        dataNames,
                        mainNames,
                        colNames)
