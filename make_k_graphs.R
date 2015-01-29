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
source("make_Conductance_graphs.R")
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


dataNames <- c("F",
               "TREE_length",
               "TREE_volume",
               "TREE_K_ratio","Upper_K_ratio","Lower_K_ratio",
               "Upper_Diam","Lower_Diam",
               "N_Upper_Syn","N_Lower_Syn")

mainNames <-c("F",
              "Neuron length",
              "Neuron volume",
              "Neuron Ka ratio","Upper Dendrite Ka ratio","Lower Dendrite Ka ratio",
              "Upper Dendrite diameter","Lower Dendrite diameter",
              "Number of Red Synapse","Number of Blue Synapse")

colNames <- c("F",
              expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]")),
              expression(paste("Neuron Ka ratio",sep="")),
              expression(paste("Upper Dendrite Ka ratio",sep="")),
              expression(paste("Lower Dendrite Ka ratio",sep="")),
              expression(paste("Upper Stem diameter [",mu,"m]",sep="")),
              expression(paste("Lower Stem diameter [",mu,"m]",sep="")),
              "Number of Red Synpase","Number of Blue Synpase")


load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME

load(paste(Gausian_prefix,"passive_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
passive_Data <- ALL_DATA_FRAME
#load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa_05_75_0_All_Data_FRAME.xdr",sep=""))
#Tsuishi_Data <- ALL_DATA_FRAME

make_Conductance_graphs(Gausian_Data,reduced_Gausian_Data,
                        Liner_Data,reduced_Liner_Data,
                        passive_Data,
                        prefix,
                        dt_row,
                        dataNames,
                        mainNames,
                        colNames)
