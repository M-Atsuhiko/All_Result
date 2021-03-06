make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}
  
source("plot_one_graph.R")
source("barplot_datas.R")
source("t_test.R")
#source("wilcox_test.R")
#source("f_test.R")
source("make_legends.R")
source("graph_setting.R")
source("make_Conductance_graphs.R")
library(colorspace)

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

typeName <- "ca"
prefix <- "ca_st50_test_"

dataNames <- c("F",
               "TREE_length",
               "TREE_volume",
               "TREE_Ca_ratio","Upper_Ca_ratio","Lower_Ca_ratio",
               "Upper_Diam","Lower_Diam",
               "N_Upper_Syn","N_Lower_Syn",
               "N_Upper_bif","N_Lower_bif")

mainNames <-c("F",
              "Neuron length",
              "Neuron volume",
              "Neuron CaT ratio","Upper Dendrite CaT ratio","Lower Dendrite CaT ratio",
              "Upper Dendrite diameter","Lower Dendrite diameter",
              "Number of Red Synapse","Number of Blue Synapse",
              "Number of Upper Bifurcation","Number of Lower Bifurcation")

colNames <- c("F",
              expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]")),
              expression(paste("Neuron CaT ratio",sep="")),
              expression(paste("Upper Dendrite CaT ratio",sep="")),
              expression(paste("Lower Dendrite CaT ratio",sep="")),
              expression(paste("Upper Stem diameter [",mu,"m]",sep="")),
              expression(paste("Lower Stem diameter [",mu,"m]",sep="")),
              "Number of Red Synpase","Number of Blue Synpase",
              "Number of Upper Bifurcation","Number of Lower Bifurcation")

load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_st50_75_0_All_Data_FRAME.xdr",sep=""))
Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_st50_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_st50_75_0_All_Data_FRAME.xdr",sep=""))
Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_st50_75_5_All_Data_FRAME.xdr",sep=""))
reduced_Liner_Data <- ALL_DATA_FRAME

load(paste(Gausian_prefix,"passive_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
passive_Data <- ALL_DATA_FRAME
#load(paste(Tsuishi_prefix,typeName,"_Tsuishi_alfa05__All_Data_FRAME.xdr",sep=""))
#                                        Tsuishi <- ALL_DATA_FRAME
doTest <- TRUE

make_Conductance_graphs(Gausian_Data,reduced_Gausian_Data,
                        Liner_Data,reduced_Liner_Data,
                        passive_Data,
                        prefix,
                        dt_row,
                        dataNames,
                        mainNames,
                        colNames,
                        doTest)

