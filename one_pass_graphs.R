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
source("graph_setting.R")
library(colorspace)

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

typeName <- "passive"
prefix <- "passive_one_"

load(paste(Gausian_prefix,typeName,"_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
rerative_data <- ALL_DATA_FRAME

dataList <- list(rerative_data)

N_data <- length(dataList)
Colors <- "#00FFFFFF"

legends <- c("Upper Dendrite","Lower Dendrite")

LineType <- rep("solid",N_data)
point_type <- c(rep("",N_data),rep("*",2))

dataNames <- c("TREE_length",
               "TREE_volume",
               "Dendrite Diam",
               "N_Upper_Syn","N_Lower_Syn"
               )

mainNames <-c("Neuron length",
              "Neuron volume",
              "Dendrite Diam",
              "Number of Synapse"
              )


colNames <- c(expression(paste("Neuron length [",mu,"m]",sep="")),
              expression(paste("Neuron volume [",mu,m^3,"]",sep="")),
              expression(paste("Dendrite Stem Diameter [",mu,"m]",sep="")),
              "Number of Synapse")

rowNames <- rep(dt_row,length(colNames))

cat("Neuron length\n")

Filename <- paste(OutputDir,prefix,"TREE_length",".eps",sep="")

data_list <- lapply(dataList,function(dataframe){
  return(make_matrix(dataframe,DELTA_T,"TREE_length"))})

## test_result <- rbind(DELTA_T,
##                      sapply(DELTA_T,function(dt){
##                        t_test(subset(alfa_data,DT == dt)[[data_name]],
##                               subset(rerative_data,DT == dt)[[data_name]],
##                               0.05)
##                      }))
test_result <- c()
plot_one_graph(data_list,
               dataNames[1],
               rowNames[1],
               colNames[1],
               legends,
               Colors,
               LineType,
               DELTA_T,
               FALSE,
               test_result,
               test_result,
               test_result
               )
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
cat("\n")

cat("Neuron length\n")

Filename <- paste(OutputDir,prefix,"TREE_volume",".eps",sep="")

data_list <- lapply(dataList,function(dataframe){
  return(make_matrix(dataframe,DELTA_T,"TREE_volume"))})

## test_result <- rbind(DELTA_T,
##                      sapply(DELTA_T,function(dt){
##                        t_test(subset(alfa_data,DT == dt)[[data_name]],
##                               subset(rerative_data,DT == dt)[[data_name]],
##                               0.05)
##                      }))
test_result <- c()
plot_one_graph(data_list,
               dataNames[2],
               rowNames[2],
               colNames[2],
               legends,
               Colors,
               LineType,
               DELTA_T,
               FALSE,
               test_result,
               test_result,
               test_result
               )
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
cat("\n")

# ----------------------------------------

Dend_Colors <- c("red","blue")

cat("Dend Diam","n")
Filename <- paste(OutputDir,prefix,"Dends_Diam",".eps",sep="")

test_result <- rbind(DELTA_T,
                     sapply(DELTA_T,function(dt){
                       t_test(subset(rerative_data,DT == dt)[["Upper_Diam"]],
                              subset(rerative_data,DT == dt)[["Lower_Diam"]],
                              "two.sided",
                              0.05)
                     }))

star_black <- TRUE
  
plot_one_graph(list(make_matrix(rerative_data,DELTA_T,"Upper_Diam"),
                    make_matrix(rerative_data,DELTA_T,"Lower_Diam")),
                 mainNames[3],
                 rowNames[3],
                 colNames[3],
                 legends,
                 Dend_Colors,
                 LineType,
                 DELTA_T,
                 FALSE,
                 list(test_result),
                 c(),
                 c(),
               star_black
               )
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
cat("\n")

cat("N Dend Syn","n")
Filename <- paste(OutputDir,prefix,"N_Dends_Syn",".eps",sep="")

test_result <- rbind(DELTA_T,
                     sapply(DELTA_T,function(dt){
                       t_test(subset(rerative_data,DT == dt)[["N_Upper_Syn"]],
                              subset(rerative_data,DT == dt)[["N_Lower_Syn"]],
                              "two.sided",
                              0.05)
                     }))
  
plot_one_graph(list(make_matrix(rerative_data,DELTA_T,"N_Upper_Syn"),
                    make_matrix(rerative_data,DELTA_T,"N_Lower_Syn")),
               mainNames[4],
               rowNames[4],
               colNames[4],
               legends,
               Dend_Colors,
               LineType,
               DELTA_T,
               FALSE,
               list(test_result),
               c(),
               c(),
               star_black
               )
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
cat("\n")


cat("the legend \n")
Filename <- paste(OutputDir,prefix,"legend",".eps",sep="")
make_legends(legends,Dend_Colors,LineType,c("",""))
dev.copy2eps(file=Filename)
cat("Output ->",Filename,"\n")
