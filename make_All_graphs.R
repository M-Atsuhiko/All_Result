make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}

make_All_graphs <- function(DELTA_T,
                            legends,
                            type){

  if(length(grep("k",type))) WITH_K <- TRUE
  else WITH_K <- FALSE
  if(length(grep("ca",type))) WITH_Ca <- TRUE
  else WITH_Ca <- FALSE

  typeName <- chartr(" ","_",type)
  
  source("plot_datas.R")
  source("barplot_datas.R")
  library(colorspace)

  par(lwd=3,
      cex=1.4,
      mex=1.2,
      oma=c(0,0,0,0))

  Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
  Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

                                        #DELTA_T <- seq(5,30,by=5)
  ## legends <- c("Gausian",
  ##              "dim-Gaus",
  ##              "Liner",
  ##              "dim-Liner")

  N_data_kind <- length(legends)

  Colors <- rainbow_hcl(N_data_kind)

  SolidType <- rep("solid",N_data_kind)

  dt_row<- expression(paste("Optimized ",Delta,"t"))

  vol_col <- expression(paste("[",mu,m^3,"]"))
  length_col <- expression(paste("[",mu,"m]"))
  N_col <- "[number]"
  cond_amount_col <- expression(paste("[pS/c",m^2,"]",sep=""))

  OutputDir <- "./Graphs/"

  dataNames <- c("F",
                 "TREE_volume",
                 "Upper_Dend_volume","Lower_Dend_volume",
                 "TREE_length",
                 "Upper_Dend_length","Lower_Dend_length",
                 "N_Upper_Syn","N_Lower_Syn",
                 "Upper_Diam","Lower_Diam")

  mainNames <-c("F",
                "Tree volume",
                "Upper Dendrite volume","Lower Dendrite volume",
                "Tree length",
                "Upper Dendrite length","Lower Dendrite length",
                "Number of Upper Synapse","Number of Lower Synapse",
                "Upper Dendrite diameter","Lower Dendrite diameter")
  

  colNames <- c("",
                vol_col,
                vol_col,vol_col,
                length_col,
                length_col,length_col,
                N_col,N_col,
                length_col,length_col)

  if(WITH_K){
    dataNames <- c(dataNames,
                   "TREE_K_amount","Upper_K_amount","Lower_K_amount",
                   "TREE_K_ratio","Upper_K_ratio","Lower_K_ratio")
    mainNames <- c(mainNames,
                   "Tree K amount","Upper Dendrite K amount","Lower Dendrite K amount",
                   "Tree K ratio","Upper Dendrite K ratio","Lower Dendrite K ratio")
    colNames <- c(colNames,
                  cond_amount_col,cond_amount_col,cond_amount_col,
                "","","")
  }

  if(WITH_Ca){
    dataNames <- c(dataNames,
                   "TREE_Ca_amount","Upper_Ca_amount","Lower_Ca_amount",
                   "TREE_Ca_ratio","Upper_Ca_ratio","Lower_Ca_ratio")
    mainNames <- c(mainNames,
                   "Tree Ca amount","Upper Dendrite Ca amount","Lower Dendrite Ca amount",
                   "Tree Ca ratio","Upper Dendrite Ca ratio","Lower Dendrite Ca ratio")
    colNames <- c(colNames,
                  cond_amount_col,cond_amount_col,cond_amount_col,
                "","","")
  }

  rowNames <- rep(dt_row,length(colNames))

  if(length(dataNames) != length(mainNames) ||
     length(dataNames) != length(rowNames) ||
     length(dataNames) != length(rowNames) ||
     length(dataNames) != length(colNames)) stop("Error: Data legends lenghes are not same!")

  load(paste(Gausian_prefix,typeName,"_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
  Gausian_Data <- ALL_DATA_FRAME
#  load(paste(Gausian_prefix,typeName,"_Rerative_Gaus75_5_All_Data_FRAME.xdr",sep=""))
#  reduced_Gausian_Data <- ALL_DATA_FRAME
  load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
  Liner_Data <- ALL_DATA_FRAME
  load(paste(Tsuishi_prefix,typeName,"_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
  reduced_Liner_Data <- ALL_DATA_FRAME

  print(make_matrix(reduced_Liner_Data,30,"F"))

  mapply(function(data_name,mainName,rowname,colname){
  
    cat(mainName,"\n")
    
    Filename <- paste(OutputDir,typeName,"_",data_name,".eps",sep="")

    data_list <- list(Gausian_mat <- make_matrix(Gausian_Data,DELTA_T,data_name),
 #                     reduced_Gausian_mat <- make_matrix(reduced_Gausian_Data,DELTA_T,data_name),
                      Liner_mat <- make_matrix(Liner_Data,DELTA_T,data_name),
                      reduced_Liner_mat <- make_matrix(reduced_Liner_Data,DELTA_T,data_name)
                      )
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
               Filename
               )
    cat("\n")
  },dataNames,mainNames,rowNames,colNames)
}

legends <- c("Gausian",
#             "dim-Gaus",
             "Liner",
             "dim-Liner")

type <- "k"

make_All_graphs(seq(5,30,by=5),
                legends,
                type)

  
