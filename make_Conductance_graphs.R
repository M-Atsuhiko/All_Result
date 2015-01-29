make_Conductance_graphs <- function(Gausian_Data,reduced_Gausian_Data,
                                    Liner_Data,reduced_Liner_Data,
                                    prefix,
                                    dt_row,
                                    dataNames,
                                    mainNames,
                                    colNames){
  dataList <- list(Gausian_Data,reduced_Gausian_Data,
                   Liner_Data,reduced_Liner_Data)
                                        #                 Tsuishi)

  N_data <- length(dataList)
  Colors <- color_fun(N_data)
  LineType <- rep("solid",N_data)
  PointType <- c(rep("",N_data),"*","*")

  legends <- c("Gausian",
               "Gausian-reduced",
               "Liner",
               "Liner-reduced",
               "t-test (Gausian)",
               "t-test (Liner)")

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
  make_legends(legends,c(Colors,"black","red"),c(LineType,rep("blank",2)),PointType)
  dev.copy2eps(file=Filename)
  cat("Output ->",Filename,"\n")
}
