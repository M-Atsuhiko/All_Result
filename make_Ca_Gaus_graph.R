make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}


means_plot <- function(Datas,dts,Colors,margins,
                       test,
                       reduced_test,
                       filename){
  max_Data <- max(sapply(Datas,function(data){
    return(max(data[,1] + data[,2]))}))

  min_Data <- min(sapply(Datas,function(data){
    return(min(data[,1] - data[,2]))}))

  plot(rbind(c(dts[1],min_Data),
             c(dts[length(dts)],max_Data)),
       type="n",
       xlab=expression(paste("Optimized ",Delta,"t [ms]")),
       ylab="Gausian mean")

  mapply(function(data,color,margin){
    lines(cbind(dts,data[,1]),
          col=color)
    arrows(dts + margin,data[,1],
           dts + margin,data[,1] + data[,2],
           angle=90,
           length=0.1,
           lwd=Error_bar_lwd,
           col=color,
           lty="longdash",
           )

    arrows(dts + margin,data[,1],
           dts + margin,data[,1] - data[,2],
           angle=90,
           length=0.1,
           lwd=Error_bar_lwd,
           col=color,
           lty="longdash",
           )
  },Datas,Colors,margins)

  
  mapply(function(result,dt,margin){
    if(result == FALSE){
      text(dt + margin,max_Data,
           label="*",
           cex=2)
    }
  },test,dts,rep(0.5,length(dts)))

  mapply(function(result,dt,margin){
    if(result == FALSE){
      text(dt + margin,max_Data,
           label="+",
           cex=2)
    }
  },reduced_test,dts,rep(-0.5,length(dts)))
  
  dev.copy2eps(file=filename)
  cat("output -> ",filename,"\n")
}

return_mean_means <- function(peak,mean,threshold){

  Mean <- c()
  Sd <- c()
  
  for(i in 1:ncol(peak)){
    means <- c()
    for(j in 1:nrow(peak)){
      if(peak[j,i] > threshold){
        means <- c(means,mean[j,i])
      }
    }
    
    if(length(means) == 0){
      Mean <- c(Mean,-1)
      Sd <- c(Sd,0)
    }else{
      Mean <- c(Mean,mean(means))
      Sd <- c(Sd,sd(means))
    }
  }
  return(cbind(Mean,Sd))
}

Lower_do_test <- function(Data1,Data2,dt,threshold){
  Lower_peak1 <- make_matrix(Data1,dt,"Lower_Gaus_Ca_peak")
  Lower_mean1 <- make_matrix(Data1,dt,"Lower_Gaus_Ca_mean")

  Lower_peak2 <- make_matrix(Data2,dt,"Lower_Gaus_Ca_peak")
  Lower_mean2 <- make_matrix(Data2,dt,"Lower_Gaus_Ca_mean")

  means1 <- c()
  means2 <- c()
  
  for(i in 1:nrow(Lower_peak1)){
    if(Lower_peak1[i] > threshold){
      means1 <- c(means1,Lower_mean1[i])
    }
    if(Lower_peak2[i] > threshold){
      means2 <- c(means2,Lower_mean2[i])
    }
  }
  
  if(length(means1) <= 1 || length(means2) <= 1){
    print("the data N is very small!")
    print(means1)
    print(means2)
    return(2)
  }else{
    return(t_test(means1,means2,"two.sided",0.05))
  }
}

Upper_do_test <- function(Data1,Data2,dt,threshold){
  Upper_peak1 <- make_matrix(Data1,dt,"Upper_Gaus_Ca_peak")
  Upper_mean1 <- make_matrix(Data1,dt,"Upper_Gaus_Ca_mean")

  Upper_peak2 <- make_matrix(Data2,dt,"Upper_Gaus_Ca_peak")
  Upper_mean2 <- make_matrix(Data2,dt,"Upper_Gaus_Ca_mean")

  means1 <- c()
  means2 <- c()
  
  for(i in 1:nrow(Upper_peak1)){
    if(Upper_peak1[i] > threshold){
      means1 <- c(means1,Upper_mean1[i])
    }
    if(Upper_peak2[i] > threshold){
      means2 <- c(means2,Upper_mean2[i])
    }
  }
  
  if(length(means1) <= 1 || length(means2) <= 1){
    print("the data N is very small!")
    print(means1)
    print(means2)
    return(2)
  }else{
    return(t_test(means1,means2,"two.sided",0.05))
  }
}
  
source("t_test.R")
source("make_legends.R")
source("graph_setting.R")
library(colorspace)

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

prefix <- "ca_Gaus_"

load(paste(Gausian_prefix,"ca_Rerative_Gaus_st50_75_0_All_Data_FRAME.xdr",sep=""))
ca_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"ca_Rerative_Gaus_st50_75_5_All_Data_FRAME.xdr",sep=""))
ca_reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"k_ca_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
k_ca_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"k_ca_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
k_ca_reduced_Gausian_Data <- ALL_DATA_FRAME

Ca_Max <- 0.0022
threshold <- Ca_Max*0.05

data_list <- list(ca_Gausian_Data,ca_reduced_Gausian_Data,
                  k_ca_Gausian_Data,k_ca_reduced_Gausian_Data)
legends <- c("CaT","CaT-reduced",
             "Ka CaT","Ka CaT-reduced")
Colors <- c("blueviolet","brown1",
            "forestgreen","greenyellow")
margins <- c(-0.1,-0.5,0.5,0.1)

Upper_result <- lapply(data_list,function(data){
  peak <- make_matrix(data,DELTA_T,"Upper_Gaus_Ca_peak")
  mean <- make_matrix(data,DELTA_T,"Upper_Gaus_Ca_mean")
  return(return_mean_means(peak,mean,threshold))
})

Lower_result <- lapply(data_list,function(data){
  peak <- make_matrix(data,DELTA_T,"Lower_Gaus_Ca_peak")
  mean <- make_matrix(data,DELTA_T,"Lower_Gaus_Ca_mean")
  return(return_mean_means(peak,mean,threshold))
})


ca_Lower_test_result <- sapply(DELTA_T,function(dt){
  return(Lower_do_test(ca_Gausian_Data,k_ca_Gausian_Data,dt,threshold))
})

ca_Upper_test_result <- sapply(DELTA_T,function(dt){
  return(Upper_do_test(ca_Gausian_Data,k_ca_Gausian_Data,dt,threshold))
})

ca_reduced_Lower_test_result <- sapply(DELTA_T,function(dt){
  return(Lower_do_test(ca_reduced_Gausian_Data,k_ca_reduced_Gausian_Data,dt,threshold))
})

ca_reduced_Upper_test_result <- sapply(DELTA_T,function(dt){
  return(Upper_do_test(ca_reduced_Gausian_Data,k_ca_reduced_Gausian_Data,dt,threshold))
})

filename <- paste(OutputDir,prefix,"Upper_mean.eps",sep="")
means_plot(Upper_result,DELTA_T,Colors,margins,
           ca_Upper_test_result,ca_reduced_Upper_test_result,filename)
filename <- paste(OutputDir,prefix,"Lower_mean.eps",sep="")
means_plot(Lower_result,DELTA_T,Colors,margins,
           ca_Lower_test_result,ca_reduced_Lower_test_result,filename)

filename <- paste(OutputDir,prefix,"mean_legend.eps",sep="")
make_legends(legends,Colors,rep("solid",4),rep("",4))
dev.copy2eps(file=filename)
cat("output -> ",filename,"\n")
