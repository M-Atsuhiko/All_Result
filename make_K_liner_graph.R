make_matrix <- function(data,dts,data_name){
  mat <- sapply(dts,function(dt){
    return(subset(data,DT == dt)[[data_name]])
  })
  return(mat)
}

plot_stem_conductance_taper <- function(conductance,tapers,Colors){

  
  x_range <- c(min(sapply(conductance,function(stem_conductance){
    return(min(stem_conductance))
  })),
               max(sapply(upper_stem_conductance,function(stem_conductance){
                 return(max(stem_conductance))
               })))

  y_range <- c(min(sapply(tapers,function(taper){
    return(min(taper))
  })),
               max(sapply(upper_tapers,function(taper){
                 return(max(taper))
               })))
  plot(cbind(x_range,y_range),
       type="n")

  mapply(function(cond,taper,color){
    for(i in 1:ncol(cond)){
      points(cbind(cond[,i],
                   taper[,i]),
             pch=16,
             col=color)
    }
  },conductance,tapers,Colors)
}

return_correlation <- function(data1,data2){
  return(sapply(1:ncol(data1),function(i){
    return(cor(data1[,i],data2[,i],method="pearson"))
  }))
}

source("t_test.R")
source("make_legends.R")
source("graph_setting.R")
library(colorspace)
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

dt_row<- expression(paste("Optimized ",Delta,"t [ms]"))

OutputDir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

prefix <- "k_liner_"

load(paste(Tsuishi_prefix,"k_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
k_Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
k_reduced_Liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_ca_Rerative_liner_75_0_All_Data_FRAME.xdr",sep=""))
k_ca_liner_Data <- ALL_DATA_FRAME
load(paste(Tsuishi_prefix,"k_ca_Rerative_liner_75_5_All_Data_FRAME.xdr",sep=""))
k_ca_reduced_liner_Data <- ALL_DATA_FRAME

data_list <- list(k_Liner_Data,k_reduced_Liner_Data,
                  k_ca_liner_Data,k_ca_reduced_liner_Data)

legends <- c("Ka","Ka-reduced",
             "Ka CaT","Ka CaT-reduced")
Colors <- c("darkorange4","darkorange",
            "forestgreen","greenyellow")

margins <- c(-0.1,-0.5,0.5,0.1)

upper_tapers <- lapply(data_list,function(data){
  return(make_matrix(data,DELTA_T,"Upper_Liner_K_taper"))
})

upper_stem_conductance <- lapply(data_list,function(data){
  return(make_matrix(data,DELTA_T,"Upper_Liner_K_Stem_conductance"))
})

lower_tapers <- lapply(data_list,function(data){
  return(make_matrix(data,DELTA_T,"Lower_Liner_K_taper"))
})

lower_stem_conductance <- lapply(data_list,function(data){
  return(make_matrix(data,DELTA_T,"Lower_Liner_K_Stem_conductance"))
})

test <- mapply(function(upper_data,lower_data){
  result <- return_correlation(upper_data,lower_data)
  return(result)
},upper_stem_conductance,lower_stem_conductance)

print(test)

## test <- mapply(function(conductance,taper){
##   return(return_correlation(conductance,taper))
## },upper_stem_conductance,upper_tapers)


#plot_stem_conductance_taper(lower_stem_conductance,lower_tapers,Colors)

## par(mar=c(3,3.2,1,2))
## min_data <- min(test)
## max_data <- max(test)

## plot(rbind(c(5,min_data),
##            c(30,max_data)),
##      type="n")

## for(i in 1:ncol(test)){
##   lines(cbind(DELTA_T,test[,i]),
##         col=Colors[i])
## }

## par(new=TRUE)
## min_data <- min(sapply(upper_stem_conductance,function(data){
##   return(min(data))}))
## max_data <- max(sapply(upper_stem_conductance,function(data){
##   return(max(data))}))

## plot(rbind(c(5,min_data),
##            c(30,max_data)),
##      type="n",
##      axes=FALSE)
## axis(4)

## mapply(function(data,color,margin){

##   for(i in 1:ncol(data)){
##     col <- data[,i]
##     mean_data <- mean(col)
##     sd_data <- sd(col)

##     dt <- DELTA_T[i]
    
##     points(cbind(rep(dt + margin,length(col)),col),
##            col=color,
##            cex=0.1)

##     points(dt + margin ,mean_data,
##            col=color,
##            pch=16)

##     arrows(dt + margin,mean_data,
##            dt + margin,mean_data - sd_data,
##            angle=90,length=0.1,
##            lwd=Error_bar_lwd,
##            lty="longdash",
##            col=color,
##            xpd=FALSE)
    
##     arrows(dt + margin,mean_data,
##            dt + margin,mean_data + sd_data,
##            angle=90,length=0.1,
##            lwd=Error_bar_lwd,
##            lty="longdash",
##            col=color,
##            xpd=FALSE)
##   }
## },upper_stem_conductance,Colors,margins)
