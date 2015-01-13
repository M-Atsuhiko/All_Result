barplot_datas <- function(Datas,
                          mainName,
                          rowname,
                          colname,
                          legends,
                          Colors,
                          lineType,
                          DELTA_T,
                          showMax,
                          OutputFilename){

  plot.new()
  par(xpd=TRUE)
  
  if(showMax){
    Max_data <- max(sapply(Datas,max))
  }else{
    Max_data <- max(sapply(Datas,function(Data){
      return(apply(Data,2,mean) + apply(Data,2,sd))}))
  }

  Min_data <- min(sapply(Datas,function(Data){
    return(apply(Data,2,mean))}))

  means <- t(sapply(Datas,function(Data){
    return(apply(Data,2,mean))
  }))

  sds <- t(sapply(Datas,function(Data){
    return(apply(Data,2,sd))
  }))

  Xs <- barplot(means,
                main=mainName,
                xlab=rowname,
                ylab=colname,
                legend.text=legends,
                args.legend=list(x="top"),
                beside=TRUE,
                col=Colors,
                ylim=c(0,Max_data))

  arrows(Xs,means,Xs,means + sds,angle=90,length=0.02)
  arrows(Xs,means,Xs,sapply(means - sds,max,0),angle=90,length=0.02)
  ## for(i in 1:nrow(means)){
  ##   lines(cbind(Xs[i,],means[i,]),
  ##         col=Colors[i],
  ##         lty="solid")

  dev.copy2eps(file=OutputFilename)
  cat("Output ->",OutputFilename,"\n")
}

