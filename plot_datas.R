plot_datas <- function(Datas,
                       mainName,
                       rowname,
                       colname,
                       legends,
                       Colors,
                       lineType,
                       DELTA_T,
                       showMax,
                       showLegend){

#  plot.new()
  if(showLegend){
    old_par <- par(xpd=NA,
                   oma=c(0,0,0,5))
  }

  sigma_margin <- seq((length(Datas) - 1)*(-0.1),(length(Datas) - 1)*0.1,length=length(Datas))
  
  if(showMax){
    Max_data <- max(sapply(Datas,max))
  }else{
    Max_data <- max(sapply(Datas,function(Data){
      return(apply(Data,2,mean))}) +
                    sapply(Datas,function(Data){
                      return(apply(Data,2,sd))}))
  }

  Min_data <- min(sapply(Datas,function(Data){
      return(apply(Data,2,mean))}) -
                    sapply(Datas,function(Data){
                      return(apply(Data,2,sd))}))
  
  plot(rbind(c(DELTA_T[1],Max_data),
             c(DELTA_T[length(DELTA_T)],Min_data)),
       main=mainName,
       type="n",
       xlab=rowname,
       ylab=colname)

  mapply(function(Data,Color,margin){
    lines(cbind(DELTA_T,apply(Data,2,mean)),
          col=Color)

    arrows(DELTA_T + margin,apply(Data,2,mean),
           DELTA_T + margin,apply(Data,2,mean) - apply(Data,2,sd),
           angle=90,length=0.1,lwd=1,
           col=Color)

    arrows(DELTA_T + margin,apply(Data,2,mean),
           DELTA_T + margin,apply(Data,2,mean) + apply(Data,2,sd),
           angle=90,length=0.1,lwd=1,
           col=Color)
    
    if(showMax){
      lines(cbind(DELTA_T,apply(Data,2,max)),
            col=Color,lty="dashed",lwd=1.5)
    }
  },Datas,Colors,sigma_margin)

  if(showLegend){
    if(showMax){
      legend(par()$usr[2],par()$usr[4],
             legend=c(legends,"max each"),
             lty=c(lineType,"dashed"),
             lwd=c(rep(3,3),1.5),
             col=c(Colors,"black"))
    }else{
      legend(par()$usr[2],par()$usr[4],
             legend=legends,
             lty=lineType,
             lwd=rep(3,3),
             col=Colors)
    }
  }
  if(showLegend) par(old_par)
}

