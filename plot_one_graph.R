plot_one_graph <- function(Datas,
                           mainName,
                           rowname,
                           colname,
                           legends,
                           Colors,
                           LineType,
                           DELTA_T,
                           showMax,
                           test_result){

  
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
       type="n",
       xlab=rowname,
       ylab=colname)

  mapply(function(Data,Color,linetype,margin){
    lines(cbind(DELTA_T,apply(Data,2,mean)),
          col=Color,
          lty=linetype)

    arrows(DELTA_T + margin,apply(Data,2,mean),
           DELTA_T + margin,apply(Data,2,mean) - apply(Data,2,sd),
           angle=90,length=0.1,
           lwd=Error_bar_lwd,
           lty="longdash",
           col=Color)

    arrows(DELTA_T + margin,apply(Data,2,mean),
           DELTA_T + margin,apply(Data,2,mean) + apply(Data,2,sd),
           angle=90,length=0.1,
           lwd=Error_bar_lwd,
           lty="longdash",
           col=Color)
    
    if(showMax){
      lines(cbind(DELTA_T,apply(Data,2,max)),
            col=Color,lty="dashed",lwd=1.5)
    }
  },Datas,Colors,LineType,sigma_margin)

  if(length(test_result) > 0){
    for(test_i in 1:length(test_result)){

      test <- test_result[[test_i]]

      apply(test,2,function(col){
        if(col[2] == 0){
          text(col[1],Max_data,
               label="*",
               cex=2,
               col=test_i)
        }
      })
    }
  }
}

