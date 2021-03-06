plot_one_graph <- function(Datas,
                           mainName,
                           rowname,
                           colname,
                           legends,
                           Colors,
                           LineType,
                           DELTA_T,
                           showMax,
                           Gaus_liner_test_result,
                           reduced_test_result,
                           reduced_Gaus_liner_test_result,
                           star_black){
  
  sigma_margin <- seq((length(Datas) - 1)*(-0.1),(length(Datas) - 1)*0.1,length=length(Datas))
  
  if(showMax){
    Max_data <- max(sapply(Datas,max))
  }else{
    Max_data <- max(sapply(Datas,function(Data){
      return(apply(Data,2,mean))}) +
                    sapply(Datas,function(Data){
                      return(apply(Data,2,sd))}))
  }

  Min_data <- max(min(sapply(Datas,function(Data){
    return(apply(Data,2,mean))}) -
                      sapply(Datas,function(Data){
                        return(apply(Data,2,sd))})),
                  0)#一応0以下のデータにならないようにしておく
  
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
           col=Color,
           xpd=FALSE)

    arrows(DELTA_T + margin,apply(Data,2,mean),
           DELTA_T + margin,apply(Data,2,mean) + apply(Data,2,sd),
           angle=90,length=0.1,
           lwd=Error_bar_lwd,
           lty="longdash",
           col=Color,
           xpd=FALSE)
    
    if(showMax){
      lines(cbind(DELTA_T,apply(Data,2,max)),
            col=Color,lty="dashed",lwd=1.5)
    }
  },Datas,Colors,LineType,sigma_margin)

  if(length(Gaus_liner_test_result)){
    apply(Gaus_liner_test_result,2,function(col){
      if(col[2] == 0){
        text(col[1],par()$usr[3] + (par()$usr[4] - par()$usr[3])*0.97,
             label="*",
             cex=2,
             col="black")
      }
    })
  }
  
  if(length(reduced_test_result) > 0){
    for(test_i in 1:length(reduced_test_result)){
      test <- reduced_test_result[[test_i]]
      apply(test,2,function(col){
        if(col[2] == 0){
          text(col[1],par()$usr[3] + (par()$usr[4] - par()$usr[3])*0.92,
               label="#",
                 cex=1,
                 col=Colors[test_i*2])
        }
      })
    }
  }
  
  if(length(reduced_Gaus_liner_test_result) > 0){
    apply(reduced_Gaus_liner_test_result,2,function(col){
      if(col[2] == 0){
        text(col[1],par()$usr[3] + (par()$usr[4] - par()$usr[3])*0.87,
             label="+",
             cex=1.3,
             col="green")
      }
    })
  }
}


