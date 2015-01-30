make_legends <- function(legends,colors,ltys,pchs){
  par(xpd=NA)

  plot(1,1,
       type="n",
       xlab="",
       ylab="",
       xaxt="n",
       yaxt="n",
       bty="n",
       mar=c(0,0,0,0)
       )
  
  legend(0.45,1.4,
         legend=legends,
         col=colors,
         lty=ltys,
         pch=pchs,
         cex=2)
}

## # 検定legend作成
## source("graph_setting.R")
## cat("the test legend \n")

## test_legends <- c("t-test (G-P, L-P) ",
##                   "t-test (G-Gr, L-Lr)  ",
##                   "t-test (Gr-Lr) ")

## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"test_legend.eps",sep="")
## make_legends(test_legends,c(rep("black",2),"green"),1,c("*","#","+"))
## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")


# synapse legend generator
## source("graph_setting.R")
## cat("the synapse legend \n")

## test_legends <- c("Blue synapse",
##                   "Red synapse")

## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"synapse_legend.eps",sep="")
## make_legends(test_legends,c("skyblue","violetred"),1,rep(21,2))
## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")
