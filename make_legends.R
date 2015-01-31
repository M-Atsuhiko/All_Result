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

## source("graph_setting.R")
## cat("the test legend \n")
## test_legends <- c("Upper Dendrite",
##                   "Lower Dendrite",
##                   "t-test")

## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"passive_one_legend.eps",sep="")
## make_legends(test_legends,c("red","blue","black"),c(rep("solid",2),"blank"),c("","","*"))

#synapse legend generator
## source("graph_setting.R")
## cat("the synapse legend \n")
## test_legends <- c("Blue synapse",
##                   "Red synapse")
## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"synapse_legend.eps",sep="")
## make_legends(test_legends,c("skyblue","violetred"),rep("blank",2),rep(16,2))
## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")

# 2simulation legend generator
## source("graph_setting.R")
## cat("the 2simulation legend \n")

## test_legends <- c(expression(Blue %->% Red),
##                   expression(Red %->% Blue),
##                   expression(paste(Blue %->% Red," ",(-CaT))),
##                   expression(paste(Red %->% Blue," ",(-CaT))))

## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"ca_2simulation_legend.eps",sep="")
## make_legends(test_legends,
##              c("blue","red","blue","red"),
##              c(rep("solid",2),rep("dashed",2)),rep("",4))

## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")

## source("graph_setting.R")
## cat("the passive legend \n")
## test_legends <- c("Torben et al.",
##                   "Relative",
##                   "t-test")
## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"Tsuishi_Rerative_legend.eps",sep="")
## make_legends(test_legends,c(rainbow(2),"black"),c(rep("solid",2),"blank"),c("","","*"))
## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")


# #------------- Gaus
## source("graph_setting.R")
## cat("the Gaus legend \n")
## test_legends <- c("t-test","t-test (reduced)")
## OutputDir <- "~/workspace/Syuron/Images_Result/"
## Filename <- paste(OutputDir,"Gaus_legend.eps",sep="")
## make_legends(test_legends,c("black","black"),rep("blank",2),c("*","+"))
## dev.copy2eps(file=Filename)
## cat("Output ->",Filename,"\n")
