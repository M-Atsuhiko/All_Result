make_legends <- function(legends,colors,ltys){
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
         cex=2)
}
