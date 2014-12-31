par(lwd=3,
    cex=1.4,
    mex=1.2,
    oma=c(0,0,0,1))

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

xlabs <- c("Tsuishi",
           "Rerative",
           "Rerative Gausian")

Colors <- c("green","blue","red")

#Fs
Filename <- paste(Tsuishi_prefix,"ca_Tsuishi_75_0_Fs.xdr",sep="")
load(Filename)
All_F <- Fs

Filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Fs.xdr",sep="")
load(Filename)
All_F <- cbind(All_F,Fs)

Filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Fs.xdr",sep="")
load(Filename)
All_F <- cbind(All_F,Fs)

All_F <- rbind(apply(All_F,2,mean),
               apply(All_F,2,sd))

Max_Data <- max(All_F[1,] + All_F[2,])
Min_Data <- min(All_F[1,] - All_F[2,])

plot(rbind(c(1,Max_Data),
           c(3,Min_Data)),
     type="n",
     xlab="",
     ylab="F",
     xaxt="n"
     )

for(i in 1:3){

  mean <- All_F[1,i]
  sd <- All_F[2,i]
  
  print(mean)
  print(sd)

  arrows(i,mean,
         i,mean + sd,
         angle=90,length=0.1,lwd=2)
  arrows(i,mean,
         i,mean - sd,
         angle=90,length=0.1,lwd=2)
  points(i,mean,
         col=Colors[i],
         pch=16)  
}

axis(side=1,at=c(1,2,3),labels=xlabs)
