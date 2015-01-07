source("plot_three_datas.R")

par(lwd=3,
    cex=1.4,
    mex=1.2,
    oma=c(0,0,0,0))

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

DELTA_T <- seq(5,30,by=5)
rowname <- expression(paste("Optimized ",delta,"t"))
Colors <- c("red","blue","green")
legends <- c("Gausian","Liner","Tsuishi")

##### F graph #####
Filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Fs.xdr",sep="")
load(Filename)
Gausian_Fs <- Fs
Filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Fs.xdr",sep="")
load(Filename)
Tsuishi_liner_Fs <- Fs
Filename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Fs.xdr",sep="")
load(Filename)
Tsuishi_alfa_Fs <- Fs

colname <- "F"
plot_three_datas(Gausian_Fs,
                 Tsuishi_liner_Fs,
                 Tsuishi_alfa_Fs,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 DELTA_T,
                 TRUE)
## Max_data <- max(apply(Gausian_Fs,2,max),
##                 apply(Tsuishi_liner_Fs,2,max),
##                 apply(Tsuishi_alfa_Fs,2,max))

## Min_data <- min(apply(Gausian_Fs,2,mean),
##                 apply(Tsuishi_liner_Fs,2,mean),
##                 apply(Tsuishi_alfa_Fs,2,mean))

## plot(rbind(c(DELTA_T[1],Max_data),
##            c(DELTA_T[length(DELTA_T)],Min_data)),
##      type="n",
##      xlab=rowname,
##      ylab=colname)

## lines(cbind(DELTA_T,apply(Gausian_Fs,2,mean)),
##       col="red")
## lines(cbind(DELTA_T,apply(Tsuishi_liner_Fs,2,mean)),
##       col="blue")
## lines(cbind(DELTA_T,apply(Tsuishi_alfa_Fs,2,mean)),
##       col="green")

## lines(cbind(DELTA_T,apply(Gausian_Fs,2,max)),
##       col="red",lty="dashed",lwd=1.5)
## lines(cbind(DELTA_T,apply(Tsuishi_liner_Fs,2,max)),
##       col="blue",lty="dashed",lwd=1.5)
## lines(cbind(DELTA_T,apply(Tsuishi_alfa_Fs,2,max)),
##       col="green",lty="dashed",lwd=1.5)

## legend("topleft",
##        legend=c("Rerative Gausian","Rerative Liner","Tsuishi","max each"),
##        lty=c(rep("solid",3),"dashed"),
##        lwd=c(rep(3,3),1.5),
##        col=c("red","blue","green","black"))

##### TREE length #####
TREE_lengthilename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_TREE_lengths.xdr",sep="")
load(TREE_lengthilename)
Gausian_TREE_lengths <- TREE_lengths
TREE_lengthilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_TREE_lengths.xdr",sep="")
load(TREE_lengthilename)
Tsuishi_liner_TREE_lengths <- TREE_lengths
TREE_lengthilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__TREE_lengths.xdr",sep="")
load(TREE_lengthilename)
Tsuishi_alfa_TREE_lengths <- TREE_lengths

colname <- expression(paste("sum TREE length [",mu,"m]",sep=""))

plot_three_datas(Gausian_TREE_lengths,
                 Tsuishi_liner_TREE_lengths,
                 Tsuishi_alfa_TREE_lengths,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 DELTA_T,
                 FALSE)

##### TREE volume #####
TREE_volumeilename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_TREE_volumes.xdr",sep="")
load(TREE_volumeilename)
Gausian_TREE_volumes <- TREE_volumes
TREE_volumeilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_TREE_volumes.xdr",sep="")
load(TREE_volumeilename)
Tsuishi_liner_TREE_volumes <- TREE_volumes
TREE_volumeilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__TREE_volumes.xdr",sep="")
load(TREE_volumeilename)
Tsuishi_alfa_TREE_volumes <- TREE_volumes

colname <- expression(paste("sum TREE volume [",mu,"m]",sep=""))

## Gausian_TREE_volumes <- log10(Gausian_TREE_volumes)
## Tsuishi_liner_TREE_volumes <- log10(Tsuishi_liner_TREE_volumes)
## Tsuishi_alfa_TREE_volumes <- log10(Tsuishi_alfa_TREE_volumes)

plot_three_datas(Gausian_TREE_volumes,
                 Tsuishi_liner_TREE_volumes,
                 Tsuishi_alfa_TREE_volumes,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 DELTA_T,
                 FALSE)

##### Ca conductance amount #####
Ca_amountilename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Ca_amount.xdr",sep="")
load(Ca_amountilename)
Gausian_Ca_amounts <- Ca_amounts
Ca_amountilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Ca_amount.xdr",sep="")
load(Ca_amountilename)
Tsuishi_liner_Ca_amounts <- Ca_amounts
Ca_amountilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Ca_amount.xdr",sep="")
load(Ca_amountilename)
Tsuishi_alfa_Ca_amounts <- Ca_amounts

#コンダクタンスに関しては、Upper Lowerのコンダクタンス含有量、最大値などが4つ入っている
colname <- expression(paste("Ca conductance amount [S/c",m^2,"]",sep=""))

plot_three_datas(Gausian_Ca_amounts,
                 Tsuishi_liner_Ca_amounts,
                 Tsuishi_alfa_Ca_amounts,
                 rowname,
                 colname,
                 legends,
                 Colors,
                 DELTA_T,
                 FALSE)
