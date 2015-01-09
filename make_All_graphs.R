source("plot_datas.R")
source("barplot_datas.R")

par(lwd=3,
    cex=1.4,
    mex=1.2,
    oma=c(0,0,0,0))

Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

DELTA_T <- seq(5,30,by=5)
rowname <- expression(paste("Optimized ",delta,"t"))
Colors <- c("red","orange","cyan","darkolivegreen1")
legends <- c("Gausian","Liner","Tsuishi","reduced Gausian")
SolidType <- rep("solid",4)

OutputDir <- "./Graphs/"

##### F graph #####
Filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Fs.xdr",sep="")
load(Filename)
Gausian_Fs <- Fs
Filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_Fs.xdr",sep="")
load(Filename)
reduced_Gausian_Fs <- Fs
Filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Fs.xdr",sep="")
load(Filename)
Tsuishi_liner_Fs <- Fs
Filename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Fs.xdr",sep="")
load(Filename)
Tsuishi_alfa_Fs <- Fs

colname <- ""
mainName <- "Function ratio"
OutputFilename <- paste(OutputDir,"Fs.eps",sep="")
plot_datas(list(Gausian_Fs,
                Tsuishi_liner_Fs,
                Tsuishi_alfa_Fs,
                reduced_Gausian_Fs
                ),
           mainName,
           rowname,
           colname,
           legends,
           Colors,
           SolidType,
           DELTA_T,
           TRUE,
           OutputFilename)

##### TREE length #####
TREE_lengt_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_TREE_lengths.xdr",sep="")
load(TREE_lengt_filename)
Gausian_TREE_lengths <- TREE_lengths
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_TREE_lengths.xdr",sep="")
load(TREE_length_filename)
reduced_Gausian_TREE_lengths <- TREE_lengths
TREE_lengt_filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_TREE_lengths.xdr",sep="")
load(TREE_lengt_filename)
Tsuishi_liner_TREE_lengths <- TREE_lengths
TREE_lengt_filename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__TREE_lengths.xdr",sep="")
load(TREE_lengt_filename)
Tsuishi_alfa_TREE_lengths <- TREE_lengths

colname <- expression(paste("[",mu,"m]",sep=""))
mainName <- "TREE length"
OutputFilename <- paste(OutputDir,"TREE_length.eps",sep="")
barplot_datas(list(Gausian_TREE_lengths,
                   Tsuishi_liner_TREE_lengths,
                   Tsuishi_alfa_TREE_lengths,
                   reduced_Gausian_TREE_lengths
                   ),
              mainName,
              rowname,
              colname,
              legends,
              Colors,
              SolidType,
              DELTA_T,
              FALSE,
              OutputFilename)

##### TREE volume #####
TREE_volume_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_TREE_volumes.xdr",sep="")
load(TREE_volume_filename)
Gausian_TREE_volumes <- TREE_volumes
TREE_volume_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_TREE_volumes.xdr",sep="")
load(TREE_volume_filename)
reduced_Gausian_TREE_volumes <- TREE_volumes
TREE_volumeilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_TREE_volumes.xdr",sep="")
load(TREE_volumeilename)
Tsuishi_liner_TREE_volumes <- TREE_volumes
TREE_volumeilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__TREE_volumes.xdr",sep="")
load(TREE_volumeilename)
Tsuishi_alfa_TREE_volumes <- TREE_volumes

colname <- expression(paste("[",mu,"m]",sep=""))
mainName <- "TREE volume"
OutputFilename <- paste(OutputDir,"TREE_volume.eps",sep="")
barplot_datas(list(Gausian_TREE_volumes,
                   Tsuishi_liner_TREE_volumes,
#                   Tsuishi_alfa_TREE_volumes,
                   reduced_Gausian_TREE_volumes
                   ),
              mainName,
              rowname,
              colname,
              legends[-3],
              Colors[-3],
              SolidType[-3],
              DELTA_T,
              FALSE,
              OutputFilename)

##### Upper Ca conductance amount #####
Ca_amountilename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Ca_amount.xdr",sep="")
load(Ca_amountilename)
Gausian_Ca_amounts <- Ca_amounts
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_Ca_amount.xdr",sep="")
load(TREE_length_filename)
reduced_Gausian_Ca_amounts <- Ca_amounts
Ca_amountilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Ca_amount.xdr",sep="")
load(Ca_amountilename)
Tsuishi_liner_Ca_amounts <- Ca_amounts
Ca_amountilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Ca_amount.xdr",sep="")
load(Ca_amountilename)
Tsuishi_alfa_Ca_amounts <- Ca_amounts

colname <- expression(paste("[pS/c",m^2,"]",sep=""))
mainName <- "Upper Dendrite CaT conductance amount"
OutputFilename <- paste(OutputDir,"Upper_Dend_Ca_amount.eps",sep="")
barplot_datas(list(Gausian_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
                   Tsuishi_liner_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
#                   Tsuishi_alfa_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
                   reduced_Gausian_Ca_amounts[["Upper_Ca_amounts"]]*10^9
                   ),
              mainName,
              rowname,
              colname,
              legends[-3],
              Colors[-3],
              SolidType[-3],
              DELTA_T,
              FALSE,
              OutputFilename)
##### Lower Ca conductance amount #####
mainName <- "Lower Dendrite CaT conductance amount"
OutputFilename <- paste(OutputDir,"Lower_Dend_ca_amount.eps",sep="")
barplot_datas(list(Gausian_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
                   Tsuishi_liner_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
#                   Tsuishi_alfa_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
                   reduced_Gausian_Ca_amounts[["Lower_Ca_amounts"]]*10^9
                   ),
              mainName,
              rowname,
              colname,
              legends[-3],
              Colors[-3],
              SolidType[-3],
              DELTA_T,
              FALSE,
              OutputFilename)

##### Upper Ca conductance ratio #####
Ca_ratio_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Ca_ratio.xdr",sep="")
load(Ca_ratio_filename)
Gausian_Ca_ratios <- Ca_Ratios
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_Ca_ratio.xdr",sep="")
load(TREE_length_filename)
reduced_Gausian_Ca_ratios <- Ca_Ratios
Ca_ratioilename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Ca_ratio.xdr",sep="")
load(Ca_ratioilename)
Tsuishi_liner_Ca_ratios <- Ca_Ratios
Ca_ratioilename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Ca_ratio.xdr",sep="")
load(Ca_ratioilename)
Tsuishi_alfa_Ca_ratios <- Ca_Ratios

colname <- "%"
mainName <- "Upper Dendrite CaT conductance ratio"
OutputFilename <- paste(OutputDir,"Upper_Dend_Ca_ratio.eps",sep="")
barplot_datas(list(Gausian_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
                   Tsuishi_liner_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
#                   Tsuishi_alfa_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
                   reduced_Gausian_Ca_ratios[["Upper_Ca_ratios"]]*10^2
                   ),
              mainName,
              rowname,
              colname,
              legends[-3],
              Colors[-3],
              SolidType[-3],
              DELTA_T,
              FALSE,
              OutputFilename)
##### Lower Ca conductance ratio #####
mainName <- "Lower Dendrite CaT conductance ratio"
OutputFilename <- paste(OutputDir,"Lower_Dend_Ca_ratio.eps",sep="")
barplot_datas(list(Gausian_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
                   Tsuishi_liner_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
#                   Tsuishi_alfa_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
                   reduced_Gausian_Ca_ratios[["Lower_Ca_ratios"]]*10^2
                   ),
              mainName,
              rowname,
              colname,
              legends[-3],
              Colors[-3],
              SolidType[-3],
              DELTA_T,
              FALSE,
              OutputFilename)


print(Gausian_Ca_amounts[["Lower_Ca_amounts"]])
