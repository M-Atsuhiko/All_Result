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
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_TREE_lengths.xdr",sep="")
load(TREE_length_filename)
Gausian_TREE_lengths <- TREE_lengths
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_TREE_lengths.xdr",sep="")
load(TREE_length_filename)
reduced_Gausian_TREE_lengths <- TREE_lengths
TREE_length_filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_TREE_lengths.xdr",sep="")
load(TREE_length_filename)
Tsuishi_liner_TREE_lengths <- TREE_lengths
TREE_length_filename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__TREE_lengths.xdr",sep="")
load(TREE_length_filename)
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

colname <- expression(paste("[",mu,m^3,"]",sep=""))
mainName <- expression(paste("TREE volume (",log[10],")",sep=""))
OutputFilename <- paste(OutputDir,"TREE_volume_4.eps",sep="")
barplot_datas(list(log10(Gausian_TREE_volumes),
                   log10(Tsuishi_liner_TREE_volumes),
                   log10(Tsuishi_alfa_TREE_volumes),
                   log10(reduced_Gausian_TREE_volumes)
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
mainName <- expression(paste("Upper Dendrite CaT conductance amount",sep=""))
OutputFilename <- paste(OutputDir,"Upper_Dend_Ca_amount_4.eps",sep="")
barplot_datas(list(Gausian_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
                   Tsuishi_liner_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
                   Tsuishi_alfa_Ca_amounts[["Upper_Ca_amounts"]]*10^9,
                   reduced_Gausian_Ca_amounts[["Upper_Ca_amounts"]]*10^9
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
##### Lower Ca conductance amount #####
mainName <- expression(paste("Lower Dendrite CaT conductance amount",sep=""))
OutputFilename <- paste(OutputDir,"Lower_Dend_ca_amount_4.eps",sep="")
barplot_datas(list(Gausian_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
                   Tsuishi_liner_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
                   Tsuishi_alfa_Ca_amounts[["Lower_Ca_amounts"]]*10^9,
                   reduced_Gausian_Ca_amounts[["Lower_Ca_amounts"]]*10^9
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

##### Upper Ca conductance ratio #####
Ca_ratio_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus_75_0_Ca_ratio.xdr",sep="")
load(Ca_ratio_filename)
Gausian_Ca_ratios <- Ca_Ratios
TREE_length_filename <- paste(Gausian_prefix,"ca_Rerative_Gaus75_5_Ca_ratio.xdr",sep="")
load(TREE_length_filename)
reduced_Gausian_Ca_ratios <- Ca_Ratios
Ca_ratio_filename <- paste(Tsuishi_prefix,"ca_Rerative_liner_75_0_Ca_ratio.xdr",sep="")
load(Ca_ratio_filename)
Tsuishi_liner_Ca_ratios <- Ca_Ratios
Ca_ratio_filename <- paste(Tsuishi_prefix,"ca_Tsuishi_alfa05__Ca_ratio.xdr",sep="")
load(Ca_ratio_filename)
Tsuishi_alfa_Ca_ratios <- Ca_Ratios

colname <- "%"
mainName <- expression(paste("Upper Dendrite CaT conductance ratio",sep=""))
OutputFilename <- paste(OutputDir,"Upper_Dend_Ca_ratio_4.eps",sep="")
barplot_datas(list(Gausian_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
                   Tsuishi_liner_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
                   Tsuishi_alfa_Ca_ratios[["Upper_Ca_ratios"]]*10^2,
                   reduced_Gausian_Ca_ratios[["Upper_Ca_ratios"]]*10^2
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
##### Lower Ca conductance ratio #####
mainName <- expression(paste("Lower Dendrite CaT conductance ratio",sep=""))
OutputFilename <- paste(OutputDir,"Lower_Dend_Ca_ratio_4.eps",sep="")
barplot_datas(list(Gausian_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
                   Tsuishi_liner_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
                   Tsuishi_alfa_Ca_ratios[["Lower_Ca_ratios"]]*10^2,
                   reduced_Gausian_Ca_ratios[["Lower_Ca_ratios"]]*10^2
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

##### TREE Ca conductance_amount #####
mainName <- expression(paste("Dendritic Ca Conductance amount",sep=""))
OutputFilename <- paste(OutputDir,"TREE_ca_amount_4.eps",sep="")
All_Gausian_Ca_amount <- Gausian_Ca_amounts[["Lower_Ca_amounts"]] + Gausian_Ca_amounts[["Upper_Ca_amounts"]]
All_Tsuishi_liner_Ca_amount <- Tsuishi_liner_Ca_amounts[["Lower_Ca_amounts"]] + Tsuishi_liner_Ca_amounts[["Upper_Ca_amounts"]]
All_Tsuishi_alfa_Ca_amount <- Tsuishi_alfa_Ca_amounts[["Lower_Ca_amounts"]] + Tsuishi_alfa_Ca_amounts[["Upper_Ca_amounts"]]
All_reduced_Gausian_Ca_amount <- reduced_Gausian_Ca_amounts[["Lower_Ca_amounts"]] + reduced_Gausian_Ca_amounts[["Upper_Ca_amounts"]]
colname <- expression(paste("[pS/c",m^2,"]",sep=""))
barplot_datas(list(All_Gausian_Ca_amount*10^9,
                   All_Tsuishi_liner_Ca_amount*10^9,
                   All_Tsuishi_alfa_Ca_amount*10^9,
                   All_reduced_Gausian_Ca_amount*10^9
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
##### TREE Ca conductance_ratio #####
mainName <- "Dendritic Ca Conductance ratio"
OutputFilename <- paste(OutputDir,"TREE_ca_ratio_4.eps",sep="")
All_Gausian_Ca_ratio <- All_Gausian_Ca_amount/(Gausian_Ca_amounts[["Lower_Ca_maxs"]] + Gausian_Ca_amounts[["Upper_Ca_maxs"]])
All_Tsuishi_liner_Ca_ratio <- All_Tsuishi_liner_Ca_amount/(Tsuishi_liner_Ca_amounts[["Lower_Ca_maxs"]] + Tsuishi_liner_Ca_amounts[["Upper_Ca_maxs"]])
All_Tsuishi_alfa_Ca_ratio <- All_Tsuishi_alfa_Ca_amount/(Tsuishi_alfa_Ca_amounts[["Lower_Ca_maxs"]] + Tsuishi_alfa_Ca_amounts[["Upper_Ca_maxs"]])
All_reduced_Gausian_Ca_ratio <- All_reduced_Gausian_Ca_amount/(reduced_Gausian_Ca_amounts[["Lower_Ca_maxs"]] + reduced_Gausian_Ca_amounts[["Upper_Ca_maxs"]])
colname <- "%"
barplot_datas(list(All_Gausian_Ca_ratio*10^2,
                    All_Tsuishi_liner_Ca_ratio*10^2,
                   All_Tsuishi_alfa_Ca_ratio*10^2,
                    All_reduced_Gausian_Ca_ratio*10^2
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
