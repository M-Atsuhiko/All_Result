Gausian_prefix <- "~/workspace/Gausian/Gausian_Result/"
Tsuishi_prefix <- "~/workspace/Tsuishi/Tsuishi_Result/"

outputdir <- "./Graphs/"

DELTA_T <- seq(5,30,by=5)

load(paste(Gausian_prefix,"ca_Rerative_Gaus_st50_75_0_All_Data_FRAME.xdr",sep=""))
ca_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"ca_Rerative_Gaus_st50_75_5_All_Data_FRAME.xdr",sep=""))
ca_reduced_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"k_ca_Rerative_Gaus_75_0_All_Data_FRAME.xdr",sep=""))
k_ca_Gausian_Data <- ALL_DATA_FRAME
load(paste(Gausian_prefix,"k_ca_Rerative_Gaus_75_5_All_Data_FRAME.xdr",sep=""))
k_ca_reduced_Gausian_Data <- ALL_DATA_FRAME

load(paste(Tsuishi_prefix,"passive_Tsuishi_alfa_05_75_0_All_Data_FRAME.xdr",sep=""))
alfa_data<- ALL_DATA_FRAME
load(paste(Gausian_prefix,"passive_Rerative_75_0_All_Data_FRAME.xdr",sep=""))
rerative_data <- ALL_DATA_FRAME

for(dt in DELTA_T){
  test <- ca_reduced_Gausian_Data[rerative_data$DT==dt,]

  test <- test[c("F",
                                        #               "TREE_length",
                                        #               "TREE_volume",
                                        #               "Upper_Dend_volume",
                                        #               "Lower_Dend_volume",
                 "Upper_Dend_length",
                 "Lower_Dend_length",
                 "N_Upper_Syn",
                 "N_Lower_Syn",
                 "Lower_Diam",
                 "Upper_Diam",
                 "Upper_Synaptic_length_diam",
                 "Lower_Synaptic_length_diam",
                 ## "TREE_Ca_ratio",
                 ## "Upper_Ca_ratio",
                 ## "Lower_Ca_ratio",
                 "Upper_Gaus_Ca_peak",
                 "Upper_Gaus_Ca_mean",
                 "Upper_Gaus_Ca_sd",
                 "Lower_Gaus_Ca_peak",
                 "Lower_Gaus_Ca_mean",
                 "Lower_Gaus_Ca_sd",
                 "N_Upper_bif",
                 "N_Lower_bif"
                 )]

  print(cor(test))

  par(oma=c(10,0,0,0))
  barplot(cor(test)[,"F"],las=2,
          main=paste("dt = ",dt,", vs F",sep=""))

  lines(cbind(c(0,100),
              c(0.2,0.2)),
        col="blue")
  lines(cbind(c(0,100),
              c(-0.2,-0.2)),
        col="blue")

  lines(cbind(c(0,100),
              c(0.5,0.5)),
        col="green")
  lines(cbind(c(0,100),
              c(-0.5,-0.5)),
        col="green")

  lines(cbind(c(0,100),
              c(0.8,0.8)),
        col="red")
  lines(cbind(c(0,100),
              c(-0.8,-0.8)),
        col="red")

  ## test_line <- lm(formula = F ~., data=test)
  ## print(summary(test_line))
  Filename <- paste(outputdir,"ca_vsF_dt",dt,".eps",sep="")
  dev.copy2eps(file=Filename)
}
