t_test <- function(data1,data2,limit){

  q <- qt(1 - limit,length(data1) + length(data2) - 2)
  
  data1_length <- length(data1)
  data2_length <- length(data2)

  data1_var <- var(data1)
  data2_var <- var(data2)

  t <- (mean(data1) - mean(data2))/sqrt(data1_var/length(data1) + data2_var/length(data2))
  
  if(abs(t) > q) return(FALSE)
  else return(TRUE)
}
  
#A <- c(3,4,4,3,2,3,1,3,5,2)
#B <- c(5,5,6,7,4,4,3,5,6,5)

#print(t_test(A,B,0.01))
