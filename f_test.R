f_test <- function(data1,data2,limit){

  p <- var.test(data1,data2)$p.value
           
  if(p < limit) return(FALSE)
  else return(TRUE)
}

