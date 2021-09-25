get_data <-  function(filename){
  getwd()
  data <- read.csv(filename)
}


remove_space <- function(col){
  fixed_col <- gsub(" ", "", col, fixed = TRUE)
  num_col <- as.numeric(fixed_col)
  
  if (any(is.na(num_col))){
    return(col)
  }
  else{
    return(num_col)
  }
}

fix_data <- function(data, work_func)
    return(sapply(data, remove_space))

data <- get_data('test_data_01.csv')
print(fix_data(data, remove_space))

