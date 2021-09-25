merge_data <- function(data){
  counter = 2
  new_data = merge(data[1], data[2], by='id', all=TRUE, suffixes = c(".1",".2"))
  while (counter < 7){
    new_data = merge(new_data, data[counter], by='id', all=TRUE, 
                     suffixes = c(paste0(".", counter),paste0(".", counter + 1)))
    counter = counter + 1
  }
  return(new_data)
}

get_id <- function(data){
  new_data = merge_data(data)
  new_data = na.omit(new_data)
  data_without_id <- new_data[, -c(1)]
  means = rowMeans(data_without_id)
  return(data.frame(id=new_data$id, mean_temp=means))
}

getwd()
load('data.RData')
print(get_id(data))
