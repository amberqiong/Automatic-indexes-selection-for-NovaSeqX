index_selection=function(i7,i5,select_number,max_g=0.5,min_c_t=0.3){
  i7=i7
  i5=i5
  indexes=paste(i7,i5,sep="")
  
  g_percentage=function(indexes){
    num_indexes =length(indexes)
    index_length <- nchar(indexes[[1]])
    
    g_percentages <- numeric(index_length)
    for (i in 1:index_length) {
      g_count <- sum(substr(indexes, i, i) == "G")
      g_percentages[i] <- g_count / num_indexes
    }
    
    return(g_percentages)
  }
  
  c_t_percentage=function(indexes){
    num_indexes <- length(indexes)
    index_length <- nchar(indexes[[1]])
    
    c_t_percentages <- numeric(index_length)
    for (i in 1:index_length) {
      c_count <- sum(substr(indexes, i, i) == "C")
      t_count <- sum(substr(indexes, i, i) == "T")
      c_t_percentages[i] <- (c_count + t_count) / num_indexes
    }
    
    return(c_t_percentages)
  }
  
  generate_index_combination=combn(indexes,select_number)
  
  valid_combination=list()
  n=0
  for (i in 1:ncol(generate_index_combination)){
    g_per=g_percentage(generate_index_combination[,i])
    c_t_per=c_t_percentage(generate_index_combination[,i])
    if (all(g_per<max_g) && all(c_t_per>min_c_t)){
      valid_combination[[length(valid_combination)+1]] =generate_index_combination[,i]
      print(i)
      n=n+1
    }
    if (n>20){
      break
    }
  }
  
  df=data.frame(matrix(ncol=length(valid_combination),nrow=select_number))
  for (i in 1:length(valid_combination)) {
    df[, i] <- c(valid_combination[[i]])
  }
  
  split_df <- data.frame(matrix(ncol = ncol(df) * 2, nrow = nrow(df)))
  for (i in 1:ncol(df)) {
    split_df[, 2*i - 1] <- substr(df[[i]], 1, 10)
    split_df[, 2*i] <- substr(df[[i]], 11, 20)
  }
  
  col_names <- character(ncol(split_df))
  for (i in 1:ncol(df)) {
    col_names[2*i - 1] <- paste("Comb", i, "i7", sep = "_")
    col_names[2*i] <- paste("Comb", i, "i5", sep = "_")
  }
  colnames(split_df) <- col_names
  return(split_df)
  
}
