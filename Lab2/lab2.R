compare <- function(str, df) {
  cols <- colnames(df)
  prod <- grep(str, cols)
  prod_cols <- df[cols[prod]]
  exp <- colnames(prod_cols)[1]
  imp <- colnames(prod_cols)[2]
  indexes <- df[exp] > df[imp]
  return(indexes)

}

export <- function(name, df, region){
  if (df[df['Регион'] == region, name] == TRUE){
    return (name)
  }
  else {
    return(NULL)
  }
}

import <- function(name, df, region){
  if (df[df['Регион'] == region, name] == FALSE){
    return (name)
  }
  else {
    return(NULL)
  }
}
find_branches <- function(df, region, names, func){
  exp <- sapply(names, func, df=df, region=region)
  exp <- exp[!sapply(exp,is.null)]
  exp <- unname(unlist(exp))
  return(exp)
}

get_branches <- function(df, region){
  names <- c("Прод", "ТЭК", "Древ", "Мет", "Маш")
  new_cols <- data.frame(sapply(names, compare, df = df))
  merged <- cbind.data.frame(df, new_cols)
  exp <- find_branches(merged, region, names, export)
  imp <- find_branches(merged, region, names, import)
  return(list(export=exp, import=imp))
}