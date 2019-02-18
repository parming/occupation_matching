### Helper devised to ease list operations

list_wrap <- function(list) {
  if (length(list) == 0 || (length(list) == 1 && is.na(list))) {
    return(c())
  } else {
    return(list)
  }
}

list_invert <- function(list) {
  new_list <<- list()
  mapply(function(key, values) { 
    
    lapply(values, function(value) {
      if (is.null(new_list[value])) {
        new_list[[value]] <<- c()
      }
      new_list[[value]] <<- c(new_list[[value]], key)
    })
  }, names(list), list)
  
  return(new_list)
}
