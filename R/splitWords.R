splitWords <- function(inp, ...) {
    
  inp_nchar <- nchar(inp)
  inp_check <- rep(FALSE, length(inp))
  out <- rep("", length(inp))

  while(any(!inp_check) & any(inp_nchar > 0)) {
  
  tries <- lapply(1:max(inp_nchar), function(i) {
    substring(inp,i,inp_nchar)
  })
  
  tries <- lapply(1:length(inp), function(i) {
    sapply(tries, function(j,...) {
      j[i]
    })
  })
  
  tries <- lapply(tries, function(x) {
    x[!duplicated(x) & x != ""]
  })
  
  out_new <- sapply(tries, function(x) {
    check <- check_fun(x)
    possible <- x[check]
    possible[which.max(nchar(possible))]
  })
  
  out_new[sapply(out_new, length) == 0 | sapply(out_new, is.null)] <- ""
  
  out_new <- unlist(out_new)
   
  inp_nchar <- inp_nchar - nchar(out_new)
  
  out <- cbind(out, out_new)
  
  inp <- substring(inp, 1, inp_nchar)
  
  inp_check <- check_fun(inp)
  }

  out <- cbind(out, inp)
  
  final <- sapply(1:nrow(out), function(i) {
    paste(rev(out[i,]), collapse = " ")
  })

  
  gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", final, perl = TRUE)
}