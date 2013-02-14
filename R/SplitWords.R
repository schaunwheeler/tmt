SplitWords <- function(x, correction = F){
  
  check.fun <- function(x){
    !(x %in% aspell(as.factor(x), 
      control = c("--master=en_US --sug-mode=ultra"))$Original)
  }
  
  x <- paste(x, collapse=" ")
  y <- tolower(x)
  y <- strsplit(y, split=" ")[[1]]
  ygood <- check.fun(y)
  z <- y[!ygood]
  out2 <- NULL
  
  
  for(j in 1:length(z)){
    out <- ""
    keep <- 0
    while(keep != nchar(z[j]) | 
        is.na(out[length(out)])){
      test.vec <- (keep+1):(nchar(z[j]))
      opts <- lapply(test.vec,function(i){c(substring(z[j],(keep+1),i))})
      opts <- unlist(opts)
      yes <- sapply(opts, check.fun)
      if(sum(yes)>0){
        goods <- opts[yes]
      }else{
        goods <- " "
      }
      out <- c(out,goods[which.max(nchar(goods))])
      keep <- nchar(gsub(" ", "", paste(out,collapse="")))
      out <- out[out!=""]
      out <- paste(out, collapse=" ")
      tst <- substring(z,nchar(out)+1,nchar(z))
      if(correction == T){
        sugs <- unlist(aspell(as.factor(tst), 
          control = c(
            "--master=en_US --sug-mode=ultra"))$Suggestions)
        if(length(sugs) == 0){
          out
        }else{
          out <- paste(out, grep("^[a-z ]+$",sugs,value=T)[1])
          keep <- nchar(gsub(" ", "", paste(out,collapse="")))
        }
      }else{
        out
      }
    }
    out2 <- c(out2, out)
  }
  y[!ygood] <- out2
  final <- paste(y, collapse=" ")
  gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", final, perl = TRUE)
  
}
