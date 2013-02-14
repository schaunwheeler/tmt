AspellCheck <- function(input, output = "eval", sep = FALSE, cap.flag = "none", 
  ignore=NULL, split.missing = FALSE, progress = "text", parallel = FALSE){
  
  aspell.internal <- function(inp,...){
    
    input <- unlist(strsplit(inp, split="\\s+"))
    
    if(grepl("first|all|none", cap.flag)){
      if(cap.flag=="first"){
        flag.words <- grepl("^[[:upper:]][[:lower:]]+$", input)
      }
      if(cap.flag=="all"){
        flag.words <- grepl("^[[:upper:]]+$", input)
      }
      if(cap.flag=="none"){
        flag.words <- rep(FALSE,length(input))
      }
    }else{
      stop("cap.flag must be 'first', 'all', or 'none")
    }
    
    ignore.words <- tolower(input) %in% tolower(ignore)
    skip.words <- ignore.words | flag.words
    
    x <- input[!skip.words]
    x.uniq <- unique(x)
    
    check <- aspell(as.factor(x.uniq), 
      control = c("--master=en_US --sug-mode=ultra"))
    
    if(nrow(check)==0){
      if(output == "eval"){
        out <- rep(TRUE,length(input))
        xeval <- data.frame()
      }
      if(output == "sugg"){
        out <- rep(NA,length(input))
        xeval <- data.frame()
      }
      if(output == "fix"){
        out <- paste(input, collapse = " ")
        xeval <- data.frame()
      }
    }else{
      
      pattern <- ifelse(sep, "^[a-z ]+$", "^[a-z]+$")
      good <- !(x %in% check$Original)
      missing <- x %in% check$Original[sapply(check$Suggestions,is.null)]
      xeval <- x[!good & !missing]
      
      if(output == "eval"){
        out <- rep(TRUE,length(input))
        out[!skip.words] <- good
      }
      if(output == "sugg"){
        out <- rep(NA,length(input))
        out[!skip.words][!good] <- check$Suggestions
      }
      if(output == "fix"){
        if(length(xeval) == 0){
          out <- ifelse(split.missing,SplitWords(x),x)
        }else{
          ind <- mapply(grepl, pattern, check$Suggestions)
          ind.list <- is.list(ind)
          if(ind.list == T){
            ind <- unlist(sapply(ind, which.max))
          }else{
            ind <- which.max(ind)
          }
          picked <- rep(NA,length(ind))
          for(i in 1:length(ind)){
            picked[i] <- check$Suggestions[!sapply(check$Suggestions,
              is.null)][[i]][ind[i]]}
          out <- x
          out[!good & !missing] <- picked[match(out[!good & !missing],
            check$Original)]
          if(split.missing == TRUE & sum(missing)>0){
            out[missing] <- SplitWords(out[missing])
          }
        }
      }
    }
    
    if(grepl("eval|sugg", output) == TRUE | 
        (grepl("fix", output) == TRUE & length(xeval) == 0)){
      final <- out
    }else{
      final <- input
      final[!skip.words] <- out
      final <- paste(final, collapse=" ")
      final <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+","",final,perl=TRUE)
    }
    final
  }
  
  if(output == "fix"){
    laply(input, aspell.internal, .progress = progress, .parallel = parallel)
  }else{
    llply(input, aspell.internal, .progress = progress, .parallel = parallel)
  }
}
