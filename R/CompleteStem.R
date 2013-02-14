CompleteStem <- function(texts, clean = T, stem.type="prevalent",stops=stopwords()){
  require(tm)
  require(Snowball)
  
  print("Stemming documents.")
  
  if(clean == T){
    texts <- CleanText(texts, stops = stops)		
  }
  
  stemmed <- llply(texts,function(x){
    x <- stemDocument(unlist(strsplit(x, split= " ")))
  }, .progress = "text")
  
  sent.dict <- Corpus(VectorSource(strsplit(paste(texts, collapse=" "), 
    split= " ")))
  
  print("Restemming documents.")
  
  restemmed <- laply(stemmed,function(x, ...){
    
    leave <- (x %in% 
        aspell(as.factor(x), 
          control = c("--master=en_US --sug-mode=ultra"))$Original)	
    
    if(!is.logical(leave)){
      leave <- 0
    }
    
    out <- x
    
    if(mean(leave) > 0){
      y <- x[leave]
      
      restem <- try(stemCompletion(y, sent.dict, 
        type=stem.type), silent=T)
      if(class(restem) == "try-error"){
        restem <- y
      }
      out[leave] <- restem
    }
    paste(out, collapse = " ")}, .progress = "text")
  
  restemmed
}
