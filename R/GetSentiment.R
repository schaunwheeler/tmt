GetSentiment <- function(vec, pos, neg){
  options(stringsAsFactors = F)
  
  posind <- 1:length(pos)
  negind <- 1:length(neg)
  
  print("evaluating positive words")
  n <- length(pos)
  pb <- txtProgressBar(min = 1, max = n, style=3)
  
  poslist <- lapply(posind, function(i){
    first <- mapply(function(x,...){sum(x!=(-1))}, 
      gregexpr(paste("\\b",pos[i],"\\b", sep=""), vec))
    fake <- mapply(function(x,...){sum(x!=(-1))}, 
      gregexpr(paste0("(not|no)\\s(\\w+\\s)?",pos[i],"\\b"), vec))
    out <- first-fake
    setTxtProgressBar(pb, i)
    out
  })
  
  print("evaluating negative words")
  n <- length(neg)
  pb <- txtProgressBar(min = 1, max = n, style=3)
  
  neglist <- lapply(negind, function(i){
    first <- mapply(function(x,...){sum(x!=(-1))}, 
      gregexpr(paste("\\b",neg[i],"\\b", sep=""), vec))
    fake <- mapply(function(x,...){sum(x!=(-1))}, 
      gregexpr(paste0("(not|no)\\s(\\w+\\s)?",neg[i],"\\b"), vec))
    out <- first-fake
    setTxtProgressBar(pb, i)
    out
  })
  possum <- Reduce("+", poslist)
  negsum <- Reduce("+", neglist)
  
  totalwords <- sapply(gregexpr("\\s+", vec), function(x){sum(x!=(-1))}) + 1
  
  polarity <- (possum-negsum)/(possum+negsum)
  polarity[is.na(polarity)] <- 0
  subjectivity <- (possum+negsum)/totalwords
  positivity <- possum/totalwords
  negativity <- negsum/totalwords
  balance <- (possum-negsum)/totalwords
  
  as.data.frame(cbind("words" = totalwords,
    "positives" = possum,
    "negatives" = negsum,
    "polarity" = polarity,
    "subjectivity" = subjectivity,
    "positivity" = positivity,
    "negativity" = negativity,
    "balance" = balance))
}
