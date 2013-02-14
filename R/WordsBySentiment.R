WordsBySentiment <- function(texts, scores, type = "both", n = 20, binary = TRUE, 
  ...){
  require(tm)
  require(Matrix)
  
  tdm <- removeSparseTerms(TermDocumentMatrix(Corpus(VectorSource(texts))), 
    (length(texts)-2)/length(texts))
  
  if(binary == T){
    tdm.sparse <- sparseMatrix(
      i = tdm$i, 
      j = tdm$j, 
      dimnames = dimnames(tdm))
  }else{
    tdm.sparse <- sparseMatrix(
      i = tdm$i, 
      j = tdm$j, 
      x = tdm$v,
      dimnames = dimnames(tdm))
  }
  
  posind <- tdm$dimnames$Docs[scores>0]
  negind <- tdm$dimnames$Docs[scores<0]
  
  all.freq <- as.data.frame(cbind(
    "terms" = rownames(tdm.sparse), 
    "frequency" = rowSums(tdm.sparse),
    "positive" = rowSums(tdm.sparse[,colnames(tdm.sparse) %in% posind, drop = FALSE]),
    "negative" = rowSums(tdm.sparse[,colnames(tdm.sparse) %in% negind, drop = FALSE])), 
    stringsAsFactors = FALSE)
  
  all.freq$frequency <- as.numeric(all.freq$frequency)
  all.freq$positive <- as.numeric(all.freq$positive)
  all.freq$negative <- as.numeric(all.freq$negative)
  
  all.freq <- all.freq[!(all.freq$positive == 0 & all.freq$negative == 0),]
  
  
  all.freq$difference <- (all.freq$positive-all.freq$negative)/
    all.freq$frequency
  
  all.freq$subjectivity <- (all.freq$positive+all.freq$negative)/
    all.freq$frequency
  
  if(nrow(all.freq) < n){
    n <- nrow(all.freq)
  }
  
  final.freq <- arrange(all.freq,-frequency)[1:n,"terms"]
  final.subj <- arrange(all.freq,-subjectivity)[1:n,"terms"]
  
  if(type == "freq"){
    final.subj <- NULL
  }
  if(type == "subj"){
    final.freq <- NULL
  }
  
  terms.freq <- all.freq[all.freq$terms %in% final.freq,] 
  terms.subj <- all.freq[all.freq$terms %in% final.subj,]
  
  terms.freq$type <- rep("frequency",nrow(terms.freq))
  terms.subj$type <- rep("subjectivity",nrow(terms.subj))
  
  if(type == "freq"){
    final.subj <- NULL
  }
  if(type == "subj"){
    final.freq <- NULL
  }
  
  out <- rbind(terms.freq,terms.subj)
  
  out$type[(out$terms %in% final.freq) & (out$terms %in% final.subj)] <- "both"
  
  unique(out)
}
