CleanText <- function(x, stops, neutralize=T, remove=NULL){
  
  # convert to lowercase
  x <- tolower(x)
  
  # Remove numbers
  x <- gsub("\\d+", "", x)
  
  # Remove punctiation
  x <- gsub("[']+(?!ll)", "", x, perl = T)
  x <- gsub("[^[:alpha:]'[:space:]]", " ", x, perl = T)
  
  # Insert apostraphes in all contactions
  x <- gsub("\\b(are|ca|could|did|does|do|had|has|have)nt\\b", "\\1n't", x)
  x <- gsub("\\b(is|might|mus|sha|should|was|were|would|wo)nt\\b", 
    "\\1n't", x)
  x <- gsub("\\b(that|there|where|who)(d|ll|s)\\b", "\\1'\\2", x)
  x <- gsub("\\b(she|he)(s)\\b", "\\1'\\2", x)
  x <- gsub("\\bi(m|ve)", "i'\\1", x)
  x <- gsub("\\b(what|they|you)(d|ll|ve|re)", "\\1'\\2", x)
  x <- gsub("\\bweve", "we've", x)
  
  # Remove stops words
  x <- removeWords(x, stops)
  
  # Neutralize
  if(neutralize == T){
    x <- gsub("^[[:print:]]*([[:punct:]]+|no idea|don.?t know|\\bn[[:punct:]]?a[[:punct:]]?\\b)[[:print:]]*$",
      "", x, ignore.case=T)
    x <- gsub("^(.)\\1*$", "", x)
    x[x == "" | is.na(x)] <- "neutral"
  }
  
  if(!is.null(remove)){
    x <- gsub(paste("\\b(", paste(remove, collapse = "|"), ")\\w*?\\b", 
      sep=""), "", x)
    if(neutralize == T){
      x[x == "" | grepl("^\\s+$", x)] <- "neutral"
    }else{
      x[grepl("^\\s+$", x)] <- ""
    }
  }
  
  # remove empty spaces
  x <- gsub("(?<=\\s)\\s+|^\\s+|\\s+(?=$)", "", x, perl = T)
  
  x
}
