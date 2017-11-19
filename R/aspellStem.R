aspellStem <- function(texts, clean = FALSE, pattern_flag = NULL, 
  word_flag = NULL, stem.type="prevalent",stops=stopwords()){
  require(tm)
  require(SnowballC)
  Sys.setenv(NOAWT = TRUE) 
  
  if(clean == T){
    texts <- gsub("[^[:alnum:][:space:]]", "", texts)
    texts <- removeWords(texts, words = stops)
    texts <- gsub("^\\s+|\\s+$|(?<=\\s)\\s+", "", texts, perl = TRUE)
  }
  
  input <- lapply(1:length(texts), function(i) {
    words <- unlist(strsplit(texts[i], "\\s+"))
    index <- rep(i, length(words))
    cbind(words,index)
  })
  
  input <- data.frame(do.call("rbind", input), stringsAsFactors = FALSE)
  
  if(!is.null(pattern_flag)) {
    pattern_flag <- lapply(pattern_flag, function(x, ...) {
      grepl(x, input$words)
    })
    pattern_flag <- rowSums(do.call("cbind", pattern_flag), na.rm = TRUE) > 0
  } else {
    pattern_flag <- rep(FALSE, length(input$words))
  }
  
  if(!is.null(word_flag)) {
    word_flag <- input$words %in% word_flag
  } else {
    word_flag <- rep(FALSE, length(input$words))
  }
  
  input_duplicated <- duplicated(input$words)
  
  input_words <- input$words[!pattern_flag & !word_flag & !input_duplicated]
  
  stemmed <- stemDocument(input_words)
  
  restem <- try(stemCompletion(stemmed, input$words, type=stem.type), 
                silent = TRUE)
  if(class(restem) == "try-error"){
    restem <- input_words
  }

  out <- restem[match(input$words, input_words)]
  
  out[pattern_flag | word_flag] <- input$words[pattern_flag | word_flag]
  
  final_words <- tapply(out, input$index, "paste", collapse = " ")
  
  final_index <- as.numeric(names(final_words))
  
  final <- data.frame(
    "words" = final_words,
    "index" = final_index,
    stringsAsFactors = FALSE)
  
  final <- final[order(final$index),]
  
  as.vector(final$words)
}
