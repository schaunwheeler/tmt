aspellCheck <- function(input, output = "eval", sep = FALSE, keep_caps = TRUE,
  pattern_flag = NULL, word_flag = NULL, split_missing = FALSE, 
  mode = "ultra", dict = "en_US", ...){
  
  input <- lapply(1:length(input), function(i) {
    words <- unlist(strsplit(input[i], "\\s+"))
    index <- rep(i, length(words))
    cbind(words,index)
  })
  
  input <- data.frame(do.call("rbind", input), stringsAsFactors = FALSE)
  
  input_caps_first <- grepl("^[[:upper:]][[:lower:]]", input$words)
  input_caps_all <- grepl("^[[:upper:]]+$", input$words)
  
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
    
  input$words <- tolower(input$words)
  
  input_duplicated <- duplicated(input$words)
  
  input_words <- input$words[!pattern_flag & !word_flag & !input_duplicated]
  
  check <- aspell(as.factor(input_words), 
      control = paste(
          paste0("--master=",dict),
          paste0("--sug-mode=", mode)
        )
  )
    
    if(nrow(check) == 0) {
      if(output == "eval") {
        out <- rep(TRUE,length(input$words))
      }
      if(output == "sugg") {
        out <- rep(NA, length(input$words))
      }
      if(output == "fix") {
        out <- input$words
      }
    } else {
      
      sep_pattern <- ifelse(sep, "^[a-z ]+$", "^[a-z]+$")
      
      if(output == "eval"){
        out <- rep(TRUE, length(input$words))
        out[!is.na(match(input$words, check$Original))] <- FALSE
        out[pattern_flag | word_flag] <- TRUE
        final_words <- tapply(out, input$index, "c")
      }
      if(output == "sugg"){
        out <- check$Suggestions[match(input$words, check$Original)]
        out[sapply(out, is.null)] <- NA
        final_words <- tapply(out, input$index, "c")
      }
      if(output == "fix"){
        recs <- mapply(grepl, sep_pattern, check$Suggestions)
        recs[sapply(recs, length) == 0] <- FALSE
          if(is.list(recs)){
            recs <- unlist(sapply(recs, which.max))
          }else{
            recs <- which.max(recs)
          }
        recs <- sapply(1:length(recs), function(i) {
          out <- check$Suggestions[[i]][recs[i]]
        })
        
        if(split_missing) {
          recs[sapply(recs, is.null)] <-  Split_Words(
            check$Original[sapply(recs, is.null)]
            )
        } else {
        recs[sapply(recs, is.null)] <- check$Original[sapply(recs, is.null)]
        }
        
        recs <- recs[match(input$words, check$Original)]
        
        recs[sapply(recs, is.null)] <- input$words[sapply(recs, is.null)]
        
        recs <- unlist(recs)
        
        recs[pattern_flag | word_flag] <- input$words[pattern_flag | word_flag]

        if(keep_caps) {
          recs[input_caps_first] <- gsub("(^[[:alpha:]])", "\\U\\1", 
                                         recs[input_caps_first], perl = TRUE)
          
          recs[input_caps_all] <- gsub("([[:alpha:]])", "\\U\\1", 
                                       recs[input_caps_all], perl = TRUE)
        }
        
        out <- recs
        
        final_words <- tapply(out, input$index, "paste", collapse = " ")
        
      }
      
      final_index <- as.numeric(names(final_words))
      
      final <- data.frame(
        "words" = final_words,
        "index" = final_index,
        stringsAsFactors = FALSE)
      
      final <- final[order(final$index),]
    }

  as.vector(final$words)
  
}