MakeWordLists <- function(wlists = c("gi","afinn","liu")){
  
  require(tm)
  
  if(is.null(wlists)){
    stop("wslists must be a character vector")
  }
  
  # General Inquirer world list
  
  if("gi" %in% wlists){
    print("Creating lists from General Inquirer.")
    
    gi <- read.delim(
      "http://www.wjh.harvard.edu/~inquirer/inqtabs.txt", as.is=T)[
        ,c("Entry", "Positiv", "Negativ")]
    gi <- gi[(grepl("[[:print:]]", gi$Positiv) | 
        grepl("[[:print:]]", gi$Negativ)) &	nchar(gi$Entry>1),]
    gi$Entry <- gsub("[^[:alpha:] ]", "", gi$Entry)
    gi <- unique(gi)
    
    gi.pos <- tolower(gi[grepl("[[:print:]]", gi$Positiv), "Entry"])
    gi.neg <- tolower(gi[grepl("[[:print:]]", gi$Negativ), "Entry"])
  }
  
  # AFINN word list
  
  if("afinn" %in% wlists){
    print("Creating lists from AFINN site.")
    
    temp <- tempfile()
    download.file("http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/6010/zip/imm6010.zip",temp)
    afinn <- read.delim(unz(temp, "AFINN/AFINN-111.txt"), header=F, as.is=T)
    unlink(temp)
    
    afinn.pos <- unique(afinn[afinn$V2>0,"V1"])
    afinn.neg <- unique(afinn[afinn$V2<0,"V1"])
  }
  
  # Liu word list
  if("liu" %in% wlists){
    print("Creating lists from Bing Liu lexicon.")
    
    liu.pos <- read.delim("https://raw.github.com/schaunwheeler/tmt/master/liu_positive_words.txt", 
      header=F, as.is=T, skip=35)$V1
    liu.neg <- read.delim("https://raw.github.com/schaunwheeler/tmt/master/liu_negative_words.txt", 
      header=F, as.is=T, skip=35)$V1
  }
  
  # Combine and clean word lists
  
  print("The following lists have been loaded:")
  sapply(c(as.character(bquote(gi.pos)),
    as.character(bquote(gi.neg)),
    as.character(bquote(afinn.pos)),
    as.character(bquote(afinn.neg)),
    as.character(bquote(liu.pos)),
    as.character(bquote(liu.neg))),exists)
  
  if(!exists(as.character(bquote(gi.pos)))){
    gi.pos <- NULL
  }
  if(!exists(as.character(bquote(gi.neg)))){
    gi.neg <- NULL
  }
  if(!exists(as.character(bquote(afinn.pos)))){
    afinn.pos <- NULL
  }
  if(!exists(as.character(bquote(afinn.neg)))){
    afinn.neg <- NULL
  }
  if(!exists(as.character(bquote(liu.pos)))){
    liu.pos <- NULL
  }
  if(!exists(as.character(bquote(liu.neg)))){
    liu.neg <- NULL
  }
  
  pos <- unique(c(gi.pos, afinn.pos, liu.pos))
  neg <- unique(c(gi.neg, afinn.neg, liu.neg))
  
  pos <- pos[!grepl("\\d", pos)]
  neg <- neg[!grepl("\\d", neg)]
  
  pos.stem <- stemDocument(pos)
  neg.stem <- stemDocument(neg)
  
  # De-dupe lists and remove characters that will confuse the regex engine
  
  print("De-duping lists")
  
  dupes <- sort(pos.stem[pos.stem %in% neg.stem])
  dupes <- dupes[!(dupes %in% 
      c("awe", "help","exasper", "irrespons", "dumbfound"))]
  
  pos <- pos[!(pos.stem %in% dupes | pos.stem %in% 
      c("exasper", "irrespons", "dumbfound") |
      grepl("\\b((does )?not|can[']?t|don[']?t|no|\\s)\\b", pos.stem))]
  neg <- neg[!(neg.stem %in% dupes | neg.stem %in% 
      c("awe", "help", "bull****", "bull----", "d*mn", "f**k", "sh*t") |
      grepl("\\b((does )?not|can[']?t|don[']?t|no|\\s)\\b", neg.stem))]
  
  pos.stem <- pos.stem[!(pos.stem %in% dupes | pos.stem %in% 
      c("exasper", "irrespons", "dumbfound") |
      grepl("\\b((does )?not|can[']?t|don[']?t|no|\\s)\\b", pos.stem))]
  neg.stem <- neg.stem[!(neg.stem %in% dupes | neg.stem %in% 
      c("awe", "help", "bull****", "bull----", "d*mn", "f**k", "sh*t") |
      grepl("\\b((does )?not|can[']?t|don[']?t|no|\\s)\\b", neg.stem))]
  
  pos <- pos[!duplicated(pos.stem)]
  neg <- neg[!duplicated(neg.stem)]
  
  pos.stem <- pos.stem[!duplicated(pos.stem)]
  neg.stem <- neg.stem[!duplicated(neg.stem)]
  
  pos.part <- pos.stem[grepl("([-]|(er|est)$)", pos.stem)]
  neg.part <- neg.stem[grepl("([-]|(er|est)$)", neg.stem)]
  
  pos.part2 <- gsub("(^\\w+)([-]|(er|est)$)([[:print:]]+)?$", "\\1", pos.part)
  neg.part2 <- gsub("(^\\w+)([-]|(er|est)$)([[:print:]]+)?$", "\\1", neg.part)
  
  pos.part.remove <- pos.part[(pos.part2 %in% pos.stem) | (pos.part2 %in% pos)]
  neg.part.remove <- neg.part[(neg.part2 %in% neg.stem) | (neg.part2 %in% neg)]
  
  pos.remove <- (pos.stem %in% pos.part.remove) | (pos %in% pos.part.remove)
  neg.remove <- (neg.stem %in% neg.part.remove) | (neg %in% neg.part.remove)
  
  pos <- pos[!pos.remove]
  neg <- neg[!neg.remove]
  
  pos.stem <- pos.stem[!pos.remove]
  neg.stem <- neg.stem[!neg.remove]
  
  print("Fitting for regex.")
  
  pos.regex <- pos != pos.stem
  neg.regex <- neg != neg.stem
  
  pos.stem <- gsub("a[+]\\b", "a[+]|a([-]|\\s)plus", pos.stem)
  neg.stem <- gsub("naC/ve", "naive", neg.stem)
  
  pos.stem <- gsub("[-]+", "[[:punct:][:space:]]*", pos.stem, perl = T)
  neg.stem <- gsub("[-]+", "[[:punct:][:space:]]*", neg.stem, perl = T)																
  
  pos.stem <- gsub("([[:alpha:]])\\1+","\\1{2,}", pos.stem, perl=T)
  neg.stem <- gsub("([[:alpha:]])\\1+","\\1{2,}", neg.stem, perl=T)
  
  pos.stem <- gsub("i$","(i|y)", pos.stem, perl=T)
  neg.stem <- gsub("i$","(i|y)", neg.stem, perl=T)
  
  pos.stem[pos.regex] <- paste(pos.stem[pos.regex],"[[:punct:][:alpha:]]*?",sep="")
  neg.stem[neg.regex] <- paste(neg.stem[neg.regex],"[[:punct:][:alpha:]]*?",sep="")
  
  pos.stem <- gsub("(?<=\\s)\\s+|\\s{2,}|\\s+(?=$)", " ", pos.stem, perl = T)
  neg.stem <- gsub("(?<=\\s)\\s+|\\s{2,}|\\s+(?=$)", " ", neg.stem, perl = T)
  
  pos.stem <- sort(unique(pos.stem))
  neg.stem <- sort(unique(neg.stem))
  
  pos <- unique(pos.stem)
  neg <- unique(neg.stem)
  
  pos.dups <- (duplicated(gsub("[[:punct:][:space:]]*", "", pos, fixed = T)) |
      duplicated(gsub("[[:punct:][:space:]]*", "", pos, fixed=T), fromLast = T)) &
    !grepl("[[:punct:][:space:]]*", pos, fixed=T)
  
  neg.dups <- (duplicated(gsub("[[:punct:][:space:]]*", "", neg, fixed = T)) |
      duplicated(gsub("[[:punct:][:space:]]*", "", neg, fixed=T), fromLast = T)) &
    !grepl("[[:punct:][:space:]]**", neg, fixed=T)
  
  pos <- pos[!pos.dups]
  neg <- neg[!neg.dups]
  
  # De-conflict sentiment lists with stopwords
  print("De-conflicting sentiment words and stop words")
  
  posneg <- c(pos,neg)
  stops <- stopwords()
  
  stops.flag <- rep(FALSE,length(stops))
  
  for(i in 1:length(posneg)){ 
    flag.temp <- grepl(paste("\\b", posneg[i], "\\b", sep=""),stops)
    stops.flag <- stops.flag + flag.temp
  }
  
  stops <- stops[stops.flag == 0 & stops!="not"]
  
  list(pos, neg, stops)
}
