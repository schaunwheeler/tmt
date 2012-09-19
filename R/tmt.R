# File-Name:       tmt.R           
# Date:            2012-09-07                     
# Author:          Schaun Wheeler
# Email:           schaun.wheeler@gmail.com                                      
# Purpose:         Attempt to integrate automatic spell correction and regular
#                  expressions into existing text mining tools, and also 
#                  integrate those into some basic sentiment analysis
# Data Used:       inqtabs.txt, AFINN-111.txt, positive-words.txt, 
#                  negative-words.txt
#                  input.docs: set of documents to be corrected and analyzed
# Packages Used:   tm, Snowball, Matrix
# Output File:     
# Data Output:     
# Machine:         Schaun Wheeler's Dell Precision T7500 and MacBook Pro

# Copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: 
# http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Load libraries
library(tm)
library(Snowball)

SetEnvVarW <- function(...){

	print("This function has not been extensively tested - use with caution.")
	print("This function only works on Windows.")
	print("Make sure you have downloaded and installed the following programs:")
	print("    Java JDK from http://www.oracle.com/technetwork/java/javase/downloads/index.html")
	print("    Xpdf from http://www.foolabs.com/xpdf/download.html")
	print("    Aspell from http://aspell.net/win32/")
	print("    Package 'rJava' from http://cran.r-project.org/web/packages/rJava/index.html")
	print("    Package 'RWekajars' from http://cran.r-project.org/web/packages/RWekajars/index.html")
	
	sys <- Sys.info()
	
	root <- gsub("(?<=:/)[[:print:]]+","",getwd(),perl=T)
	
	locs <- grep("Program Files", list.dirs(root,recursive=F), value=T)
	
	path.vars <- unique(c(strsplit(Sys.getenv("PATH"),split=";")))[[1]]
	
	java.vars <- unique(c(strsplit(Sys.getenv("JAVA_HOME"),split=";")))[[1]]
	
	noawt.vars <- unique(c(strsplit(Sys.getenv("NOAWT"),split=";")))[[1]]
	
	wekahome.vars <- unique(c(strsplit(Sys.getenv("WEKAHOME"),split=";")))[[1]]
	
	classpath.vars <- unique(c(strsplit(Sys.getenv("classpath"),
	  split=";")))[[1]]
	
	r.path <- grep('R.exe',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)
	rJava.path <- grep('rJava.dll',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)
	pdftotext.path <- grep('pdftotext.exe',list.files(path = locs[1], 
	  all.files = T, full.names = TRUE, recursive = TRUE),value=TRUE)
	aspell.path <- grep('aspell.exe',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)
	java.path <-  grep('java.dll',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)[1]
	jvm.path <-  grep('jvm.dll',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)[1]
	weka.path <-  grep('weka.jar',list.files(path = locs[1], all.files = T, 
	  full.names = TRUE, recursive = TRUE),value=TRUE)[1]
	
	
	if(length(r.path) == 0){
		r.path <- grep('R.exe',list.files(path = locs[2], all.files = T, 
		  full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(rJava.path) == 0){
		rJava.path <- grep('rJava.dll',list.files(path = locs[2], all.files = T, 
		  full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(pdftotext.path) == 0){
		pdftotext.path <- grep('pdftotext.exe',list.files(path = locs[2], 
		  all.files = T, full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(aspell.path) == 0){
		aspell.path <- grep('aspell.exe',list.files(path = locs[2], 
		  all.files = T, full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(java.path) == 0){
		java.path <- grep('java.dll',list.files(path = locs[2], all.files = T, 
		  full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(jvm.path) == 0){
		jvm.path <- grep('jvm.dll',list.files(path = locs[2], all.files = T, 
		  full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(r.path) > 1){
		if(grepl("64$", sys["machine"])){
			r.path <- r.path[grepl("x64", r.path)]
		}else{
			r.path <- r.path[!grepl("x64", r.path)]
		}
	}
	
	if(length(rJava.path) > 1){
		if(grepl("64$", sys["machine"])){
			rJava.path <- rJava.path[grepl("x64", rJava.path)]
		}else{
			rJava.path <- rJava.path[!grepl("x64", rJava.path)]
		}
	}
	
	if(length(pdftotext.path) > 1){
		if(grepl("64$", sys["machine"])){
			pdftotext.path <- pdftotext.path[grepl("bin64", pdftotext.path)]
		}else{
			pdftotext.path <- pdftotext.path[!grepl("bin64", pdftotext.path)]
		}
	}
	
	r.path <- gsub("(?<=/)\\w+[.]\\w+$", "", r.path, perl = T)
	rJava.path <- gsub("(?<=/)\\w+[.]\\w+$", "", rJava.path, perl = T)
	pdftotext.path <- gsub("(?<=/)\\w+[.]\\w+$", "", pdftotext.path, perl = T)
	aspell.path <- gsub("(?<=/)\\w+[.]\\w+$", "", aspell.path, perl = T)
	java.path <-  gsub("(?<=/)\\w+[.]\\w+$", "", java.path, perl = T)
	jvm.path <-  gsub("(?<=/)\\w+[.]\\w+$", "", jvm.path, perl = T)
	weka.path <-  gsub("(?<=/)\\w+[.]\\w+$", "", weka.path, perl = T)
	
	Sys.setenv(PATH = paste(
	  unique(c(strsplit(Sys.getenv("PATH"),split=";")[[1]],
								r.path,
								rJava.path,
								java.path,
								jvm.path,
								pdftotext.path,
								aspell.path)),
							collapse = ";"), 
						JAVA_HOME = paste(r.path,
						 		rJava.path,
						 		java.path,
						 		jvm.path, 
						 	sep = ";"), 
						NOAWT = "TRUE",
						WEKAHOME = weka.path,
						CLASSPATH = weka.path)
}

PdfToText <- function(pdfloc, remove = T){
	
	pdfloc <- gsub("/$", "", pdfloc)
	
	sys <- Sys.info()
	
	root <- gsub("(?<=:/)[[:print:]]+","",getwd(),perl=T)
	
	locs <- grep("Program Files", list.dirs(root,recursive=F), value=T)
	
	pdftotext.path <- grep('pdftotext.exe',list.files(path = locs[1], 
	  all.files = T, full.names = TRUE, recursive = TRUE),value=TRUE)
	
	if(length(pdftotext.path) == 0){
		pdftotext.path <- grep('pdftotext.exe',list.files(path = locs[2], 
			all.files = T, full.names = TRUE, recursive = TRUE),value=TRUE)
	}
	
	if(length(pdftotext.path) > 1){
		if(grepl("64$", sys["machine"])){
			pdftotext.path <- pdftotext.path[grepl("bin64", pdftotext.path)]
		}else{
			pdftotext.path <- pdftotext.path[!grepl("bin64", pdftotext.path)]
		}
	}
	
	originalwd <- getwd()
		
	pdf.files <- list.files(pdfloc)
	pdf.files.clean <- gsub("\xad|\xe1|[^\x01-\x80]+", "", pdf.files)
	pdf.files.clean <- gsub("[^[:alpha:]. ]+", "", pdf.files.clean)
	pdf.files.clean <- tolower(pdf.files.clean)
	pdf.files.clean <- gsub("(?<=\\s)\\s+|^\\s+|\\s+(?=$)", "", 
							pdf.files.clean, perl = T)
	pdf.files.clean <- gsub("\\s+", "_", pdf.files.clean)
		
	file.rename(from = paste(pdfloc, "/", pdf.files, sep = ""), 
	            to = paste(pdfloc, "/", pdf.files.clean, sep = ""))
	
	file.copy(from = pdftotext.path, 
						to = paste(pdfloc,"/pdftotext.exe",sep=""))
	
	setwd(pdfloc)

	print("Converting PDFs to text files...")
	n <- length(pdf.files.clean)
	pb <- txtProgressBar(min = 1, max = n, style=3)
	
	for(i in 1:length(pdf.files.clean)){
		system(paste("pdftotext ", pdf.files.clean[i], sep = ""), intern = TRUE)
		setTxtProgressBar(pb, i)
	}
	
	file.remove(paste(pdfloc, "/pdftotext.exe", sep = ""))
	
	if(remove == T){
		file.remove(paste(pdfloc, "/", pdf.files.clean, sep = ""))
	}
	setwd(originalwd)
}

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
					out[!good & !missing] <- picked[match(out[!good & !missing],check$Original)]
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

CompleteStem <- function(texts, clean = T, stem.type="prevalent",stops=stopwords()){
	require(tm)

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
		
	restemmed <- laply(stemmed,function(x){

		leave <- (x %in% 
			aspell(as.factor(x), 
						 control = c("--master=en_US --sug-mode=ultra"))$Original)	
		
		if(!is.logical(leave)){
			leave <- 0
		}
		
		out <- x
		
		if(mean(leave > 0)){
			y <- x[leave]
			
			restem <- try(stemCompletion(y, sent.dict, 
																			type=stem.type), silent=T)
			if(class(restemmed) == "try-error"){
				restemmed <- y
			}
			out[leave] <- restem
		}
		paste(out, collapse = " ")}, .progress = "text")
	
	restemmed
}

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
					gregexpr(paste("(not|no)\\s(\\w+\\s)?",pos[i],"\\b"), vec))
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
					gregexpr(paste("(not|no)\\s(\\w+\\s)?",neg[i],"\\b"), vec))
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
