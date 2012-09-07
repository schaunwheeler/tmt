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
# Packages Used:   tm, Matrix
# Output File:     
# Data Output:     
# Machine:         Schaun Wheeler's Dell Precision T7500 and MacBook Pro

# Copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: 
# http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# Load libraries
library(tm)

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

MakeWordLists <- functon(wlists = c("gi","afinn","liu")){
	
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
		
		liu.pos <- read.delim("https://github.com/schaunwheeler/tmt/blob/master/liu_positive_words.txt", 
								header=F, as.is=T, skip=35)$V1
		liu.neg <- read.delim("https://github.com/schaunwheeler/tmt/blob/master/liu_negative_words.txt", 
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
	
	pos.stem <- pos.stem[!(pos.stem %in% dupes | pos.stem %in% 
		c("exasper", "irrespons", "dumbfound"))]
	neg.stem <- neg.stem[!(neg.stem %in% dupes | neg.stem %in% 
		c("awe", "help", "bull****", "bull----", "d*mn", "f**k", "sh*t"))]
	
	pos <- unique(pos.stem)
	neg <- unique(neg.stem)
	
	pos <- gsub("[!#$%&'(),./;<=>?@^_`{|}~]+","[[:punct:]]*", pos)
	neg <- gsub("[!#$%&'(),./;<=>?@^_`{|}~]+","[[:punct:]]*", neg)
	
	pos <- gsub("a[+]\\b", "a[+]|a([-]|\\s)plus", pos)
	neg <- gsub("naïve", "naive", neg)
	
	pos <- gsub("[-]+","([[:punct:]]|\\s)*", pos, perl = T)
	neg <- gsub("[-]+","([[:punct:]]|\\s)*", neg, perl = T)																
	
	pos <- gsub("([[:alpha:]])\\1+","\\1{2,}", pos, perl=T)
	neg <- gsub("([[:alpha:]])\\1+","\\1{2,}", neg, perl=T)
	
	pos <- gsub("(?<=\\s)\\s+|\\s{2,}|\\s+(?=$)", " ", pos, perl = T)
	neg <- gsub("(?<=\\s)\\s+|\\s{2,}|\\s+(?=$)", " ", neg, perl = T)
	
	pos <- sort(unique(pos))
	neg <- sort(unique(neg))
	
  # De-conflict sentiment lists with stopwords
	print("De-conflicting sentiment words and stop words")

	neg <- neg[!(neg %in% "no")]
	
	pos.dups <- (duplicated(gsub("([[:punct:]]|s)*", "", pos, fixed = T)) |
		duplicated(gsub("([[:punct:]]|s)*", "", pos, fixed=T), fromLast = T)) &
		!grepl("([[:punct:]]|s)*", pos, fixed=T)
	
	neg.dups <- (duplicated(gsub("([[:punct:]]|s)*", "", neg, fixed = T)) |
		duplicated(gsub("([[:punct:]]|s)*", "", neg, fixed=T), fromLast = T)) &
		!grepl("([[:punct:]]|s)*", neg, fixed=T)
	
	pos <- pos[!pos.dups]
	neg <- neg[!neg.dups]

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
						ignore=NULL, split.missing = FALSE){
	
	input <- unlist(strsplit(input, split="\\s+"))
	
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
	
	check <- aspell(as.factor(x), 
					control = c("--master=en_US --sug-mode=fast"))
	
	if(nrow(check)==0){
		if(output == "eval"){
			out <- rep(TRUE,length(x))
		}
		if(output == "sugg"){
			out <- rep(NA,length(x))
		}
		if(output == "fix"){
			out <- paste(input, collapse = " ")
		}
	}else{
		
		pattern <- ifelse(sep, "^[a-z ]+$", "^[a-z]+$")
		good <- !(x %in% check$Original)
		missing <- x %in% check$Original[sapply(check$Suggestions,is.null)]
		xeval <- x[!good & !missing]
		
		if(output == "eval"){
			out <- good
		}
		if(output == "sugg"){
			out <- rep(NA,length(x))
			out[!good] <- check$Suggestions
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
					picked[i] <- check$Suggestions[!missing][[i]][ind[i]]}
				out <- x
				out[!good & !missing] <- picked
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

CompleteStem <- function(stemmed, unstemmed, stem.type="prevalent"){
	require(tm)
	
	stemmed <- strsplit(stemmed, split = " ")[[1]]
	
	sent.dict <- strsplit(paste(unstemmed, collapse=" "), split= " ")
	
	leave <- (stemmed %in% 
		aspell(as.factor(stemmed), 
					 control = c("--master=en_US --sug-mode=ultra"))$Original)
	
	if(!is.logical(leave)){
		leave <- 0
	}
	
	out <- stemmed
	
	if(mean(leave > 0)){
		x <- stemmed[leave]
		
		restemmed <- try(stemCompletion(x, Corpus(VectorSource(sent.dict)), 
										type=stem.type), silent=T)
		if(class(restemmed) == "try-error"){
			restemmed <- x
		}
		out[leave] <- restemmed
	}
	paste(out, collapse = " ")
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

PlotSentiment <- function(texts, scores, type = "both", n = 20, binary = TRUE, 
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
		"positive" = rowSums(tdm.sparse[,colnames(tdm.sparse) %in% posind]),
		"negative" = rowSums(tdm.sparse[,colnames(tdm.sparse) %in% negind])), 
					stringsAsFactors = FALSE)
	
	all.freq$frequency <- as.numeric(all.freq$frequency)
	all.freq$positive <- as.numeric(all.freq$positive)
	all.freq$negative <- as.numeric(all.freq$negative)
	
	all.freq <- all.freq[!(all.freq$positive == 0 & all.freq$negative == 0),]
	
	
	all.freq$difference <- (all.freq$positive-all.freq$negative)/
							all.freq$frequency
	
	all.freq$polarity <- (all.freq$positive+all.freq$negative)/
							all.freq$frequency
		
	final.freq <- arrange(all.freq,-frequency)[1:n,"terms"]
	final.pola <- arrange(all.freq,-polarity)[1:n,"terms"]
	
	if(type == "freq"){
		final.pola <- NULL
	}
	if(type == "pola"){
		final.freq <- NULL
	}
	
	final <- all.freq[all.freq$terms %in% unique(c(final.freq, final.pola)),] 
	
	final$occurrence <- cut(
		final$frequency,
		round(quantile(final$frequency,
									 probs=c(0, .2, .4, .6, .8, .99, 1)),
					digits=0),include.lowest = T)
	
	levels(final$occurrence) <- gsub("[,]", "-", levels(final$occurrence))
	levels(final$occurrence) <- gsub("[^[:digit:]-]", "", 
										levels(final$occurrence))
	
	
	final$terms <- reorder.factor(as.factor(final$terms), 
					new.order = arrange(final,polarity)[,"terms"])
	
	final$color <- rep(NA, nrow(final))
	final$color[final$terms %in% final.freq] <- "High Frequency"
	final$color[final$terms %in% final.pola] <- "High Polarity"
	final$color[(final$terms %in% final.pola) & 
	  (final$terms %in% final.freq)] <- "High Frequency and Polarity"
	
	n2 <- ifelse(type == "both",n*2,n)
	if(type == "both"){
		tagline <- "(frequency and polarity)"
	}else{
		if(type == "freq"){
			tagline <- "(frequency)"
		}else{
			tagline <- "(polarity)"
		}
	}
	
	ggplot(final, aes(x = difference, y = terms)) + 
		geom_point(aes(size = occurrence, colour = color)) + 
		scale_x_reverse("Sentiment", limits = c(1,-1), breaks = c(1,0,-1), 
		  labels=c("positive", "neutral", "negative")) +
		scale_size_discrete("# of\ndocuments",range=c(3,7)) + 
		theme_bw() + 
		ylab(paste("Top",n2,"terms",tagline,"\nin order of polarity")) + 
		opts(axis.title.x = theme_text(size=14, face="bold"), 
		  axis.title.y = theme_text(size=14, face="bold", angle=90),
		  axis.text.x = theme_text(size=12), 
		  axis.text.y = theme_text(size=12, hjust=1, vjust=.25),
		  legend.position="bottom", 
		  legend.key = theme_rect(colour = 'transparent'),
		  title = "Sentiment Keywords\n",
		  plot.title  = theme_text(size=16, face="bold"),
		  panel.border = theme_rect(colour = "transparent"),
		  axis.ticks = theme_segment(colour = "transparent"))
}
