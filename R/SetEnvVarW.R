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
      collapse = ";"), 
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
