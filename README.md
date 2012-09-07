# tmt

The aim of `tmt` is to provide some added functionality to the tm package by 
facilitating the cleaning and analysis of messy text data. You can track and 
contribute to development of `tmt` at https://github.com/schaunwheeler/tmt.

## Package text manipulation tools

Housecleaning:

* `SetEnvVarW` searches for external applications necessary to run various text
  cleaning and analysis functions. It only works on Windows and should be used
  with extreme caution, as it has not been extensively tested.
  
* NOTE: if switching between a Windows operating system and some other 
  operating system, text encoding can get confused. Use the following to avoid 
  some of that confusion:
  
    Sys.setlocale('LC_ALL', 'C')
  
Preparation:

* `MakeWordLists` take lists of sentiment-laden words from General Inquirer
  (http://www.wjh.harvard.edu/~inquirer/), the list compiled by Finn Årup 
  (http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010), or the 
  list compiled by Bing Liu (http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.
  html#lexicon) - or from all three lists (the default). More information about
  these lists and other can be found at http://sentiment.christopherpotts.net/
  lexicons.html. The function combines and de-dupes the lists, preprocesses
  the words to make them compatible with use a regular expression patterns, and
  deconflicts the sentiment words with the list of common English stop words
  from the tm package. It returns a list with three elements - 'pos', 'neg', 
  and 'stops'.

* `CleanText` is a general set of text-cleaning rules. It changes everything to
  lower case, removes numbers and punctuation except in contractions, ensures 
  that most contractions with omitted apostrophes get those apostrophes 
  inserted, and removes specified stop words. A 'neutralize' option replaces all
  empty strings with the word 'neutral' (for purposes of the sentiment-analysis
  function in this package). A vector of words to remove can also be passed to
  the function.
  
* `PdfToText` converts PDF files to text files. It's argument, 'pdfloc' is the 
  path of the folder in which the PDFs to convert are located. As a default, it
  removes the PDF versions of the files after they have been converted to text.

Spelling Correction:
  
* `AspellCheck` is a wrapper for the aspell() function in the R utils package.
  It takes as input a single character string. The output has three
  modes. "eval" returns a logical vector indicating whether each word (words are 
  delineated by single spaces) was found in the dictionary. "sugg" returns a 
  list where each misspelled word is given all suggested alternatives, and where
  each correctly spelled word is given NA. "fix" replaces each misspelled word
  with the word suggested as its most likely alternative. Proper nouns are not
  considered viable alternatives. The 'sep' option take a logical value and 
  specifies whether two separate words should be considered a viable alternative
  to a misspelled word. 'cap.flag' takes one of three values: 'none' (the 
  default) does nothing; 'first' tells the function to ignore all words that 
  start with a capital letter; "all" tells the function to ignore all words that
  are entirely composed of capital letters. This allows subject-specific words 
  to survive the spell check. The 'ignore' option takes a character vector and,
  like cap.flag, give the function a list of words to ignore. 'split.missing' 
  takes a logical value. When set to TRUE, it makes a call to SplitWords() 
  function in this package in each case where a word is not found in the
  dictionary and a viable alternative cannot be found.

* `SplitWords` takes a character vector, splits it into single words (delineated
  by spaces), keeps all correctly spelled words the same, and splits all 
  misspelled words into multiple words. It does this by splitting the original 
  word into each possible combination of two words, taking the longest first-word
  option that is recognized in the dictionary, and then repeating the process 
  for the remaining characters. A 'correction' option take a logical value. If 
  TRUE, once a word is split, it checks the remaining characters against the
  dictionary and, if misspelled but having at least one viable alternative, that
  alternative is used instead of continuing the splitting.
  
* NOTE: The AspellCheck() and SplitWords() functions requires that aspell be    
  installed and on the PATH environmental variable. On Windows, download aspell 
  and dictionaries from http://aspell.net/win32/, and set the environmental PATH 
  variable to include aspell, possibly using the SetEnvVarW() function in this 
  package. On a Mac, download Cocoaspell from http://cocoaspell.leuski.net/, 
  then go into the folder where the dictionaries are kept (probably library/
  Application/Support/cocoAspell/aspell6-en-6.0-0/) and copy all of the 
  dictionary files. Then open the Finder, click on Go>Go to Folder, type "/usr", 
  then navigate to /usr/loca/lib/aspell-0.60/ and paste them all in that 
  directory.

* `CompleteStem` combined aspell() with the tm package's stemCompletion()
  function. Given a vector of sample text, it compiles that text into a 
  dictionary. It then evaluates each word from a character string of stemmed
  (as well as unstemmed, if you like) words using aspell(). If a word is found
  in the dictionary, the function keeps it. If the word is not found in the 
  dictionary, the function calls stemCompletion to complete it.

Basic Sentiment Analysis:

* `GetSentiment` takes a vector of texts, a list of positive-sentiment words, 
  and a list of negative-sentiment words, and uses regular-expressions to score
  each text. It returns a data frame of scores for each text: 'words' gives the 
  number of words in each text; 'positives' and 'negatives' give the number of 
  positive or negative words, respectively, in each text; 'polarity' gives the 
  differences between the number of positive and number of negative words in 
  each text, divided by the total number of sentiment-laden (positive or 
  negative) words; 'subjectivity' gives the total number of sentiment-laden 
  words divided by the total number of words; 'positivity' and 'negativity' give
  the total of positive or negative words, respectively, divided by the total 
  number of words; 'balance' gives the number of positive words minus the number
  of negative words, divided by the total number of words.

* `PlotSentiment` takes a vector of texts that have already been scored (and 
  assumes the scores run from negative to positive with zero as a neutral point)
  and plots the top n words, as measured either by frequency or polarity. The 
  default is to use binary measurement - to count a word occurrence only once per
  text. If binary is set to FALSE, then the results will reflect total word
  occurrences within the corpus rather than within individual texts.