\name{AspellCheck}
\alias{AspellCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Spell check a vector of texts
}
\description{
  This function provides a user-friendly interface to `aspell`.
}
\usage{
  AspellCheck(input, output = "eval", sep = FALSE, cap.flag = "none", 
  ignore=NULL, split.missing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
  A vector of character strings.
}
  \item{output}{
  One of three values: "eval" returns a logical vector indicating whether each word (words are delineated by single spaces) was found in the dictionary. "sugg" returns a list where each misspelled word is given all suggested alternatives, and where each correctly spelled word is given NA. "fix" replaces each misspelled word with the word suggested as its most likely alternative. Proper nouns are not considered viable alternatives.
}
  \item{sep}{
  A logical value that specifies whether two separate words should be considered a viable alternative to a misspelled word
}
  \item{cap.flag}{
  One of three values: 'none' (the default) does nothing; 'first' tells the function to ignore all words that start with a capital letter; "all" tells the function to ignore all words that are entirely composed of capital letters.
}
  \item{ignore}{
  A character vector which, similar to cap.flag, gives the function a list of words to ignore.
}
  \item{split.missing}{
  A logical value. When set to TRUE, it makes a call to SplitWords() 
  function in this package in each case where a word is not found in the
  dictionary and a viable alternative cannot be found.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  This function returns a vector of logical values when output = "eval", a list of lists when output = "sugg" and a character vector when output = "fix".
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  \dontrun{
  }
}
