\name{AspellCheck}
\alias{AspellCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Automatically check or correct spelling in a vector of texts
}
\description{
  This function provides a user-friendly interface to `aspell`.
}
\usage{
  AspellCheck(input, output = "eval", sep = FALSE, keep_caps = TRUE, 
  pattern_flag = NULL, word_flag = NULL, split_missing = FALSE, mode = "ultra", 
  dict = "en_US")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
  A character vector. White space is used to delineate words.
}
  \item{output}{
  One of three values: "eval" returns a logical vector indicating whether each word was found in the dictionary; "sugg" returns a list where each misspelled word is given all suggested alternatives, and where each correctly spelled word is given NA; "fix" replaces each misspelled word with the word suggested as its most likely alternative. Proper nouns are not considered viable alternatives.
}
  \item{sep}{
  A logical value that specifies whether two separate words should be considered a viable alternative to a single misspelled word
}
  \item{keep_caps}{
  A logical vector indicating whether words capitalized in the input should remain capitalized in the output. Only has an effect when output = "eval".
}
  \item{pattern_flag}{
  If not NULL (the default), a regular expression to flag words that should not be evaluated or corrected.
}
  \item{word_flag}{
  If not NULL (the default), a vector of words that should not be evaluated or corrected.
}
  \item{split_missing}{
  A logical value. When set to TRUE (the default), each mispelled word for which a viable alternative cannot be found is split into smaller words which are then evaluated separately.
}
  \item{mode}{
  A character vector passed to `aspell` indicating the speed and detail of the spell checker. Details can be found at http://aspell.net/man-html/Notes-on-the-Different-Suggestion-Modes.html
}
  \item{dict}{
  A character vector passed to `aspell` indicating which dictionary to use. Details can be found at http://aspell.net/0.50-doc/man-html/5_Working.html, and a list of available dictionaries can be found at ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  This function returns a vector of logical values when output = "eval", a list of lists when output = "sugg" and a character vector when output = "fix".
}

\author{
Schaun Wheeler <schaun.wheeler@gmail.com>
}
\note{
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  \dontrun{
  data(holbrook)
  
  AspellCheck(holbrook, "eval")
  AspellCheck(holbrook, "sugg")
  AspellCheck(holbrook, "fix")
  }
}
