\name{splitWords}
\alias{splitWords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Split words into smaller chunks
}
\description{
  This is an ineternal function used to split unknown character strings into smaller pieces in an attempt to find recognizable words.
}
\usage{
  split_words(inp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inp}{
    A vector of individual words.
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  This function returns the input vector with each character string split into smallers words.
}

\author{
  Schaun Wheeler <schaun.wheeler@gmail.com>
}
\note{
  If a string is not made up of recognizable words, it will be split into small chunks of gibberish.
}


\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  \dontrun{
  }
}
