\name{userfriendlyscienceBasics}
\alias{userfriendlyscienceBasics}
\alias{safeRequire}
\alias{trim}
\alias{noZero}
\alias{formatPvalue}
\alias{formatR}
\alias{repeatStr}
\alias{ifelseObj}
\alias{invertItem}
\alias{is.odd}
\alias{is.even}
\alias{convertToNumeric}
\alias{massConvertToNumeric}
\alias{vecTxt}
\alias{\%IN\%}
\alias{cat0}
\title{
userfriendlyscience basics
}
\description{
The userfriendlyscience basics functions are some very basic functions to make life that little bit easier.
}
\usage{
safeRequire(packageName, mirrorIndex=NULL)
trim(str)
noZero(str)
formatPvalue(values, digits = 3, spaces=TRUE, includeP = TRUE)
formatR(r, digits)
repeatStr(str = " ", n = 1)
ifelseObj(condition, ifTrue, ifFalse)
invertItem(item, range=NULL, ignorePreviousInversion = FALSE)
is.odd(vector)
is.even(vector)
convertToNumeric(vector, byFactorLabel = FALSE)
massConvertToNumeric(dat, byFactorLabel = FALSE, ignoreCharacter = TRUE)
vecTxt(vector, delimiter = ", ", firstDelimiter = NULL, lastDelimiter = " & ",
       firstElements = 0, lastElements = 1, lastHasPrecedence = TRUE)
find \%IN\% table
cat0(..., sep="")
}
\arguments{
  \item{packageName}{
The name of the package, as character string.
}
  \item{mirrorIndex}{
The index of the mirror to use, in case you want to specify the mirror
in the call (see e.g. /code{/link{getCRANmirrors}()[, 1:4]} for an overview
of these mirrors. For example, at the time of writing, Antwerp is 7,
Amsterdam is 60, and Auckland is 62).
}
  \item{str}{
The character string to process.
}
  \item{values}{
The p-values to format.
}
  \item{digits}{
For formatPvalue, number of digits to round to. Numbers smaller than this
number will be shown as <.001 or <.0001 etc.

For formatR, the number of digits to use when formatting the
Pearson correlation.
}
  \item{spaces}{
Whether to include spaces between symbols, operators, and digits.
}
  \item{includeP}{
Whether to include the 'p' and '='-symbol in the results (the '<' symbol
is always included).
}
  \item{r}{
The Pearson correlation to format.
}
  \item{n}{
The number of times to repeat the string.
}
  \item{condition}{
  Condition to evaluate.
}
  \item{ifTrue}{
  Object to return if the condition is true.
}
  \item{ifFalse}{
  Object to return if the condition is false.
}
  \item{item}{
  Item to invert
}
  \item{range}{
  If provided it must be a numeric vector with the minimum and the maximum of the scale. If not provided, the range function is used (so, use this range argument if the scale minimum and/or maximum do not occur in the data).
}
  \item{ignorePreviousInversion}{
  If this item has already been inverted, the function will halt with an error unless it's told to ignore previous inversions with this boolean.
}
  \item{dat, vector}{
  The dataframe of vector to process.
}
  \item{byFactorLabel}{
  If TRUE, convertToNumeric and massConvertToNumeric use the factor labels, interpreted as character vectors, to determine the numeric value, instead of the level's indices (which is what as.numeric() does).
}
  \item{ignoreCharacter}{
  If TRUE, character vectors are ignored. If FALSE, character vectors are converted (or, an attempt is made :-)).
  }
  \item{find}{
  The element(s) to look up in the vector or matrix.
  }
  \item{table}{
  The vector or matrix in which to look up the element.
  }
  \item{delimiter, firstDelimiter, lastDelimiter}{
  The delimiters to use for respectively the middle, first
  \code{firstElements}, and last \code{lastElements} elements.
  }
  \item{firstElements, lastElements}{
  The number of elements for which to use the first respective last delimiters
  }
  \item{lastHasPrecedence}{
  If the vector is very short, it's possible that the sum of firstElements
  and lastElements is larger than the vector length. In that case, downwardly
  adjust the number of elements to separate with the first delimiter 
  (\code{TRUE}) or the number of elements to separate with the last
  delimiter (\code{FALSE})?
  }
  \item{sep}{
  The separator to pass to \code{\link{cat}}, of course, "" by default.
}
  \item{...}{
  Extra arguments are passed to whatever function is wrapped (e.g.
  \code{\link{cat}} for \code{cat0}).
}
}
\details{
The safeRequire function checks whether a package is already installed. If so, it loads the package (using require/library). If not, it first installs it, and then loads it.

The trim function removes whitespaces from the start and end of a text string.

The noZero function removes the first zero from a string that was originally a number.

The formatPvalue function formats a P value, roughly according to APA
style guidelines. This means that the noZero is used to remove the zero
preceding the decimal point, and p values that would round to zero given
the requested number of digits are shown as e.g. p<.001.

The formatR function format a Pearson correlation for pretty printing (using noZero).

The repeatStr function repeats a string a given number of times.

The ifelseObj function just evaluates a condition, returning one object if it's true, and another if it's false.

The invertItem function 'unmirrors' an inverted item (i.e. for a 1-3 item, 1 becomes 3, 2 stays 2, and 3 becomes 1).

is.odd and is.even check whether a number is, or numbers in a vector are, odd or even.

The infix function \%IN\% is a case-insensitive version of \code{\link{\%in\%}}.

The cat0 function is to cat what paste0 is to paste; it simply makes 
concatenating many strings without a separator easier.

}
\value{
safeRequire returns nothing.

trim, formatPvalue, noZero, formatR, and repeatStr return a string.

ifelseObj return an object.

The invertItem function returns the inverted item vector, with an attribute "inverted" set to TRUE.

is.odd and is.even return a logical vector.

\%IN\% returns a logical vector of the same length as its first argument.

cat0 returns a string.

}
\examples{

### load a package
safeRequire('ggplot2');

### trim a string
trim(' this is a string with whitespace in front and at the end               ');
### Returns "this is a string with whitespace in front and at the end"

repeatStr("-", 8);
### Returns "--------" (incredibly useful, no? :-))

tempVector <- c(1,2,3,3,2,4,3,2,1,1,3,4,5,4,3,2,2,1,1,2);
invertedTempVector <- invertItem(tempVector);

### We can also invert it back, but then we have to override the security
### that prevents accidently inverting items back.
invertItem(tempVector, ignorePreviousInversion=TRUE);

}
\keyword{ utilities }
