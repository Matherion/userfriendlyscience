\name{rnwString}
\alias{rnwString}
\alias{hasLaTeX}
\alias{sanitizeLatexString}
\alias{rnwString.initiate}
\alias{rnwString.terminate}
\alias{rnwString.generate}
\title{
rnwString functions
}
\description{
The rnwString functions make knitting PFDs a bit more userfriendly.

The sanitizeLatexString function sanitizes a LaTeX string by escaping special characters. It is strongly based on the function described on http://stackoverflow.com/questions/5406071/r-sweave-latex-escape-variables-to-be-printed-in-latex by Aaron Rendahl.


}
\usage{
rnwString.initiate(studyName, authorName,
                   docClassArgs = 'a4paper,portrait,11pt',
                   newPage = TRUE, pageMargins=15)
rnwString.terminate(rnwString) 
rnwString.generate(rnwString, rnwPath, fileName, pdfLatexPath,
                   envir = parent.frame())
sanitizeLatexString(str)
hasLaTeX(pdfLatexPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{studyName}{
  The name of the study - used as the title of the PDF.
}
  \item{authorName}{
  The name of the author(s) - also inserted on title page of the PDF.
}
  \item{docClassArgs}{
  Default arguments for the document class in LaTeX. For example, to use landscape
pages, this should be 'a4paper,landscape,11pt'.
}
  \item{newPage}{
  Whether to end the initiation string with a newpage command. This can be
  set to false if you want to add more information on the first page(s).
}
  \item{pageMargins}{
  Margin of the pages in millimeters.
}
  \item{rnwString}{
  The rnwString to terminate or (after termination) generate.
}
  \item{rnwPath}{
  The path where the temporary files (.rnw, .tex, etc) should be created. Use forward slashes. Note: the last character should not be a slash!
}
  \item{fileName}{
  The filename to use for the temporary files. Omit the extension!
}
  \item{pdfLatexPath}{
  The path to PdfLaTex. This file is part of a LaTeX installation that creates a pdf out of a .tex file.

In Windows, you can download (portable) MikTex from
http://miktex.org/portable. You then decide yourself
where to install MikTex; pdflatex will end up in a
subfolder 'miktex\\bin', so if you installed MikTex
in, for example, 'C:\\Program Files\\MikTex', the total
path becomes 'C:\\Program Files\\MikTex\\miktex\\bin'. Note
that R uses slashes instead of backslashes to separate
folders, so in this example, pdfLatexPath should be
'C:/Program Files/MikTex/miktex/bin'

In MacOS, you can install MacTex from http://tug.org/mactex/
By default, pdflatex ends up in folder '/user/texbin', which
is what pdfLatexPath should be in that default case.

In Ubuntu, you can install TexLive base by using your package
manager to install texlive-latex-base, or using the terminal:
'sudo apt-get install texlive-latex-base'
In ubuntu, by default pdflatex ends un in folder '/usr/bin',
which is what pdfLatexPath should be in that default case.
}
  \item{envir}{
  The environment where to evaluate the expressions (normally the environment
  where the function is called).
}
  \item{str}{
The character string to sanitize.
}
}
\value{

rnwString.initiate starts an rnwString; rnwString.terminate closes it; and rnwString.generate takes an rnwString and creates a pdf.

sanitizeLatexString returns the sanitized string.

hasLaTeX checks pdfLatexPath to make sure pdflatex or pdflatex.exe exists.

}
\examples{

### sanitize a string
newString <- sanitizeLatexString('this is a tilde: ~.');
newString;
### newString is now: "this is a tilde: \\~."

}
\keyword{ utilities }
