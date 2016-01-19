\name{itemInspection}
\alias{itemInspection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
itemInspection
}
\description{
Function to generate a PDF with four panels per page, showing some basic item characteristics.
}
\usage{
itemInspection(dat, items,
               docTitle = "Scale inspection", docAuthor = "Author",
               pdfLaTexPath, rnwPath, filename="itemInspection",
               convertFactors = TRUE, digits=4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
  Dataframe containing the items of the relevant scale
}
  \item{items}{
  Either a character vector with the itemnames, or, if the items are organised in scales, a list of character vectors with the items in each scale.
}
  \item{docTitle}{
  Title to use when generating the PDF.
}
  \item{docAuthor}{
  Author(s) to include when generating the PDF.
}
  \item{pdfLaTexPath}{
  The path to PdfLaTex. This file is part of a LaTeX installation that creates a pdf out of a .tex file.

In Windows, you can download (portable) MikTex from
http://miktex.org/portable. You then decide yourself
where to install MikTex; pdflatex will end up in a
subfolder 'miktex\\bin', so if you installed MikTex
in, for example, 'C:\\Program Files\\MikTex', the total
path becomes 'C:\\Program Files\\MikTex\\miktex\\bin'. Note
that R uses slashes instead of backslashes to separate
folders, so in this example, pdfLaTexPath should be
'C:/Program Files/MikTex/miktex/bin'

In MacOS, you can install MacTex from http://tug.org/mactex/
By default, pdflatex ends up in folder '/user/texbin', which
is what pdfLaTexPath should be in that default case.

In Ubuntu, you can install TexLive base by using your package
manager to install texlive-latex-base, or using the terminal:
'sudo apt-get install texlive-latex-base'
In ubuntu, by default pdflatex ends un in folder '/usr/bin',
which is what pdfLaTexPath should be in that default case.
}
  \item{rnwPath}{
  The path where the temporary files and the resulting PDF should be stored.
}
  \item{filename}{
  The filename to use to save the pdf.
}
  \item{convertFactors}{
  Whether to convert factors to numeric vectors for the analysis.
}
  \item{digits}{
  The number of digits to use in the tables.
}
}
\value{

This function returns nothing; it just generates a PDF.

}
\examples{

\dontrun{
itemInspection(mtcars, items=c('disp', 'hp', 'drat'), pdfLaTexPath="valid/path/here");
}

}
\keyword{ utilities }
