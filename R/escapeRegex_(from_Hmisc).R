### Taken directly from Hmisc (to avoid importing the package for just this function)
escapeRegex <- function (string)
{
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}
