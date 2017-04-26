### http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation

areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}
