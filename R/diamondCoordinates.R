diamondCoordinates <- function(values, otherAxisValue = 1,
                                   direction = 'horizontal',
                                   autoSize=NULL, fixedSize=.25) {
  if (length(values) < 1) {
    stop("Specify at least two values!");
  }
  
  min <- min(values);
  max <- max(values);
  mid <- median(values);
  
  if (is.null(autoSize) && !is.null(fixedSize)) {
    size <- fixedSize;
  } else if (!is.null(autoSize)) {
    size <- (.5*autoSize) * (max-min);
  } else {
    size <- .25 * (max-min);
  }
  
  if (direction=='horizontal') {
    xValues <- c(min, mid, max, mid, min);
    yValues <- c(otherAxisValue, otherAxisValue - size, otherAxisValue, otherAxisValue + size, otherAxisValue);
  } else if (direction=='vertical') {
    xValues <- c(otherAxisValue, otherAxisValue - size, otherAxisValue, otherAxisValue + size, otherAxisValue);
    yValues <- c(min, mid, max, mid, min);
  }
  
  return(data.frame(x=xValues, y=yValues));
  
}
