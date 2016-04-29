### From http://stackoverflow.com/questions/28562288/how-to-use-the-hsl-hue-saturation-lightness-cylindric-color-model
hsl <- function(h, s, l) {
  h <- h / 360;
  r <- g <- b <- 0.0;
  if (s == 0) {
    r <- g <- b <- l;
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p);
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s));
    p <- 2.0 * l - q;
    r <- hue_to_rgb(p, q, h + 1/3);
    g <- hue_to_rgb(p, q, h);
    b <- hue_to_rgb(p, q, h - 1/3);
  }
  return(rgb(r,g,b));
}
