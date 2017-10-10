from_epiR_epi.2by2 <- function (dat, method = "cohort.count", conf.level = 0.95, units = 100, 
                                homogeneity = "breslow.day", outcome = "as.columns") 
{
  if (outcome == "as.columns") {
    dat <- dat
  }
  if (outcome == "as.rows") {
    dat <- t(dat)
  }
  if (length(dim(dat)) == 2) {
    a <- dat[1]
    A <- a
    b <- dat[3]
    B <- b
    c <- dat[2]
    C <- c
    d <- dat[4]
    D <- d
  }
  if (length(dim(dat)) > 2) {
    a <- dat[1, 1, ]
    A <- a
    b <- dat[1, 2, ]
    B <- b
    c <- dat[2, 1, ]
    C <- c
    d <- dat[2, 2, ]
    D <- d
  }
  .funincrisk <- function(dat, conf.level) {
    alpha <- 1 - conf.level
    alpha2 <- 0.5 * alpha
    x <- dat[, 1]
    n <- dat[, 2]
    p <- x/n
    x1 <- x == 0
    x2 <- x == n
    lb <- ub <- x
    lb[x1] <- 1
    ub[x2] <- n[x2] - 1
    lcl <- 1 - qbeta(1 - alpha2, n + 1 - x, lb)
    ucl <- 1 - qbeta(alpha2, n - ub, x + 1)
    if (any(x1)) 
      lcl[x1] <- rep(0, sum(x1))
    if (any(x2)) 
      ucl[x2] <- rep(1, sum(x2))
    rval <- data.frame(est = p, lower = lcl, upper = ucl)
    rval
  }
  .funincrate <- function(dat, conf.level) {
    N. <- 1 - ((1 - conf.level)/2)
    a <- dat[, 1]
    n <- dat[, 2]
    p <- a/n
    low <- 0.5 * qchisq(p = N., df = 2 * a + 2, lower.tail = FALSE)/n
    up <- 0.5 * qchisq(p = 1 - N., df = 2 * a + 2, lower.tail = FALSE)/n
    rval <- data.frame(p, low, up)
    names(rval) <- c("est", "lower", "upper")
    rval
  }
  .funRRwald <- function(dat, conf.level) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    wRR.p <- (a/N1)/(c/N0)
    lnwRR <- log(wRR.p)
    lnwRR.var <- (1/a) - (1/N1) + (1/c) - (1/N0)
    lnwRR.se <- sqrt((1/a) - (1/N1) + (1/c) - (1/N0))
    wRR.se <- exp(lnwRR.se)
    ll <- exp(lnwRR - (z * lnwRR.se))
    ul <- exp(lnwRR + (z * lnwRR.se))
    c(wRR.p, ll, ul)
  }
  .funRRscore <- function(dat, conf.level) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    scRR.p <- (a/N1)/(c/N0)
    if ((c == 0) && (a == 0)) {
      ul = Inf
      ll = 0
    }
    else {
      a1 = N0 * (N0 * (N0 + N1) * a + N1 * (N0 + a) * (z^2))
      a2 = -N0 * (N0 * N1 * (c + a) + 2 * (N0 + N1) * c * 
                    a + N1 * (N0 + c + 2 * a) * (z^2))
      a3 = 2 * N0 * N1 * c * (c + a) + (N0 + N1) * (c^2) * 
        a + N0 * N1 * (c + a) * (z^2)
      a4 = -N1 * (c^2) * (c + a)
      b1 = a2/a1
      b2 = a3/a1
      b3 = a4/a1
      c1 = b2 - (b1^2)/3
      c2 = b3 - b1 * b2/3 + 2 * (b1^3)/27
      ceta = acos(sqrt(27) * c2/(2 * c1 * sqrt(-c1)))
      t1 = -2 * sqrt(-c1/3) * cos(pi/3 - ceta/3)
      t2 = -2 * sqrt(-c1/3) * cos(pi/3 + ceta/3)
      t3 = 2 * sqrt(-c1/3) * cos(ceta/3)
      p01 = t1 - b1/3
      p02 = t2 - b1/3
      p03 = t3 - b1/3
      p0sum = p01 + p02 + p03
      p0up = min(p01, p02, p03)
      p0low = p0sum - p0up - max(p01, p02, p03)
      if ((c == 0) && (a != 0)) {
        ll = (1 - (N1 - a) * (1 - p0low)/(c + N1 - (N0 + 
                                                      N1) * p0low))/p0low
        ul = Inf
      }
      else if ((c != N0) && (a == 0)) {
        ul = (1 - (N1 - a) * (1 - p0up)/(c + N1 - (N0 + 
                                                     N1) * p0up))/p0up
        ll = 0
      }
      else if ((c == N0) && (a == N1)) {
        ul = (N0 + z^2)/N0
        ll = N1/(N1 + z^2)
      }
      else if ((a == N1) || (c == N0)) {
        if ((c == N0) && (a == 0)) {
          ll = 0
        }
        if ((c == N0) && (a != 0)) {
          phat1 = c/N0
          phat2 = a/N1
          phihat = phat2/phat1
          phil = 0.95 * phihat
          chi2 = 0
          while (chi2 <= z) {
            a = (N0 + N1) * phil
            b = -((c + N1) * phil + a + N0)
            c = c + a
            p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * 
                                                    a)
            p2hat = p1hat * phil
            q2hat = 1 - p2hat
            var = (N0 * N1 * p2hat)/(N1 * (phil - p2hat) + 
                                       N0 * q2hat)
            chi2 = ((a - N1 * p2hat)/q2hat)/sqrt(var)
            ll = phil
            phil = ll/1.0001
          }
        }
        i = c
        j = a
        ni = N0
        nj = N1
        if (a == N1) {
          i = a
          j = c
          ni = N1
          nj = N0
        }
        phat1 = i/ni
        phat2 = j/nj
        phihat = phat2/phat1
        phiu = 1.1 * phihat
        if ((c == N0) && (a == 0)) {
          if (N0 < 100) {
            phiu = 0.01
          }
          else {
            phiu = 0.001
          }
        }
        chi1 = 0
        while (chi1 >= -z) {
          a. = (ni + nj) * phiu
          b. = -((i + nj) * phiu + j + ni)
          c. = i + j
          p1hat = (-b. - sqrt(b.^2 - 4 * a. * c.))/(2 * 
                                                      a.)
          p2hat = p1hat * phiu
          q2hat = 1 - p2hat
          var = (ni * nj * p2hat)/(nj * (phiu - p2hat) + 
                                     ni * q2hat)
          chi1 = ((j - nj * p2hat)/q2hat)/sqrt(var)
          phiu1 = phiu
          phiu = 1.0001 * phiu1
        }
        if (a == N1) {
          ul = (1 - (N1 - a) * (1 - p0up)/(c + N1 - (N0 + 
                                                       N1) * p0up))/p0up
          ll = 1/phiu1
        }
        else {
          ul = phiu1
        }
      }
      else {
        ul = (1 - (N1 - a) * (1 - p0up)/(c + N1 - (N0 + 
                                                     N1) * p0up))/p0up
        ll = (1 - (N1 - a) * (1 - p0low)/(c + N1 - (N0 + 
                                                      N1) * p0low))/p0low
      }
    }
    c(scRR.p, ll, ul)
  }
  .funORwald <- function(dat, conf.level) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    wOR.p <- (a/b)/(c/d)
    lnwOR <- log(wOR.p)
    lnwOR.var <- 1/a + 1/b + 1/c + 1/d
    lnwOR.se <- sqrt(lnwOR.var)
    ll <- exp(lnwOR - (z * lnwOR.se))
    ul <- exp(lnwOR + (z * lnwOR.se))
    c(wOR.p, ll, ul)
  }
  .funORcfield <- function(dat, conf.level, interval = c(1e-08, 
                                                         1e+08)) {
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    cfOR.p <- (a/b)/(c/d)
    if (((a == 0) && (c == 0)) || ((a == N1) && (c == N0))) {
      ll <- 0
      ul <- Inf
    }
    else if (c == N0 || a == 0) {
      ll <- 0
      ul <- uniroot(function(or) {
        sum(sapply(max(0, a + c - N0):a, dFNCHypergeo, 
                   N1, N0, a + c, or)) - dFNCHypergeo(a, N1, N0, 
                                                      a + c, or)/2 - (1 - conf.level)/2
      }, interval = interval)$root
    }
    else if (a == N1 || c == 0) {
      ll <- uniroot(function(or) {
        sum(sapply(a:min(N1, a + c), dFNCHypergeo, N1, 
                   N0, a + c, or)) - dFNCHypergeo(a, N1, N0, a + 
                                                    c, or)/2 - (1 - conf.level)/2
      }, interval = interval)$root
      ul <- Inf
    }
    else {
      ll <- uniroot(function(or) {
        sum(sapply(a:min(N1, a + c), dFNCHypergeo, N1, 
                   N0, a + c, or)) - dFNCHypergeo(a, N1, N0, a + 
                                                    c, or)/2 - (1 - conf.level)/2
      }, interval = interval)$root
      ul <- uniroot(function(or) {
        sum(sapply(max(0, a + c - N0):a, dFNCHypergeo, 
                   N1, N0, a + c, or)) - dFNCHypergeo(a, N1, N0, 
                                                      a + c, or)/2 - (1 - conf.level)/2
      }, interval = interval)$root
    }
    c(cfOR.p, ll, ul)
  }
  .limit <- function(x1, n1, x2, n2, conf.level, lim, t) {
    z = qchisq(conf.level, 1)
    px = x1/n1
    score <- 1:1000
    score = 0
    repeat {
      a. = n2 * (lim - 1)
      b. = n1 * lim + n2 - (x1 + x2) * (lim - 1)
      c. = -(x1 + x2)
      p2d = (-b. + sqrt(b.^2 - 4 * a. * c.))/(2 * a.)
      p1d = p2d * lim/(1 + p2d * (lim - 1))
      score = ((n1 * (px - p1d))^2) * (1/(n1 * p1d * (1 - 
                                                        p1d)) + 1/(n2 * p2d * (1 - p2d)))
      ci = lim
      if (t == 0) {
        lim = ci/1.001
      }
      else {
        lim = ci * 1.001
      }
      if (score > z) {
        break
      }
    }
    return(ci)
  }
  .funORscore <- function(dat, conf.level) {
    x1 <- dat[1]
    n1 <- dat[1] + dat[3]
    x2 <- dat[2]
    n2 <- dat[2] + dat[4]
    px = x1/n1
    py = x2/n2
    scOR.p <- (dat[1]/dat[3])/(dat[2]/dat[4])
    if (((x1 == 0) && (x2 == 0)) || ((x1 == n1) && (x2 == 
                                                    n2))) {
      ul = 1/0
      ll = 0
    }
    else if ((x1 == 0) || (x2 == n2)) {
      ll = 0
      theta = 0.01/n2
      ul = .limit(x1, n1, x2, n2, conf.level, theta, 1)
    }
    else if ((x1 == n1) || (x2 == 0)) {
      ul = 1/0
      theta = 100 * n1
      ll = .limit(x1, n1, x2, n2, conf.level, theta, 0)
    }
    else {
      theta = px/(1 - px)/(py/(1 - py))/1.1
      ll = .limit(x1, n1, x2, n2, conf.level, theta, 0)
      theta = px/(1 - px)/(py/(1 - py)) * 1.1
      ul = .limit(x1, n1, x2, n2, conf.level, theta, 1)
    }
    c(scOR.p, ll, ul)
  }
  .funORml <- function(dat, conf.level) {
    mOR.tmp <- fisher.test(dat, conf.int = TRUE, conf.level = conf.level)
    mOR.p <- as.numeric(mOR.tmp$estimate)
    mOR.l <- as.numeric(mOR.tmp$conf.int)[1]
    mOR.u <- as.numeric(mOR.tmp$conf.int)[2]
    c(mOR.p, mOR.l, mOR.u)
  }
  .funARwald <- function(dat, conf.level, units) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    wARisk.p <- ((a/N1) - (c/N0))
    wARisk.se <- (sqrt(((a * (N1 - a))/N1^3) + ((c * (N0 - 
                                                        c))/N0^3)))
    ll <- (wARisk.p - (z * wARisk.se))
    ul <- (wARisk.p + (z * wARisk.se))
    c(wARisk.p * units, ll * units, ul * units)
  }
  .funARscore <- function(dat, conf.level, units) {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    N1 <- a + b
    N0 <- c + d
    sARisk.p <- ((a/N1) - (c/N0))
    px = a/N1
    py = c/N0
    z = qchisq(conf.level, 1)
    proot = px - py
    dp = 1 - proot
    niter = 1
    while (niter <= 50) {
      dp = 0.5 * dp
      up2 = proot + dp
      score = .z2stat(px, N1, py, N0, up2)
      if (score < z) {
        proot = up2
      }
      niter = niter + 1
      if ((dp < 1e-07) || (abs(z - score) < 1e-06)) {
        niter = 51
        ul = up2
      }
    }
    proot = px - py
    dp = 1 + proot
    niter = 1
    while (niter <= 50) {
      dp = 0.5 * dp
      low2 = proot - dp
      score = .z2stat(px, N1, py, N0, low2)
      if (score < z) {
        proot = low2
      }
      niter = niter + 1
      if ((dp < 1e-07) || (abs(z - score) < 1e-06)) {
        ll = low2
        niter = 51
      }
    }
    c(sARisk.p * units, ll * units, ul * units)
  }
  .z2stat <- function(p1x, nx, p1y, ny, dif) {
    diff = p1x - p1y - dif
    if (abs(diff) == 0) {
      fmdiff = 0
    }
    else {
      t = ny/nx
      a = 1 + t
      b = -(1 + t + p1x + t * p1y + dif * (t + 2))
      c = dif * dif + dif * (2 * p1x + t + 1) + p1x + t * 
        p1y
      d = -p1x * dif * (1 + dif)
      v = (b/a/3)^3 - b * c/(6 * a * a) + d/a/2
      s = sqrt((b/a/3)^2 - c/a/3)
      if (v > 0) {
        u = s
      }
      else {
        u = -s
      }
      w = (3.141592654 + acos(v/u^3))/3
      p1d = 2 * u * cos(w) - b/a/3
      p2d = p1d - dif
      var = p1d * (1 - p1d)/nx + p2d * (1 - p2d)/ny
      fmdiff = diff^2/var
    }
    return(fmdiff)
  }
  .funMHRD.Sato0 <- function(dat, conf.level = 0.95, units = units) {
    if (length(dim(dat)) > 2) {
      ndat <- addmargins(A = dat, margin = 2, FUN = sum, 
                         quiet = FALSE)
      c1 <- ndat[1, 1, ]
      c2 <- ndat[1, 3, ]
      c3 <- ndat[2, 1, ]
      c4 <- ndat[2, 3, ]
      dataset <- cbind(c1, c2, c3, c4)
      num <- sum(apply(X = dataset, MARGIN = 1, FUN = function(ro) (ro[1] * 
                                                                      ro[4] - ro[3] * ro[2])/(ro[2] + ro[4])))
      W <- sum(apply(dataset, 1, function(ro) ro[2] * ro[4]/(ro[2] + 
                                                               ro[4])))
      delta.MH <- num/W
      P <- sum(apply(dataset, 1, function(ro) (ro[2]^2 * 
                                                 ro[3] - ro[4]^2 * ro[1] + 0.5 * ro[2] * ro[4] * 
                                                 (ro[4] - ro[2]))/(ro[2] + ro[4])^2))
      Q <- sum(apply(dataset, 1, function(ro) (ro[1] * 
                                                 (ro[4] - ro[3]) + ro[3] * (ro[2] - ro[1]))/(2 * 
                                                                                               (ro[2] + ro[4]))))
      delta.Mid <- delta.MH + 0.5 * qchisq(conf.level, 
                                           df = 1) * (P/W^2)
      ME <- sqrt(delta.Mid^2 - delta.MH^2 + qchisq(conf.level, 
                                                   df = 1) * Q/W^2)
      CI <- delta.Mid + cbind(-1, 1) * ME
      Sato0ARisk.p <- delta.Mid
      Sato0ARisk.l <- Sato0ARisk.p - ME
      Sato0ARisk.u <- Sato0ARisk.p + ME
      c(Sato0ARisk.p * units, Sato0ARisk.l * units, Sato0ARisk.u * 
          units)
    }
  }
  .funMHRD.Sato <- function(dat, conf.level = 0.95, units = units) {
    if (length(dim(dat)) > 2) {
      ndat <- addmargins(A = dat, margin = 2, FUN = sum, 
                         quiet = FALSE)
      c1 <- ndat[1, 1, ]
      c2 <- ndat[1, 3, ]
      c3 <- ndat[2, 1, ]
      c4 <- ndat[2, 3, ]
      dataset <- cbind(c1, c2, c3, c4)
      num <- sum(apply(X = dataset, MARGIN = 1, FUN = function(ro) (ro[1] * 
                                                                      ro[4] - ro[3] * ro[2])/(ro[2] + ro[4])))
      W <- sum(apply(dataset, 1, function(ro) ro[2] * ro[4]/(ro[2] + 
                                                               ro[4])))
      delta.MH <- num/W
      P <- sum(apply(dataset, 1, function(ro) (ro[2]^2 * 
                                                 ro[3] - ro[4]^2 * ro[1] + 0.5 * ro[2] * ro[4] * 
                                                 (ro[4] - ro[2]))/(ro[2] + ro[4])^2))
      Q <- sum(apply(dataset, 1, function(ro) (ro[1] * 
                                                 (ro[4] - ro[3]) + ro[3] * (ro[2] - ro[1]))/(2 * 
                                                                                               (ro[2] + ro[4]))))
      var.delta.MH = (delta.MH * P + Q)/W^2
      SatoARisk.p <- delta.MH
      SatoARisk.l <- SatoARisk.p - qnorm(1 - (1 - conf.level)/2) * 
        sqrt(var.delta.MH)
      SatoARisk.u <- SatoARisk.p + qnorm(1 - (1 - conf.level)/2) * 
        sqrt(var.delta.MH)
      c(SatoARisk.p * units, SatoARisk.l * units, SatoARisk.u * 
          units)
    }
  }
  .funMHRD.GR <- function(dat, conf.level = 0.95, units = units) {
    if (length(dim(dat)) > 2) {
      ndat <- addmargins(A = dat, margin = 2, FUN = sum, 
                         quiet = FALSE)
      c1 <- ndat[1, 1, ]
      c2 <- ndat[1, 3, ]
      c3 <- ndat[2, 1, ]
      c4 <- ndat[2, 3, ]
      dataset <- cbind(c1, c2, c3, c4)
      num <- sum(apply(X = dataset, MARGIN = 1, FUN = function(ro) (ro[1] * 
                                                                      ro[4] - ro[3] * ro[2])/(ro[2] + ro[4])))
      W <- sum(apply(dataset, 1, function(ro) ro[2] * ro[4]/(ro[2] + 
                                                               ro[4])))
      delta.MH <- num/W
      P <- sum(apply(dataset, 1, function(ro) (ro[2]^2 * 
                                                 ro[3] - ro[4]^2 * ro[1] + 0.5 * ro[2] * ro[4] * 
                                                 (ro[4] - ro[2]))/(ro[2] + ro[4])^2))
      Q <- sum(apply(dataset, 1, function(ro) (ro[1] * 
                                                 (ro[4] - ro[3]) + ro[3] * (ro[2] - ro[1]))/(2 * 
                                                                                               (ro[2] + ro[4]))))
      p1 <- dataset[, 1]/dataset[, 2]
      p2 <- dataset[, 3]/dataset[, 4]
      denom <- apply(dataset, 1, function(ro) ro[2] * ro[4]/(ro[2] + 
                                                               ro[4]))
      var.delta.MH <- sum(denom^2 * (p1 * (1 - p1)/dataset[, 
                                                           2] + p2 * (1 - p2)/dataset[, 4]))/W^2
      GRARisk.p <- delta.MH
      GRARisk.l <- GRARisk.p - qnorm(1 - (1 - conf.level)/2) * 
        sqrt(var.delta.MH)
      GRARisk.u <- GRARisk.p + qnorm(1 - (1 - conf.level)/2) * 
        sqrt(var.delta.MH)
      c(GRARisk.p * units, GRARisk.l * units, GRARisk.u * 
          units)
    }
  }
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  a <- as.numeric(a)
  A <- as.numeric(A)
  b <- as.numeric(b)
  B <- as.numeric(B)
  c <- as.numeric(c)
  C <- as.numeric(C)
  d <- as.numeric(d)
  D <- as.numeric(D)
  M1 <- a + c
  M0 <- b + d
  N1 <- a + b
  N0 <- c + d
  total <- a + b + c + d
  n.strata <- length(a)
  if (sum(A) > 0 & sum(B) > 0 & sum(C) > 0 & sum(D) > 0) {
    sa <- sum(A)
    sb <- sum(B)
    sc <- sum(C)
    sd <- sum(D)
  }
  if (sum(A) == 0 | sum(B) == 0 | sum(C) == 0 | sum(D) == 0) {
    sa <- sum(a)
    sb <- sum(b)
    sc <- sum(c)
    sd <- sum(d)
  }
  sM1 <- sa + sc
  sM0 <- sb + sd
  sN1 <- sa + sb
  sN0 <- sc + sd
  stotal <- sa + sb + sc + sd
  .tmp <- .funincrisk(as.matrix(cbind(a, N1)), conf.level = conf.level)
  IRiske.p <- as.numeric(.tmp[, 1]) * units
  IRiske.l <- as.numeric(.tmp[, 2]) * units
  IRiske.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrisk(as.matrix(cbind(c, N0)), conf.level = conf.level)
  IRisko.p <- as.numeric(.tmp[, 1]) * units
  IRisko.l <- as.numeric(.tmp[, 2]) * units
  IRisko.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrisk(as.matrix(cbind(M1, total)), conf.level = conf.level)
  IRiskpop.p <- as.numeric(.tmp[, 1]) * units
  IRiskpop.l <- as.numeric(.tmp[, 2]) * units
  IRiskpop.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(a, b)), conf.level = conf.level)
  IRatee.p <- as.numeric(.tmp[, 1]) * units
  IRatee.l <- as.numeric(.tmp[, 2]) * units
  IRatee.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(c, d)), conf.level = conf.level)
  IRateo.p <- as.numeric(.tmp[, 1]) * units
  IRateo.l <- as.numeric(.tmp[, 2]) * units
  IRateo.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(M1, M0)), conf.level = conf.level)
  IRatepop.p <- as.numeric(.tmp[, 1]) * units
  IRatepop.l <- as.numeric(.tmp[, 2]) * units
  IRatepop.u <- as.numeric(.tmp[, 3]) * units
  Al <- (qbinom(1 - N., size = a + b, prob = (a/(a + b))))/(a + 
                                                              b)
  Au <- (qbinom(N., size = a + b, prob = (a/(a + b))))/(a + 
                                                          b)
  Oe.p <- (a/b)
  Oe.l <- (Al/(1 - Al))
  Oe.u <- (Au/(1 - Au))
  Al <- (qbinom(1 - N., size = c + d, prob = (c/(c + d))))/(c + 
                                                              d)
  Au <- (qbinom(N., size = c + d, prob = (c/(c + d))))/(c + 
                                                          d)
  Oo.p <- (c/d)
  Oo.l <- (Al/(1 - Al))
  Oo.u <- (Au/(1 - Au))
  Al <- (qbinom(1 - N., size = M1 + M0, prob = (M1/(M1 + M0))))/(M1 + 
                                                                   M0)
  Au <- (qbinom(N., size = M1 + M0, prob = (M1/(M1 + M0))))/(M1 + 
                                                               M0)
  Opop.p <- (M1/M0)
  Opop.l <- (Al/(1 - Al))
  Opop.u <- (Au/(1 - Au))
  .tmp <- .funincrisk(as.matrix(cbind(sa, sN1)), conf.level = conf.level)
  cIRiske.p <- as.numeric(.tmp[, 1]) * units
  cIRiske.l <- as.numeric(.tmp[, 2]) * units
  cIRiske.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrisk(as.matrix(cbind(sc, sN0)), conf.level = conf.level)
  cIRisko.p <- as.numeric(.tmp[, 1]) * units
  cIRisko.l <- as.numeric(.tmp[, 2]) * units
  cIRisko.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrisk(as.matrix(cbind(sM1, stotal)), conf.level = conf.level)
  cIRiskpop.p <- as.numeric(.tmp[, 1]) * units
  cIRiskpop.l <- as.numeric(.tmp[, 2]) * units
  cIRiskpop.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(sa, sb)), conf.level = conf.level)
  cIRatee.p <- as.numeric(.tmp[, 1]) * units
  cIRatee.l <- as.numeric(.tmp[, 2]) * units
  cIRatee.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(sc, sd)), conf.level = conf.level)
  cIRateo.p <- as.numeric(.tmp[, 1]) * units
  cIRateo.l <- as.numeric(.tmp[, 2]) * units
  cIRateo.u <- as.numeric(.tmp[, 3]) * units
  .tmp <- .funincrate(as.matrix(cbind(sM1, sM0)), conf.level = conf.level)
  cIRatepop.p <- as.numeric(.tmp[, 1]) * units
  cIRatepop.l <- as.numeric(.tmp[, 2]) * units
  cIRatepop.u <- as.numeric(.tmp[, 3]) * units
  Al <- (qbinom(1 - N., size = sa + sb, prob = (sa/(sa + sb))))/(sa + 
                                                                   sb)
  u <- (qbinom(N., size = sa + sb, prob = (sa/(sa + sb))))/(sa + 
                                                              sb)
  cOe.p <- sa/sb
  cOe.l <- Al/(1 - Al)
  cOe.u <- Au/(1 - Au)
  Al <- (qbinom(1 - N., size = sc + sd, prob = (sc/(sc + sd))))/(sc + 
                                                                   sd)
  u <- (qbinom(N., size = sc + sd, prob = (sc/(sc + sd))))/(sc + 
                                                              sd)
  cOo.p <- sc/sd
  cOo.l <- Al/(1 - Al)
  cOo.u <- Au/(1 - Au)
  Al <- (qbinom(1 - N., size = sM1 + sM0, prob = (sM1/(sM1 + 
                                                         sM0))))/(sM1 + sM0)
  u <- (qbinom(N., size = sM1 + sM0, prob = (sM1/(sM1 + sM0))))/(sM1 + 
                                                                   sM0)
  cOpop.p <- sM1/sM0
  cOpop.l <- Al/(1 - Al)
  cOpop.u <- Au/(1 - Au)
  wRR.ctype <- "Wald"
  wRR.p <- c()
  wRR.l <- c()
  wRR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funRRwald(dat[, , i], conf.level)
      wRR.p <- c(wRR.p, .tmp[1])
      wRR.l <- c(wRR.l, .tmp[2])
      wRR.u <- c(wRR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funRRwald(dat, conf.level)
    wRR.p <- .tmp[1]
    wRR.l <- .tmp[2]
    wRR.u <- .tmp[3]
  }
  scRR.ctype <- "Score"
  scRR.p <- c()
  scRR.l <- c()
  scRR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funRRscore(dat[, , i], conf.level)
      scRR.p <- c(scRR.p, .tmp[1])
      scRR.l <- c(scRR.l, .tmp[2])
      scRR.u <- c(scRR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funRRscore(dat, conf.level)
    scRR.p <- .tmp[1]
    scRR.l <- .tmp[2]
    scRR.u <- .tmp[3]
  }
  IRR.ctype <- ""
  IRR.p <- (a/b)/(c/d)
  lnIRR <- log(IRR.p)
  lnIRR.var <- (1/a) + (1/c)
  lnIRR.se <- sqrt((1/a) + (1/c))
  IRR.se <- exp(lnIRR.se)
  pl <- a/(a + (c + 1) * (1/qf(1 - N., 2 * a, 2 * c + 2)))
  ph <- (a + 1)/(a + 1 + c/(1/qf(1 - N., 2 * c, 2 * a + 2)))
  IRR.l <- pl * d/((1 - pl) * b)
  IRR.u <- ph * d/((1 - ph) * b)
  IRR.w <- 1/(exp(lnIRR.var))
  wOR.ctype <- "Wald"
  wOR.p <- c()
  wOR.l <- c()
  wOR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funORwald(dat[, , i], conf.level)
      wOR.p <- c(wOR.p, .tmp[1])
      wOR.l <- c(wOR.l, .tmp[2])
      wOR.u <- c(wOR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funORwald(dat, conf.level)
    wOR.p <- .tmp[1]
    wOR.l <- .tmp[2]
    wOR.u <- .tmp[3]
  }
  cfOR.ctype <- "Cornfield"
  cfOR.p <- c()
  cfOR.l <- c()
  cfOR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funORcfield(dat[, , i], conf.level)
      cfOR.p <- c(cfOR.p, .tmp[1])
      cfOR.l <- c(cfOR.l, .tmp[2])
      cfOR.u <- c(cfOR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funORcfield(dat, conf.level)
    cfOR.p <- .tmp[1]
    cfOR.l <- .tmp[2]
    cfOR.u <- .tmp[3]
  }
  scOR.ctype <- "Score"
  scOR.p <- c()
  scOR.l <- c()
  scOR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funORscore(dat[, , i], conf.level)
      scOR.p <- c(scOR.p, .tmp[1])
      scOR.l <- c(scOR.l, .tmp[2])
      scOR.u <- c(scOR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funORscore(dat, conf.level)
    scOR.p <- .tmp[1]
    scOR.l <- .tmp[2]
    scOR.u <- .tmp[3]
  }
  mOR.ctype <- "MLE"
  mOR.p <- c()
  mOR.l <- c()
  mOR.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funORml(dat[, , i], conf.level)
      mOR.p <- c(mOR.p, .tmp[1])
      mOR.l <- c(mOR.l, .tmp[2])
      mOR.u <- c(mOR.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funORml(dat, conf.level)
    mOR.p <- .tmp[1]
    mOR.l <- .tmp[2]
    mOR.u <- .tmp[3]
  }
  wARisk.ctype <- "Wald"
  wARisk.p <- c()
  wARisk.l <- c()
  wARisk.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funARwald(dat[, , i], conf.level, units)
      wARisk.p <- c(wARisk.p, .tmp[1])
      wARisk.l <- c(wARisk.l, .tmp[2])
      wARisk.u <- c(wARisk.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funARwald(dat, conf.level, units)
    wARisk.p <- .tmp[1]
    wARisk.l <- .tmp[2]
    wARisk.u <- .tmp[3]
  }
  scARisk.ctype <- "Score"
  scARisk.p <- c()
  scARisk.l <- c()
  scARisk.u <- c()
  if (length(dim(dat)) == 3) {
    for (i in 1:dim(dat)[3]) {
      .tmp <- .funARscore(dat[, , i], conf.level, units)
      scARisk.p <- c(scARisk.p, .tmp[1])
      scARisk.l <- c(scARisk.l, .tmp[2])
      scARisk.u <- c(scARisk.u, .tmp[3])
    }
  }
  if (length(dim(dat)) == 2) {
    .tmp <- .funARscore(dat, conf.level, units)
    scARisk.p <- .tmp[1]
    scARisk.l <- .tmp[2]
    scARisk.u <- .tmp[3]
  }
  ARate.ctype <- ""
  ARate.p <- ((a/b) - (c/d)) * units
  ARate.var <- (a/b^2) + (c/d^2)
  ARate.se <- (sqrt((a/b^2) + (c/d^2))) * units
  ARate.l <- ARate.p - (z * ARate.se)
  ARate.u <- ARate.p + (z * ARate.se)
  ARate.w <- 1/(ARate.var)
  AFRisk.ctype <- ""
  AFRisk.p <- ((wRR.p - 1)/wRR.p)
  AFRisk.l <- (wRR.l - 1)/wRR.l
  AFRisk.u <- (wRR.u - 1)/wRR.u
  AFRate.ctype <- ""
  AFRate.p <- (IRR.p - 1)/IRR.p
  AFRate.l <- (IRR.l - 1)/IRR.l
  AFRate.u <- (IRR.u - 1)/IRR.u
  AFest.ctype <- ""
  AFest.p <- (mOR.p - 1)/mOR.p
  AFest.l <- (mOR.l - 1)/mOR.l
  AFest.u <- (mOR.u - 1)/mOR.u
  wPARisk.ctype <- ""
  wPARisk.p <- ((M1/total) - (c/N0)) * units
  wPARisk.se <- (sqrt(((M1 * (total - M1))/total^3) + ((c * 
                                                          (N0 - c))/N0^3))) * units
  wPARisk.l <- wPARisk.p - (z * wPARisk.se)
  wPARisk.u <- wPARisk.p + (z * wPARisk.se)
  pPARisk.ctype <- "Pirikahu"
  pPARisk.p <- ((M1/total) - (c/N0)) * units
  pPARisk.d1 <- (1/total) - ((a + c)/total^2)
  pPARisk.d2 <- -((a + c)/total^2)
  pPARisk.d3 <- (c/(c + d)^2) - ((a + c)/total^2) + (1/total) - 
    (1/(c + d))
  pPARisk.d4 <- (c/(c + d)^2) - ((a + c)/total^2)
  pPARisk.var <- ((pPARisk.d1^2) * a) + ((pPARisk.d2^2) * b) + 
    ((pPARisk.d3^2) * c) + ((pPARisk.d4^2) * d)
  pPARisk.se <- sqrt(pPARisk.var) * units
  pPARisk.l <- pPARisk.p - (z * pPARisk.se)
  pPARisk.u <- pPARisk.p + (z * pPARisk.se)
  PARate.ctype <- ""
  PARate.p <- ((M1/M0) - (c/d)) * units
  PARate.se <- (sqrt((M1/M0^2) + (c/d^2))) * units
  PARate.l <- PARate.p - (z * PARate.se)
  PARate.u <- PARate.p + (z * PARate.se)
  PAFRisk.ctype <- "Jewell"
  PAFRisk.p <- ((a * d) - (b * c))/((a + c) * (c + d))
  PAFRisk.var <- (b + (PAFRisk.p * (a + d)))/(total * c)
  PAFRisk.l <- 1 - exp(log(1 - PAFRisk.p) + (z * sqrt(PAFRisk.var)))
  PAFRisk.u <- 1 - exp(log(1 - PAFRisk.p) - (z * sqrt(PAFRisk.var)))
  PAFRate.ctype <- "Sullivan"
  PAFRate.p <- (IRatepop.p - IRateo.p)/IRatepop.p
  PAFRate.l <- min((IRatepop.l - IRateo.l)/IRatepop.l, (IRatepop.u - 
                                                          IRateo.u)/IRatepop.u)
  PAFRate.u <- max((IRatepop.l - IRateo.l)/IRatepop.l, (IRatepop.u - 
                                                          IRateo.u)/IRatepop.u)
  PAFest.ctype <- "Jewell"
  PAFest.p <- ((a * d) - (b * c))/(d * (a + c))
  PAFest.var <- (a/(c * (a + c))) + (b/(d * (b + d)))
  PAFest.l <- 1 - exp(log(1 - PAFest.p) + (z * sqrt(PAFest.var)))
  PAFest.u <- 1 - exp(log(1 - PAFest.p) - (z * sqrt(PAFest.var)))
  cwRR.ctype <- "Wald"
  .tmp <- .funRRwald(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                     conf.level)
  cwRR.p <- .tmp[1]
  cwRR.l <- .tmp[2]
  cwRR.u <- .tmp[3]
  csRR.ctype <- "Score"
  .tmp <- .funRRscore(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                      conf.level)
  csRR.p <- .tmp[1]
  csRR.l <- .tmp[2]
  csRR.u <- .tmp[3]
  ceIRR.ctype <- "Exact"
  ceIRR.p <- (sa/sb)/(sc/sd)
  celnIRR <- log(ceIRR.p)
  celnIRR.se <- sqrt((1/sa) + (1/sc))
  ceIRR.se <- exp(celnIRR.se)
  pl <- sa/(sa + (sc + 1) * (1/qf(1 - N., 2 * sa, 2 * sc + 
                                    2)))
  ph <- (sa + 1)/(sa + 1 + sc/(1/qf(1 - N., 2 * sc, 2 * sa + 
                                      2)))
  ceIRR.l <- pl * sd/((1 - pl) * sb)
  ceIRR.u <- ph * sd/((1 - ph) * sb)
  cwOR.ctype <- "Wald"
  .tmp <- .funORwald(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                     conf.level)
  cwOR.p <- .tmp[1]
  cwOR.l <- .tmp[2]
  cwOR.u <- .tmp[3]
  ccfOR.ctype <- "Cornfield"
  .tmp <- .funORcfield(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                       conf.level)
  ccfOR.p <- .tmp[1]
  ccfOR.l <- .tmp[2]
  ccfOR.u <- .tmp[3]
  csOR.ctype <- "Score"
  .tmp <- .funORscore(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                      conf.level)
  csOR.p <- .tmp[1]
  csOR.l <- .tmp[2]
  csOR.u <- .tmp[3]
  cmOR.ctype <- "MLE"
  cmOR.tmp <- fisher.test(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                          conf.int = TRUE, conf.level = conf.level)
  cmOR.p <- as.numeric(cmOR.tmp$estimate)
  cmOR.l <- as.numeric(cmOR.tmp$conf.int)[1]
  cmOR.u <- as.numeric(cmOR.tmp$conf.int)[2]
  cwARisk.ctype <- "Wald"
  .tmp <- .funARwald(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                     conf.level, units)
  cwARisk.p <- .tmp[1]
  cwARisk.l <- .tmp[2]
  cwARisk.u <- .tmp[3]
  cscARisk.ctype <- "Score"
  .tmp <- .funARscore(apply(dat, MARGIN = c(1, 2), FUN = sum), 
                      conf.level, units)
  cscARisk.p <- .tmp[1]
  cscARisk.l <- .tmp[2]
  cscARisk.u <- .tmp[3]
  cARate.ctype <- "Wald"
  cARate.p <- ((sa/sb) - (sc/sd)) * units
  cARate.se <- (sqrt((sa/sb^2) + (sc/sd^2))) * units
  cARate.l <- cARate.p - (z * cARate.se)
  cARate.u <- cARate.p + (z * cARate.se)
  cAFrisk.ctype <- "Score"
  cAFRisk.p <- (csRR.p - 1)/csRR.p
  cAFRisk.l <- min((csRR.l - 1)/csRR.l, (csRR.u - 1)/csRR.u)
  cAFRisk.u <- max((csRR.l - 1)/csRR.l, (csRR.u - 1)/csRR.u)
  cAFRate.ctype <- "Exact"
  cAFRate.p <- (ceIRR.p - 1)/ceIRR.p
  cAFRate.l <- min((ceIRR.l - 1)/ceIRR.l, (ceIRR.u - 1)/ceIRR.u)
  cAFRate.u <- max((ceIRR.l - 1)/ceIRR.l, (ceIRR.u - 1)/ceIRR.u)
  cAFest.ctype <- "Score"
  cAFest.p <- (scOR.p - 1)/scOR.p
  cAFest.l <- min((scOR.l - 1)/scOR.l, (scOR.u - 1)/scOR.u)
  cAFest.u <- max((scOR.l - 1)/scOR.l, (scOR.u - 1)/scOR.u)
  cwPARisk.ctype <- "Wald"
  cwPARisk.p <- ((sM1/stotal) - (sc/sN0)) * units
  cwPARisk.se <- (sqrt(((sM1 * (stotal - sM1))/stotal^3) + 
                         ((sc * (sN0 - sc))/sN0^3))) * units
  cwPARisk.l <- cwPARisk.p - (z * cwPARisk.se)
  cwPARisk.u <- cwPARisk.p + (z * cwPARisk.se)
  cpPARisk.ctype <- "Pirikahu"
  cpPARisk.p <- ((sM1/stotal) - (sc/sN0)) * units
  cpPARisk.d1 <- (1/stotal) - ((sa + sc)/stotal^2)
  cpPARisk.d2 <- -((sa + sc)/stotal^2)
  cpPARisk.d3 <- (sc/(sc + sd)^2) - ((sa + sc)/stotal^2) + 
    (1/stotal) - (1/(sc + sd))
  cpPARisk.d4 <- (sc/(sc + sd)^2) - ((sa + sc)/stotal^2)
  cpPARisk.var <- ((cpPARisk.d1^2) * sa) + ((cpPARisk.d2^2) * 
                                              sb) + ((cpPARisk.d3^2) * sc) + ((cpPARisk.d4^2) * sd)
  cpPARisk.se <- sqrt(cpPARisk.var) * units
  cpPARisk.l <- cpPARisk.p - (z * cpPARisk.se)
  cpPARisk.u <- cpPARisk.p + (z * cpPARisk.se)
  cPARate.ctype <- "Wald"
  cPARate.p <- ((sM1/sM0) - (sc/sd)) * units
  cPARate.se <- (sqrt((sM1/sM0^2) + (sc/sd^2))) * units
  cPARate.l <- cPARate.p - (z * cPARate.se)
  cPARate.u <- cPARate.p + (z * cPARate.se)
  cPAFRisk.ctype <- ""
  cPAFRisk.p <- (cIRiskpop.p - cIRisko.p)/cIRiskpop.p
  cPAFRisk.l <- min((cIRiskpop.l - cIRisko.l)/cIRiskpop.l, 
                    (cIRiskpop.u - cIRisko.u)/cIRiskpop.u)
  cPAFRisk.u <- max((cIRiskpop.l - cIRisko.l)/cIRiskpop.l, 
                    (cIRiskpop.u - cIRisko.u)/cIRiskpop.u)
  cPAFRate.ctype <- ""
  cPAFRate.p <- (cIRatepop.p - cIRateo.p)/cIRatepop.p
  cPAFRate.l <- min((cIRatepop.l - cIRateo.l)/cIRatepop.l, 
                    (cIRatepop.u - cIRateo.u)/cIRatepop.u)
  cPAFRate.u <- max((cIRatepop.l - cIRateo.l)/cIRatepop.l, 
                    (cIRatepop.u - cIRateo.u)/cIRatepop.u)
  cPAFest.ctype <- ""
  cPAFest.p <- (cOpop.p - cOo.p)/cOpop.p
  cPAFest.l <- min((cOpop.l - cOo.l)/cOpop.l, (cOpop.u - cOo.u)/cOpop.u)
  cPAFest.u <- max((cOpop.l - cOo.l)/cOpop.l, (cOpop.u - cOo.u)/cOpop.u)
  sRR.p <- sum((a * N0/total))/sum((c * N1/total))
  varLNRR.s <- sum(((M1 * N1 * N0)/total^2) - ((a * c)/total))/(sum((a * 
                                                                       N0)/total) * sum((c * N1)/total))
  lnRR.s <- log(sRR.p)
  sRR.se <- (sqrt(varLNRR.s))
  sRR.l <- exp(lnRR.s - (z * sqrt(varLNRR.s)))
  sRR.u <- exp(lnRR.s + (z * sqrt(varLNRR.s)))
  sIRR.p <- sum((a * d)/M0)/sum((c * b)/M0)
  lnIRR.s <- log(sIRR.p)
  varLNIRR.s <- (sum((M1 * b * d)/M0^2))/(sum((a * d)/M0) * 
                                            sum((c * b)/M0))
  sIRR.se <- sqrt(varLNIRR.s)
  sIRR.l <- exp(lnIRR.s - (z * sqrt(varLNIRR.s)))
  sIRR.u <- exp(lnIRR.s + (z * sqrt(varLNIRR.s)))
  sOR.p <- sum((a * d/total))/sum((b * c/total))
  G <- a * d/total
  H <- b * c/total
  P <- (a + d)/total
  Q <- (b + c)/total
  GQ.HP <- G * Q + H * P
  sumG <- sum(G)
  sumH <- sum(H)
  sumGP <- sum(G * P)
  sumGH <- sum(G * H)
  sumHQ <- sum(H * Q)
  sumGQ <- sum(G * Q)
  sumGQ.HP <- sum(GQ.HP)
  varLNOR.s <- sumGP/(2 * sumG^2) + sumGQ.HP/(2 * sumG * sumH) + 
    sumHQ/(2 * sumH^2)
  lnOR.s <- log(sOR.p)
  sOR.se <- sqrt(varLNOR.s)
  sOR.l <- exp(lnOR.s - z * sqrt(varLNOR.s))
  sOR.u <- exp(lnOR.s + z * sqrt(varLNOR.s))
  sARisk.p <- (sum(((a * N0) - (c * N1))/total)/sum((N1 * N0)/total)) * 
    units
  w <- (N1 * N0)/total
  var.p1 <- (((a * d)/(N1^2 * (N1 - 1))) + ((c * b)/(N0^2 * 
                                                       (N0 - 1))))
  var.p1[N0 == 1] <- 0
  var.p1[N1 == 1] <- 0
  varARisk.s <- sum(w^2 * var.p1)/sum(w)^2
  sARisk.se <- (sqrt(varARisk.s)) * units
  sARisk.l <- sARisk.p - (z * sARisk.se)
  sARisk.u <- sARisk.p + (z * sARisk.se)
  SatoARisk.ctype <- "Sato"
  .tmp <- .funMHRD.Sato(dat, conf.level, units)
  SatoARisk.p <- .tmp[1]
  SatoARisk.l <- .tmp[2]
  SatoARisk.u <- .tmp[3]
  GRARisk.ctype <- "Greenland-Robins"
  .tmp <- .funMHRD.GR(dat, conf.level, units)
  GRARisk.p <- .tmp[1]
  GRARisk.l <- .tmp[2]
  GRARisk.u <- .tmp[3]
  sARate.p <- sum(((a * d) - (c * b))/M0)/sum((b * d)/M0) * 
    units
  varARate.s <- sum(((b * d)/M0)^2 * ((a/b^2) + (c/d^2)))/sum((b * 
                                                                 d)/M0)^2
  sARate.se <- sqrt(varARate.s) * units
  sARate.l <- sARate.p - (z * sARate.se)
  sARate.u <- sARate.p + (z * sARate.se)
  RR.conf.p <- (csRR.p/sRR.p)
  RR.conf.l <- (csRR.l/sRR.l)
  RR.conf.u <- (csRR.u/sRR.u)
  IRR.conf.p <- (ceIRR.p/sIRR.p)
  IRR.conf.l <- (ceIRR.l/sIRR.l)
  IRR.conf.u <- (ceIRR.u/sIRR.u)
  OR.conf.p <- (scOR.p/sOR.p)
  OR.conf.l <- (scOR.l/sOR.l)
  OR.conf.u <- (scOR.u/sOR.u)
  ARisk.conf.p <- (cscARisk.p/scARisk.p)
  ARisk.conf.l <- (cscARisk.l/scARisk.l)
  ARisk.conf.u <- (cscARisk.u/scARisk.u)
  ARate.conf.p <- (cARate.p/sARate.p)
  ARate.conf.l <- (cARate.l/sARate.l)
  ARate.conf.u <- (cARate.u/sARate.u)
  exp.a <- (N1 * M1)/total
  exp.b <- (N1 * M0)/total
  exp.c <- (N0 * M1)/total
  exp.d <- (N0 * M0)/total
  chi2 <- (((a - exp.a)^2)/exp.a) + (((b - exp.b)^2)/exp.b) + 
    (((c - exp.c)^2)/exp.c) + (((d - exp.d)^2)/exp.d)
  p.chi2 <- 1 - pchisq(chi2, df = 1)
  exp.sa <- (sN1 * sM1)/stotal
  exp.sb <- (sN1 * sM0)/stotal
  exp.sc <- (sN0 * sM1)/stotal
  exp.sd <- (sN0 * sM0)/stotal
  chi2s <- (((sa - exp.sa)^2)/exp.sa) + (((sb - exp.sb)^2)/exp.sb) + 
    (((sc - exp.sc)^2)/exp.sc) + (((sd - exp.sd)^2)/exp.sd)
  p.chi2s <- 1 - pchisq(chi2s, df = 1)
  if (length(a) > 1) {
    chi2m <- as.numeric(mantelhaen.test(dat)$statistic)
    p.chi2m <- as.numeric(mantelhaen.test(dat)$p.value)
  }
  if (length(a) > 1) {
    if (homogeneity == "woolf") {
      lnRR. <- log((a/(a + b))/(c/(c + d)))
      lnRR.var. <- (b/(a * (a + b))) + (d/(c * (c + d)))
      wRR. <- 1/lnRR.var.
      lnRR.s. <- sum(wRR. * lnRR.)/sum(wRR.)
      RR.homogeneity <- sum(wRR. * (lnRR. - lnRR.s.)^2)
      RR.homogeneity.p <- 1 - pchisq(RR.homogeneity, df = n.strata - 
                                       1)
      RR.homog <- data.frame(test.statistic = RR.homogeneity, 
                             df = n.strata - 1, p.value = RR.homogeneity.p)
      lnOR. <- log(((a + 0.5) * (d + 0.5))/((b + 0.5) * 
                                              (c + 0.5)))
      lnOR.var. <- (1/(a + 0.5)) + (1/(b + 0.5)) + (1/(c + 
                                                         0.5)) + (1/(d + 0.5))
      wOR. <- 1/lnOR.var.
      lnOR.s. <- sum((wOR. * lnOR.))/sum(wOR.)
      OR.homogeneity <- sum(wOR. * (lnOR. - lnOR.s.)^2)
      OR.homogeneity.p <- 1 - pchisq(OR.homogeneity, df = n.strata - 
                                       1)
      OR.homog <- data.frame(test.statistic = OR.homogeneity, 
                             df = n.strata - 1, p.value = OR.homogeneity.p)
    }
    if (homogeneity == "breslow.day") {
      n11k <- dat[1, 1, ]
      n21k <- dat[2, 1, ]
      n12k <- dat[1, 2, ]
      n22k <- dat[2, 2, ]
      row1sums <- n11k + n12k
      row2sums <- n21k + n22k
      col1sums <- n11k + n21k
      Amax <- apply(cbind(row1sums, col1sums), 1, min)
      bb <- row2sums + row1sums * sRR.p - col1sums * (1 - 
                                                        sRR.p)
      determ <- sqrt(bb^2 + 4 * (1 - sRR.p) * sRR.p * row1sums * 
                       col1sums)
      Astar <- (-bb + cbind(-determ, determ))/(2 - 2 * 
                                                 sRR.p)
      Astar <- ifelse(Astar[, 1] <= Amax & Astar[, 1] >= 
                        0, Astar[, 1], Astar[, 2])
      Bstar <- row1sums - Astar
      Cstar <- col1sums - Astar
      Dstar <- row2sums - col1sums + Astar
      Var <- apply(1/cbind(Astar, Bstar, Cstar, Dstar), 
                   1, sum)^(-1)
      RR.homogeneity <- sum((dat[1, 1, ] - Astar)^2/Var)
      RR.homogeneity.p <- 1 - pchisq(RR.homogeneity, df = n.strata - 
                                       1)
      bb <- row2sums + row1sums * sOR.p - col1sums * (1 - 
                                                        sOR.p)
      determ <- sqrt(bb^2 + 4 * (1 - sOR.p) * sOR.p * row1sums * 
                       col1sums)
      Astar <- (-bb + cbind(-determ, determ))/(2 - 2 * 
                                                 sOR.p)
      Astar <- ifelse(Astar[, 1] <= Amax & Astar[, 1] >= 
                        0, Astar[, 1], Astar[, 2])
      Bstar <- row1sums - Astar
      Cstar <- col1sums - Astar
      Dstar <- row2sums - col1sums + Astar
      Var <- apply(1/cbind(Astar, Bstar, Cstar, Dstar), 
                   1, sum)^(-1)
      OR.homogeneity <- sum((dat[1, 1, ] - Astar)^2/Var)
      OR.homogeneity.p <- 1 - pchisq(OR.homogeneity, df = n.strata - 
                                       1)
    }
  }
  res <- list(RR.strata.wald = data.frame(est = wRR.p, lower = wRR.l, 
                                          upper = wRR.u), RR.strata.score = data.frame(est = scRR.p, 
                                                                                       lower = scRR.l, upper = scRR.u), RR.crude.wald = data.frame(est = cwRR.p, 
                                                                                                                                                   lower = cwRR.l, upper = cwRR.u), RR.crude.score = data.frame(est = csRR.p, 
                                                                                                                                                                                                                lower = csRR.l, upper = csRR.u), RR.mh.wald = data.frame(est = sRR.p, 
                                                                                                                                                                                                                                                                         lower = sRR.l, upper = sRR.u), IRR.strata.wald = data.frame(est = IRR.p, 
                                                                                                                                                                                                                                                                                                                                     lower = IRR.l, upper = IRR.u), IRR.crude.wald = data.frame(est = ceIRR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                lower = ceIRR.l, upper = ceIRR.u), IRR.mh.wald = data.frame(est = sIRR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                            lower = sIRR.l, upper = sIRR.u), OR.strata.wald = data.frame(est = wOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lower = wOR.l, upper = wOR.u), OR.strata.score = data.frame(est = scOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     lower = scOR.l, upper = scOR.u), OR.strata.cfield = data.frame(est = cfOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    lower = cfOR.l, upper = cfOR.u), OR.strata.mle = data.frame(est = mOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                lower = mOR.l, upper = mOR.u), OR.crude.wald = data.frame(est = cwOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          lower = cwOR.l, upper = cwOR.u), OR.crude.score = data.frame(est = csOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       lower = csOR.l, upper = csOR.u), OR.crude.cfield = data.frame(est = ccfOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     lower = ccfOR.l, upper = ccfOR.u), OR.crude.mle = data.frame(est = cmOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  lower = cmOR.l, upper = cmOR.u), OR.mh.wald = data.frame(est = sOR.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lower = sOR.l, upper = sOR.u), ARisk.strata.wald = data.frame(est = wARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lower = wARisk.l, upper = wARisk.u), ARisk.strata.score = data.frame(est = scARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              lower = scARisk.l, upper = scARisk.u), ARisk.crude.wald = data.frame(est = cwARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   lower = cwARisk.l, upper = cwARisk.u), ARisk.crude.score = data.frame(est = cscARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lower = cscARisk.l, upper = cscARisk.u), ARisk.mh.wald = data.frame(est = sARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             lower = sARisk.l, upper = sARisk.u), ARisk.mh.sato = data.frame(est = SatoARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             lower = SatoARisk.l, upper = SatoARisk.u), ARisk.mh.green = data.frame(est = GRARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    lower = GRARisk.l, upper = GRARisk.u), ARate.strata.wald = data.frame(est = ARate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          lower = ARate.l, upper = ARate.u), ARate.crude.wald = data.frame(est = cARate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lower = cARate.l, upper = cARate.u), ARate.mh.wald = data.frame(est = sARate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lower = sARate.l, upper = sARate.u), AFRisk.strata.wald = data.frame(est = AFRisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                lower = AFRisk.l, upper = AFRisk.u), AFRisk.crude.wald = data.frame(est = cAFRisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    lower = cAFRisk.l, upper = cAFRisk.u), AFRate.strata.wald = data.frame(est = AFRate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lower = AFRate.l, upper = AFRate.u), AFRate.crude.wald = data.frame(est = cAFRate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               lower = cAFRate.l, upper = cAFRate.u), AFest.strata.wald = data.frame(est = AFest.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     lower = AFest.l, upper = AFest.u), AFest.crude.wald = data.frame(est = cAFest.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      lower = cAFest.l, upper = cAFest.u), PARisk.strata.wald = data.frame(est = wPARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           lower = wPARisk.l, upper = wPARisk.u), PARisk.strata.piri = data.frame(est = pPARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  lower = pPARisk.l, upper = pPARisk.u), PARisk.crude.wald = data.frame(est = cwPARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        lower = cwPARisk.l, upper = cwPARisk.u), PARisk.crude.piri = data.frame(est = cpPARisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                lower = cpPARisk.l, upper = cpPARisk.u), PARate.strata.wald = data.frame(est = PARate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lower = PARate.l, upper = PARate.u), PARate.crude.wald = data.frame(est = cPARate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             lower = cPARate.l, upper = cPARate.u), PAFRisk.strata.wald = data.frame(est = PAFRisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     lower = PAFRisk.l, upper = PAFRisk.u), PAFRisk.crude.wald = data.frame(est = cPAFRisk.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            lower = cPAFRisk.l, upper = cPAFRisk.u), PAFRate.strata.wald = data.frame(est = PAFRate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      lower = PAFRate.l, upper = PAFRate.u), PAFRate.crude.wald = data.frame(est = cPAFRate.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             lower = cPAFRate.l, upper = cPAFRate.u), PAFest.strata.wald = data.frame(est = PAFest.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      lower = PAFest.l, upper = PAFest.u), PAFest.crude.wald = data.frame(est = cPAFest.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          lower = cPAFest.l, upper = cPAFest.u), RR.conf = data.frame(est = RR.conf.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      lower = RR.conf.l, upper = RR.conf.u), IRR.conf = data.frame(est = IRR.conf.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   lower = IRR.conf.l, upper = IRR.conf.u), OR.conf = data.frame(est = OR.conf.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 lower = OR.conf.l, upper = OR.conf.u), ARisk.conf = data.frame(est = ARisk.conf.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                lower = ARisk.conf.l, upper = ARisk.conf.u), ARate.conf = data.frame(est = ARate.conf.p, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     lower = ARate.conf.l, upper = ARate.conf.u), count.units = ifelse(units == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1, "Outcomes per population unit", paste("Outcomes per ", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  units, " population units", sep = "")), time.units = ifelse(units == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                1, "Outcomes per unit of population time at risk", paste("Outcomes per ", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         units, " units of population time at risk", sep = "")), 
              chisq.strata = data.frame(test.statistic = chi2, df = 1, 
                                        p.value = p.chi2), chisq.crude = data.frame(test.statistic = chi2s, 
                                                                                    df = 1, p.value = p.chi2s))
  if (n.strata > 1) {
    res$chisq.mh = data.frame(test.statistic = chi2m, df = 1, 
                              p.value = p.chi2m)
    res$RR.homog = data.frame(test.statistic = RR.homogeneity, 
                              df = n.strata - 1, p.value = RR.homogeneity.p)
    res$OR.homog = data.frame(test.statistic = OR.homogeneity, 
                              df = n.strata - 1, p.value = OR.homogeneity.p)
  }
  if (method == "cohort.count" & n.strata == 1) {
    massoc <- list(RR.strata.wald = res$RR.strata.wald, RR.strata.score = res$RR.strata.score, 
                   OR.strata.wald = res$OR.strata.wald, OR.strata.score = res$OR.strata.score, 
                   OR.strata.cfield = res$OR.strata.cfield, OR.strata.mle = res$OR.strata.mle, 
                   ARisk.strata.wald = res$ARisk.strata.wald, ARisk.strata.score = res$ARisk.strata.score, 
                   PARisk.strata.wald = res$PARisk.strata.wald, PARisk.strata.piri = res$PARisk.strata.piri, 
                   AFRisk.strata.wald = res$AFRisk.strata.wald, PAFRisk.strata.wald = res$PAFRisk.strata.wald, 
                   chisq.strata = res$chisq.strata)
    if (outcome == "as.columns") {
      r1 <- c(a, b, N1, cIRiske.p, cOe.p)
      r2 <- c(c, d, N0, cIRisko.p, cOo.p)
      r3 <- c(M1, M0, M0 + M1, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Inc risk *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(a, c, M1)
      r2 <- c(b, d, M0)
      r3 <- c(N1, N0, N0 + N1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cohort.count", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "cohort.count" & n.strata > 1) {
    massoc <- list(RR.strata.wald = res$RR.strata.wald, RR.strata.score = res$RR.strata.score, 
                   RR.crude.wald = res$RR.crude.wald, RR.crude.score = res$RR.crude.score, 
                   RR.mh.wald = res$RR.mh.wald, OR.strata.wald = res$OR.strata.wald, 
                   OR.strata.score = res$OR.strata.score, OR.strata.cfield = res$OR.strata.cfield, 
                   OR.strata.mle = res$OR.strata.mle, OR.crude.wald = res$OR.crude.wald, 
                   OR.crude.score = res$OR.crude.score, OR.crude.cfield = res$OR.crude.cfield, 
                   OR.crude.mle = res$OR.crude.mle, OR.mh.wald = res$OR.mh.wald, 
                   ARisk.strata.wald = res$ARisk.strata.wald, ARisk.strata.score = res$ARisk.strata.score, 
                   ARisk.crude.wald = res$ARisk.crude.wald, ARisk.crude.score = res$ARisk.crude.score, 
                   ARisk.mh.wald = res$ARisk.mh.wald, ARisk.mh.sato = res$ARisk.mh.sato, 
                   ARisk.mh.green = res$ARisk.mh.green, PARisk.strata.wald = res$PARisk.strata.wald, 
                   PARisk.strata.piri = res$PARisk.strata.piri, PARisk.crude.wald = res$PARisk.crude.wald, 
                   PARisk.crude.piri = res$PARisk.crude.piri, AFRisk.strata.wald = res$AFRisk.strata.wald, 
                   AFRisk.crude.wald = res$AFRisk.crude.wald, PAFRisk.strata.wald = res$PAFRisk.strata.wald, 
                   PAFRisk.crude.wald = res$PAFRisk.crude.wald, chisq.strata = res$chisq.strata, 
                   chisq.crude = res$chisq.crude, chisq.mh = res$chisq.mh, 
                   RR.homog = res$RR.homog, OR.homog = res$OR.homog)
    if (outcome == "as.columns") {
      r1 <- c(sa, sb, sN1, cIRiske.p, cOe.p)
      r2 <- c(sc, sd, sN0, cIRisko.p, cOo.p)
      r3 <- c(sM1, sM0, sM0 + sM1, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Inc risk *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(sa, sc, sM1)
      r2 <- c(sb, sd, sM0)
      r3 <- c(sN1, sN0, sN0 + sN1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cohort.count", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "cohort.time" & n.strata == 1) {
    massoc <- list(IRR.strata.wald = res$IRR.strata.wald, 
                   ARate.strata.wald = res$ARate.strata.wald, PARate.strata.wald = res$PARate.strata.wald, 
                   AFRate.strata.wald = res$AFRate.strata.wald, PAFRate.strata.wald = res$PAFRate.strata.wald, 
                   chisq.strata = res$chisq.strata)
    if (outcome == "as.columns") {
      r1 <- c(a, b, cIRatee.p)
      r2 <- c(c, d, cIRateo.p)
      r3 <- c(M1, M0, cIRatepop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Time at risk", 
                         "       Inc rate *")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(a, c, M1)
      r2 <- c(b, d, M0)
      r3 <- c(N1, N0, N0 + N1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Time at risk", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cohort.time", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "cohort.time" & n.strata > 1) {
    massoc <- list(IRR.strata.wald = res$IRR.strata.wald, 
                   IRR.crude.wald = res$IRR.crude.wald, IRR.mh.wald = res$IRR.mh.wald, 
                   ARate.strata.wald = res$ARate.strata.wald, ARate.crude.wald = res$ARate.crude.wald, 
                   ARate.mh.wald = res$ARate.mh.wald, PARate.strata.wald = res$PARate.strata.wald, 
                   PARate.crude.wald = res$PARate.crude.wald, AFRate.strata.wald = res$AFRate.strata.wald, 
                   AFRate.crude.wald = res$AFRate.crude.wald, PAFRate.strata.wald = res$PAFRate.strata.wald, 
                   PAFRate.crude.wald = res$PAFRate.crude.wald, chisq.strata = res$chisq.strata, 
                   chisq.crude = res$chisq.crude, chisq.mh = res$chisq.mh)
    if (outcome == "as.columns") {
      r1 <- c(sa, sb, cIRatee.p)
      r2 <- c(sc, sd, cIRateo.p)
      r3 <- c(sM1, sM0, cIRatepop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Time at risk", 
                         "       Inc rate *")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(sa, sc)
      r2 <- c(sb, sd)
      r3 <- c(sN1, sN0)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cohort.time", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "case.control" & n.strata == 1) {
    massoc <- list(OR.strata.wald = res$OR.strata.wald, OR.strata.score = res$OR.strata.score, 
                   OR.strata.cfield = res$OR.strata.cfield, OR.strata.mle = res$OR.strata.mle, 
                   ARisk.strata.wald = res$ARisk.strata.wald, ARisk.strata.score = res$ARisk.strata.score, 
                   PARisk.strata.wald = res$PARisk.strata.wald, PARisk.strata.piri = res$PARisk.strata.piri, 
                   AFest.strata.wald = res$AFest.strata.wald, PAFest.strata.wald = res$PAFest.strata.wald, 
                   chisq.strata = res$chisq.strata)
    if (outcome == "as.columns") {
      r1 <- c(a, b, N1, cIRiske.p, cOe.p)
      r2 <- c(c, d, N0, cIRisko.p, cOo.p)
      r3 <- c(M1, M0, M0 + M1, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Prevalence *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(a, c, M1)
      r2 <- c(b, d, M0)
      r3 <- c(N1, N0, N0 + N1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "case.control", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "case.control" & n.strata > 1) {
    massoc <- list(OR.strata.wald = res$OR.strata.wald, OR.strata.score = res$OR.strata.score, 
                   OR.strata.cfield = res$OR.strata.cfield, OR.strata.mle = res$OR.strata.mle, 
                   OR.crude.wald = res$OR.crude.wald, OR.crude.score = res$OR.crude.score, 
                   OR.crude.cfield = res$OR.crude.cfield, OR.crude.mle = res$OR.crude.mle, 
                   OR.mh.wald = res$OR.mh.wald, ARisk.strata.wald = res$ARisk.strata.wald, 
                   ARisk.strata.score = res$ARisk.strata.score, ARisk.crude.wald = res$ARisk.crude.wald, 
                   ARisk.crude.score = res$ARisk.crude.score, ARisk.mh.wald = res$ARisk.mh.wald, 
                   ARisk.mh.sato = res$ARisk.mh.sato, ARisk.mh.green = res$ARisk.mh.green, 
                   PARisk.strata.wald = res$PARisk.strata.wald, PARisk.strata.piri = res$PARisk.strata.piri, 
                   PARisk.crude.wald = res$PARisk.crude.wald, PARisk.crude.piri = res$PARisk.crude.piri, 
                   AFest.strata.wald = res$AFest.strata.wald, AFest.crude.wald = res$AFest.crude.wald, 
                   PAFest.strata.wald = res$PAFest.strata.wald, PAFest.crude.wald = res$PAFest.crude.wald, 
                   chisq.strata = res$chisq.strata, chisq.crude = res$chisq.crude, 
                   chisq.mh = res$chisq.mh, OR.homog = res$OR.homog)
    if (outcome == "as.columns") {
      r1 <- c(sa, sb, sN1, cIRiske.p, cOe.p)
      r2 <- c(sc, sd, sN0, cIRisko.p, cOo.p)
      r3 <- c(sM1, sM0, sM0 + sM1, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Prevalence *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(sa, sc, sM1)
      r2 <- c(sb, sd, sM0)
      r3 <- c(sN1, sN0, sN0 + sN1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "case.control", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "cross.sectional" & n.strata == 1) {
    massoc <- list(PR.strata.wald = res$RR.strata.wald, PR.strata.score = res$RR.strata.score, 
                   OR.strata.wald = res$OR.strata.wald, OR.strata.score = res$OR.strata.score, 
                   OR.strata.cfield = res$OR.strata.cfield, OR.strata.mle = res$OR.strata.mle, 
                   ARisk.strata.wald = res$ARisk.strata.wald, ARisk.strata.score = res$ARisk.strata.score, 
                   PARisk.strata.wald = res$PARisk.strata.wald, PARisk.strata.piri = res$PARisk.strata.piri, 
                   AFRisk.strata.wald = res$AFRisk.strata.wald, PAFRisk.strata.wald = res$PAFRisk.strata.wald, 
                   chisq.strata = res$chisq.strata)
    if (outcome == "as.columns") {
      r1 <- c(a, b, N1, cIRiske.p, cOe.p)
      r2 <- c(c, d, N0, cIRisko.p, cOo.p)
      r3 <- c(M1, M0, M0 + M1, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Prevalence *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(a, c, M1)
      r2 <- c(b, d, M0)
      r3 <- c(N1, N0, N0 + N1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cross.sectional", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  if (method == "cross.sectional" & n.strata > 1) {
    massoc <- list(PR.strata.wald = res$RR.strata.wald, PR.strata.score = res$RR.strata.score, 
                   PR.crude.wald = res$RR.crude.wald, PR.crude.score = res$RR.crude.score, 
                   PR.mh.wald = res$RR.mh.wald, OR.strata.wald = res$OR.strata.wald, 
                   OR.strata.score = res$OR.strata.score, OR.strata.cfield = res$OR.strata.cfield, 
                   OR.strata.mle = res$OR.strata.mle, OR.crude.wald = res$OR.crude.wald, 
                   OR.crude.score = res$OR.crude.score, OR.crude.cfield = res$OR.crude.cfield, 
                   OR.crude.mle = res$OR.crude.mle, OR.mh.wald = res$OR.mh.wald, 
                   ARisk.strata.wald = res$ARisk.strata.wald, ARisk.strata.score = res$ARisk.strata.score, 
                   ARisk.crude.wald = res$ARisk.crude.wald, ARisk.crude.score = res$ARisk.crude.score, 
                   ARisk.mh.wald = res$ARisk.mh.wald, ARisk.mh.sato = res$ARisk.mh.sato, 
                   ARisk.mh.green = res$ARisk.mh.green, PARisk.strata.wald = res$PARisk.strata.wald, 
                   PARisk.strata.piri = res$PARisk.strata.piri, PARisk.crude.wald = res$PARisk.crude.wald, 
                   PARisk.crude.piri = res$PARisk.crude.piri, AFRisk.strata.wald = res$AFRisk.strata.wald, 
                   AFRisk.crude.wald = res$AFRisk.crude.wald, PAFRisk.strata.wald = res$PAFRisk.strata.wald, 
                   PAFRisk.crude.wald = res$PAFRisk.crude.wald, chisq.strata = res$chisq.strata, 
                   chisq.crude = res$chisq.crude, chisq.mh = res$chisq.mh, 
                   PR.homog = res$RR.homog, OR.homog = res$OR.homog)
    if (outcome == "as.columns") {
      r1 <- c(sa, sb, sN1, cIRiske.p, cOe.p)
      r2 <- c(sc, sd, sN0, cIRisko.p, cOo.p)
      r3 <- c(sM1, sM0, sM1 + sM0, cIRiskpop.p, cOpop.p)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Outcome +", "   Outcome -", 
                         "     Total", "       Prevalence *", "       Odds")
      rownames(tab) <- c("Exposed +", "Exposed -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    if (outcome == "as.rows") {
      r1 <- c(sa, sc, sM1)
      r2 <- c(sb, sd, sM0)
      r3 <- c(sN1, sN0, sN0 + sN1)
      tab <- as.data.frame(rbind(r1, r2, r3))
      colnames(tab) <- c("   Exposed +", "   Exposed -", 
                         "     Total")
      rownames(tab) <- c("Outcome +", "Outcome -", "Total")
      tab <- format.data.frame(tab, digits = 3, justify = "right")
    }
    out <- list(method = "cross.sectional", n.strata = n.strata, 
                conf.level = conf.level, res = res, massoc = massoc, 
                tab = tab)
  }
  class(out) <- "epi.2by2"
  return(out)
}

  
