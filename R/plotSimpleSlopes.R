# ## data     dataframe
# ## out     data frame with the parameter estimates from sem (Lavaan)
# ## xmmod   name of moderator of x - m path
# ## mymod   name of moderator of m - y path
# ## mvars   character vector with names of mediators (maximum = 3 mediators)
# ## xvar    name of predictor
# ## yvar    name of dependent variable
# ## cmvars  character vecotr with name sof covariates
# 
# plotSimpleSlopes <- function(data,
#                              out,
#                              xvar,
#                              mvars,
#                              yvar,
#                              xmmod = NULL,
#                              mymod = NULL,
#                              cmvars = NULL,
#                              cyvars = NULL) {
#   
#   ## test if moderator exists for x=m path and if it is dichotomous factor
#   if (length(xmmod)) {
#     xdichotomous <- FALSE
#     if (is.factor(data[,xmmod])) {  
#       if (length(levels(data[,xmmod])) > 2) { 
#         stop("This function can not yet plot moderation with a moderator (x-m path) that is a factor with more than two levels.");
#       }
#       else {
#         xmodLevels <- levels(data[,xmmod]);
#         data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
#         xdichotomous <- TRUE;
#       } 
#     }
#   }
#   
#   ## test if moderator exists for m=y path and if it is dichotomous factor
#   
#   if (length(mymod)) {
#     ydichotomous <- FALSE
#     if (is.factor(data[,mymod])) {  
#       if (length(levels(data[,mymod])) > 2) { 
#         stop("This function can not yet plot moderation with a moderator (x-y path) that is a factor with more than two levels.")}
#       else {
#         ymodLevels <- levels(data[,mymod]);
#         data[,mymod] <- as.numeric(data[,mymod]) - 1;
#         ydichotomous <- TRUE;
#       }
#     }
#   }
#   
#   ## compute minimum and maximum values for the plot axes
#   xmin <- min(data[,xvar], na.rm = TRUE);
#   xmax <- max(data[,xvar], na.rm = TRUE);
#   miny <- min(data[,yvar], na.rm = TRUE);
#   maxy <- max(data[,yvar], na.rm = TRUE);
#   
#   # compute simple slopes for x-m moderator 
#   
#   if (length(xmmod)) {
#     
#     if (xdichotomous) {
#       modmin <- 0;
#       modmax <- 1
#     } else {
#       modmin <- mean(data[,xmmod]) - sd(data[,xmmod]);
#       modmax <- mean(data[,xmmod]) + sd(data[,xmmod]);
#     }
#     
#     a <- subset(out, grepl("a1", out$label))[,"est"];
#     b <- subset(out, grepl("b", out$label))[,"est"];
#     w <- subset(out, grepl("w", out$label))[,"est"];
#     im <- subset(out, grepl("im", out$label))[,"est"]; 
#     
#     # loop over mediators
#     for (i in 1:length(mvars)) {
#       
#       incmin <- b[i]*w[i]*modmin;
#       slopemin <- a[i]*b[i] + b[i]*im[i]*modmin;
#       incmax  <- b[i]*w[i]*modmax;
#       slopemax <- a[i]*b[i] + b[i]*im[i]*modmax;
# 
#       cat( paste0("\n\n", "Simple slopes indirect effects through ", mvars[i]), "\n");
#       cat("---------------------------------------------","\n" );
#       cat("for 1 sd below mean of moderator: ", slopemin ,"\n");
#       cat("for 1 sd above mean of moderator: ", slopemax ,"\n\n");
# 
#       pred <- rep(0,4);
#       pred[1] <- incmin  + slopemin * xmin;
#       pred[2] <- incmin + slopemin * xmax;
#       pred[3] <- incmax  + slopemax * xmin;
#       pred[4] <- incmax  + slopemax * xmax;
#       
#       plotdat1 <-
#         as.data.frame(cbind(pred, c(xmin,xmax,xmin,xmax),
#                             c(modmin, modmin, modmax, modmax)));
#       
#       colnames(plotdat1) <- c(yvar, xvar, xmmod);
#       plotdat1[,xmmod] <- as.factor(plotdat1[,xmmod]);
#       
#       title <- paste0("Simple slopes in x-m path for indirect effect through ", mvars[i]);
#       if (xdichotomous) {
#         legendLabel <- xmodLevels;
#       } else {
#         legendLabel <- c("1 SD below mean", "1 SD above mean");
#       }
# 
#       g <- ggplot(plotdat1, aes(x=plotdat1[,xvar],
#                                 y=plotdat1[,yvar],
#                                 colour=plotdat1[,xmmod],
#                                 group = plotdat1[,xmmod])) +
#         geom_point() +
#         geom_line() +
#         labs(x = xvar, y = yvar) +
#         ylim(min(miny,min(plotdat1[,1])), max(maxy,max(plotdat1[,1]))) +
#         ggtitle(title) + 
#         theme(plot.title = element_text(lineheight=.8, face="bold")) +
#         scale_colour_discrete(name  = xmmod, labels=legendLabel);
#       
#       plot(g);
#       
#     } # end loop over mediators
#     
#   } # end loop over computation x-m simple slopes  
# 
#   # compute simple slopes for m-y moderator 
#   
#   if (length(mymod)) {
#     
#     if (ydichotomous) { modmin <- 0;
#     modmax <- 1 }
#     else {          modmin <- mean(data[,mymod]) - sd(data[,mymod])
#     modmax <- mean(data[,mymod]) + sd(data[,mymod]) }
#     a <- subset(out, grepl("a1", out$label))[,"est"] 
#     b <- subset(out, grepl("b", out$label))[,"est"] 
#     v <- subset(out, grepl("v", out$label))[,"est"] 
#     iy <- subset(out, grepl("iy", out$label))[,"est"] 
#     
#     
#     # loop over mediators
#     
#     for (i in 1:length(mvars)) {
#       
#       incmin <- v*modmin
#       slopemin <- a[i]*b[i] + a[i]*iy[i]*modmin
#       incmax  <- v*modmax
#       slopemax <- a[i]*b[i] + a[i]*iy[i]*modmax
#       
#       
#       cat( paste0("\n\n", "Simple slopes indirect effects through ", mvars[i]), "\n")
#       cat("---------------------------------------------","\n" )
#       cat("for 1 sd below mean of moderator: ", slopemin ,"\n") 
#       cat("for 1 sd above mean of moderator: ", slopemax ,"\n\n") 
#       
#       
#       pred <- rep(0,4)
#       pred[1] <- incmin  + slopemin * xmin
#       pred[2] <- incmin + slopemin * xmax
#       pred[3] <- incmax  + slopemax * xmin
#       pred[4] <- incmax  + slopemax * xmax
#       
#       plotdat1 <- as.data.frame(cbind(pred, c(xmin,xmax,xmin,xmax), c(modmin, modmin, modmax, modmax)))
#       colnames(plotdat1) <- c(yvar, xvar, mymod)
#       plotdat1[,mymod] <- as.factor(plotdat1[,mymod])
#       
#       
#       title <- paste0("Simple slopes in m-y path for indirect effect through ", mvars[i])
#       if (ydichotomous) {legendLabel <- ymodLevels }
#       else {legendLabel <- c("1 SD below mean", "1 SD above mean") }
#       
#       g <- ggplot(plotdat1,aes(x=plotdat1[,xvar],y=plotdat1[,yvar], colour=plotdat1[,mymod], group = plotdat1[,mymod])) + geom_point() + geom_line()
#       g <- g + labs(x = xvar, y = yvar)    + ylim(min(miny,min(plotdat1[,1])), max(maxy,max(plotdat1[,1]))) 
#       g <- g + ggtitle(title) + 
#         theme(plot.title = element_text(lineheight=.8, face="bold"))
#       g <- g + scale_colour_discrete(name  = mymod, labels=legendLabel)
#       plot(g)
#       
#     } # end loop over mediators
#     
#   } # end loop over computation m-y simple slopes  
#   
# } # end function plotSimpleSlopes
# 
# ## test
# 
# # plotSimpleSlopes(data = dat1, out=out, xvar="x1", mvars= mvars, yvar = "y1", xmmod = "bimod1")
# 
# 
