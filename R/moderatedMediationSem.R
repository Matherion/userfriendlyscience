# 
# #####
# ##### Function mediationSem() is build to test Mediation for one predictor and one dependent variable (as in PROCESS, see Hayes(2013))
# ##### Based on LAVAAN (Rosseel, 2012)
# ##### In funcion buildModel2() the model for input in LAVAAN is constructed
# #####
# 
# buildModMedSemModel <- function(xvar,
#                                 mvars,
#                                 yvar,
#                                 xmmod = NULL,
#                                 mymod = NULL,
#                                 cmvars = NULL,
#                                 cyvars = NULL) {
# 
#   nm <- length(mvars);
#   ncm <- length(cmvars);
#   ncy <- length(cyvars);
# 
#   ### first index predictor in x - m path (= 1 because only one predictor)
#   a1 <- 1:length(xvar);
# 
#   ### second indices mediators for x - m paths
#   a2 <- 1:nm;
# 
#   ### second index predictor in m - y path (= 1 because only one dependent)
#   b2 <- 1:length(yvar);
# 
#   ### first indices mediators for m - y paths
#   b1 <- a2;
# 
#   c1 <- 1:length(cmvars);
#   c2 <- 1:length(cyvars);
#   h <- as.vector(outer(1:nm, 1:ncm, paste0));
# 
#   ######################################################################
#   ### naming the parameters
#   ######################################################################
# 
#   ### path from x to m
#   a <- paste0("a",a1,a2);
# 
#   ### path from moderator*x to m
#   if (!is.null(xmmod)) {
#     w1 <- paste0("w",a2); w2 <- paste0("im",a2);
#   }
# 
#   ### path from m to y
#   b <- paste0("b",b1,b2);
# 
#   ### path from moderator*m to y
#   if(!is.null(mymod)) {
#     v1 <- paste0("v",1);
#     v2 <- paste0("iy",a2);
#   }
# 
#   ### path from x to y
#   c <- paste0("c",a1,b2);
# 
#   ### path from c1 to m
#   d <- paste0("d",h);
# 
#   ### path from c2 to y
#   f <- paste0("f",c2,b2);
# 
#   ### construct covariances between covariates for M
#   model_cov1 <- " ";
#   if (length(cmvars) > 1) {
#     ha <- expand.grid(cmvars,cmvars);
#     hb <- matrix(data=c(1:(ncm**2)),nrow=ncm,ncol=ncm);
#     s <- as.vector(lower.tri(hb));
#     ha <- ha[s,];
#     model_cov1 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ");
#   }
# 
#   ### construct covariances between mediators and covariates for Y
#   model_cov2 <- " ";
#   vars <- c(mvars, cyvars);
#   if (length(vars) > 1) {
#     nmy <- nm + ncy;
#     ha <- expand.grid(vars,vars);
#     hb <- matrix(data=c(1:(nmy**2)),nrow=nmy,ncol=nmy);
#     s <- as.vector(lower.tri(hb));
#     ha <- ha[s,];
#     model_cov2 <- paste0(ha[,1], " ~~ " , ha[,2], collapse = " \n ");
#   }
# 
#   ### indirect effects
#   ind <- paste0("ind", a2 );
#   modmedx <- paste0("modmedx", a2 );
#   modmedm <- paste0("modmedm", a2 );
# 
#   ### initialize path from mod on x - m path
#   modela2 <- " ";
# 
#   ### initialize path from mod on m - y path
#   modelb2 <- " ";
# 
#   ### initialize path from mod on  m path
#   modelw <- " ";
# 
#   ### initialize path from int on m path
#   modelw2 <- " ";
# 
#   ### initialize path from mod on  m path
#   modelv <- " ";
# 
#   ### initialize path from int on m path
#   modelv2 <- " "
# 
#   ### initialize indirect effects with mod x on  m path
#   modeli1 <- " ";
# 
#   ### initialize indirect effects with mod m on y path
#   modeli2 <- " ";
# 
#   #### initialize path from c1 to m
#   modeld <- " ";
# 
#   ### initialize path from c2 to y
#   modelf <- " ";
# 
#   ### construct interaction terms
# 
#   if(!is.null(xmmod)) {
#     xmint <- paste0("xmInteraction",c(1:nm));
#   }
#   if(!is.null(mymod)) {
#     myint <- paste0("myInteraction",c(1:nm));
#   }
# 
#   modela1 <- paste0(mvars, " ~ " ,a,"*",xvar ,  collapse = " \n ");
# 
#   if(!is.null(xmmod)) {
#     modelw <- paste0( mvars, " ~ " ,w1,"*",xmmod ,  collapse = " \n ");
#     modelw2 <- paste0( mvars, " ~ " ,w2,"*",xmint ,  collapse = " \n ");
#   }
# 
#   modelb1 <- paste0( yvar, " ~ " ,b,"*",mvars , collapse = " \n ");
# 
#   if(!is.null(mymod)) {
#     modelv <- paste0( yvar, " ~ " ,v1,"*",mymod , collapse = " \n ");
#     modelv2 <- paste0( yvar, " ~ " ,v2,"*",myint , collapse = " \n ");
#   }
# 
#   modelc <- paste0( yvar, " ~ " ,c,"*",xvar , collapse = " \n ");
# 
#   if (!is.null(cmvars)) {
#     modeld <- paste0( rep(mvars,ncm), " ~ " ,d,"*",rep(cmvars, each=nm) , collapse = " \n ");
#   }
#   if (!is.null(cyvars)) {
#     modelf <- paste0( yvar, " ~ " ,f,"*",cyvars , collapse = " \n ");
#   }
# 
#   modeli <- paste0(ind , " := " , a, " * ", b, collapse = " \n ");
# 
#   if(!is.null(xmmod)) {
#     modeli1 <- paste0(modmedx , " := " , w2, " * ", b, collapse = " \n ");
#   }
#   if(!is.null(mymod)) {
#     modeli2 <- paste0(modmedm , " := " , v2, " * ", a, collapse = " \n ");
#   }
# 
#   modelt <- paste0("total"," := " , (paste0(ind,  collapse = " + ")));
# 
#   model <- paste0(modela1," \n ",modela2," \n ",
#                   modelb1," \n ", modelb2," \n ",
#                   modelc, " \n ", modeld, " \n ",
#                   modelw, " \n ", modelw2, " \n ",
#                   modelv, " \n ", modelv2, " \n ",
#                   modelf, " \n ",
#                   model_cov1," \n ", model_cov2, " \n ",
#                   modeli, " \n ", modeli1, " \n ", modeli2, " \n ",
#                   modelt);
# 
#   return(model)
# }
# 
# 
# 
# 
# 
# 
# 
# moderatedMediationSem <- function(data = NULL,
#                                   xvar,
#                                   mvars,
#                                   yvar,
#                                   xmmod = NULL,
#                                   mymod = NULL,
#                                   cmvars = NULL,
#                                   cyvars = NULL,
#                                   plot=FALSE,
#                                   nboot=1000) {
# 
#   res <- list(input = as.list(environment()),
#               intermediate = list(),
#               output = list());
# 
#   res$intermediate$dataName <- as.character(deparse(substitute(data)));
# 
#   res$intermediate$numberOfMediators <-
#     nm <- length(mvars);
# 
#   ## check if there is a moderator for the x - m path
#   if (!is.null(xmmod)) {
#     if (length(xmmod) > 1) {
#       stop("This function can only handle one moderator for the x-m path. You provided ", length(xmmod),
#            " (argument 'xmmod' contained ", vecTxtQ(xmmod), ").");
#     }
#     if (is.factor(data[,xmmod])) {
#       if (nlevels(data[,xmmod]) > 2) {
#         stop("This function can not yet deal with categorical moderators with more than two levels.");
#       } else {
#         res$intermediate$xdichotomous <-
#           xdichotomous <- TRUE;
#         data[,"xmodOriginal"] <- data[,xmmod];
#         data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
#       }
#     } else {
#       res$intermediate$xdichotomous <-
#         xdichotomous <- FALSE;
#     }
# 
#     xmint <- paste0("xmInteraction",c(1:nm));
#     xmterms <- paste0(paste0("data$",xmmod,"*","data$",mvars));
# 
#     for (i in 1:nm) {
#       data[,xmint[i]] <- eval(parse(text = xmterms[i]));
#     }
# 
#   }
# 
#   ### check if there is a moderator for the m - y path;
#   if (!is.null(mymod)) {
#     if (length(mymod) > 1) {
#       stop("This function can only handle one moderator for the m-y path. You provided ", length(mymod),
#            " (argument 'mymod' contained ", vecTxtQ(mymod), ").");
#     }
#     if (is.factor(data[,mymod])) {
#       if (nlevels(data[,mymod]) > 2) {
#         stop("This function can not yet deal with categorical moderators with more than two levels.");
#       }
#       else {
#         res$intermediate$ydichotomous <-
#           ydichotomous <- TRUE;
#         data[,"ymodOriginal"] <- data[,mymod];
#         data[,mymod] <- as.numeric(data[,mymod]) - 1;
#       }
#     } else {
#       res$intermediate$ydichotomous <-
#         ydichotomous <- FALSE;
#     }
# 
#     myint <- paste0("myInteraction",c(1:nm));
#     myterms <- paste0(paste0("data$",mymod,"*","data$",mvars));
# 
#     for (i in 1:nm) {
#       data[,myint[i]] <- eval(parse(text = myterms[i] ));
#     }
#   }
# 
#   ### Build lavaan model
#   res$intermediate$model <-
#     buildModMedSemModel(xvar=xvar,
#                         mvars= mvars,
#                         yvar = yvar,
#                         xmmod = xmmod,
#                         mymod = mymod,
#                         cmvars = cmvars,
#                         cyvars = cyvars);
# 
#   ### Run SEM
#   res$intermediate$res <-
#     sem(model,
#         data=data,
#         fixed.x = FALSE,
#         std.lv = TRUE,
#         se="bootstrap",
#         bootstrap=nboot);
# 
#   ### Extract R squared values
#   res$output$Rsq <-
#     inspect(res$intermediate$res, "r2");
# 
#   ### Extract parameter estimates for direct effects
#   res$intermediate$parameterEstimates <-
#     parameterestimates(res);
#   res$output$parameterEstimates.direct <-
#     filter(parameterestimates(res),
#            lhs %in% yvar & rhs %in% xvar)[, -c(1:3)];
# 
#   ### ... And for indirect effects
#   a2 <- 1:nm;
#   ind <- paste0("ind", a2 );
#   indinter <- paste0("indinter", a2 );
#   res$output$parameterEstimates.indirect.raw <-
#     filter(parameterestimates(res),
#            lhs %in% c(ind,indinter, "total"))[, -c(1:3)];
# 
#   ### ... And the standardized indirect effects
#   res$output$parameterEstimates.indirect.standardized <-
#     lavInspect(res, "std")$beta[yvar,mvars] * lavInspect(res, "std")$beta[mvars, xvar];
# 
#   if (plot) {
#     if (xdichotomous) {
#       data[,xmmod] <- data[,"xmodOriginal"];
#     }
#     if (ydichotomous) {
#       data[,mymod] <- data[,"ymodOriginal"];
#     }
#     res$output$plot <-
#       plotSimpleSlopes(data = data,
#                        out=out,
#                        xvar=xvar,
#                        mvars= mvars,
#                        yvar = yvar,
#                        xmmod = xmmod,
#                        mymod = mymod,
#                        cmvars = cmvars,
#                        cyvars = cyvars);
#   }
# 
#   class(res) <- "moderatedMediationSem";
# 
#   return(res);
# 
# }
# 
# print.moderatedMediationSem <- function(x, ..., digits=2) {
# 
#   cat("### R square:\n\n");
#   print(x$output$Rsq);
# 
#   cat("### Direct effect:\n\n");
#   print(x$output$parameterEstimates.direct);
# 
#   cat("### Indirect effects (raw):\n\n");
#   print(x$output$parameterEstimates.indirect.raw);
# 
#   cat("### Indirect effects (standardized):\n\n");
#   print(x$output$parameterEstimates.indirect.standardized);
# 
#   if (!is.null(x$output$plot)) {
#     grid.newpage();
#     grid.draw(x$output$plot);
#   }
# 
# }
# 
# 
# 
# ###### test
# 
# options(digits = 2)
# require(lavaan);
# require(userfriendlyscience);
# require(MASS);
# require(dplyr)
# require(ggplot2)
# 
# ### construct test data
# set.seed(1);
# 
# dat1 <- simDataSet(300,varNames=c("y", "x","m1","m2","m3","c1","c2","mod1","mod2"))
# dat1$bimod1 <- factor((dat1$y > .1 & dat1$x > .1)*1)
# dat1$bimod2 <- dat1$bimod1; levels(dat1$bimod2) <- c("Male", "Female")
# dat1$x1 <- (dat1$x - 1) * 3.0
# dat1$m1 <- .4*dat1$m1  + .4*dat1$x + .2*rnorm(300, mean=0, sd=1)
# dat1$y1 <- .4*(dat1$m1 * dat1$mod2) + .4*dat1$m1+ .2*rnorm(300, mean=0, sd=1)
# 
# model <- buildModMedSemModel(xvar="x1", mvars= c("m1","m2","m3"), yvar = "y1", xmmod = "bimod1")
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "mod1", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "bimod1", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "bimod2", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", mymod = "mod1", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", mymod = "bimod1", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", mymod = "bimod2", plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1",  plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", cmvars =c("c1","c2"), cyvars =c("c1","c2"),
#                              plot = TRUE, nboot=50)   ## equals mediationSem()
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "mod1", cmvars =c("c1","c2"), plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "bimod2", cmvars =c("c1","c2"), cyvars =c("c1","c2"),
#                              plot = TRUE, nboot=50)
# 
# res <- moderatedMediationSem(dat = dat1, xvar="x1", mvars= c("m1","m2","m3"),
#                              yvar = "y1", xmmod = "bimod2", mymod= "mod1",
#                              cmvars =c("c1","c2"), cyvars =c("c1","c2"),
#                              plot = TRUE, nboot=50)
# 
# 
# summary(res, fit.measures=TRUE, rsq=TRUE, standardized = TRUE)
# 
# out <- parameterestimates(res)
# 
