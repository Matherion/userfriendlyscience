
# setClass("fHTEST", list(call = structure("call", package = "methods"), 
#     data = structure("list", package = "methods"), test = structure("list", package = "methods"), 
#     title = structure("character", package = "methods"), description = structure("character", package = "methods")));

# new("classRepresentation", slots = list(call = structure("call", package = "methods"), 
#     data = structure("list", package = "methods"), test = structure("list", package = "methods"), 
#     title = structure("character", package = "methods"), description = structure("character", package = "methods")), 
#     contains = list(), virtual = FALSE, prototype = <S4 object of class NULL>, 
#     validity = NULL, access = list(), className = structure("fHTEST", package = "fBasics"), 
#     package = "fBasics", subclasses = list(), versionKey = <pointer: (nil)>, 
#     sealed = FALSE)

# adTest_adapted_from_Fbasics <- function (x, title = NULL, description = NULL) {
#     DNAME = deparse(substitute(x))
#     if (class(x) == "fREG") 
#         x = residuals(x)
#     x = as.vector(x)
#     call = match.call()
#     x = sort(x)
#     n = length(x)
#     if (n < 8) 
#         stop("sample size must be greater than 7")
#     var.x <- var(x)
#     if (var.x > 0) {
#         p = pnorm((x - mean(x))/sqrt(var.x))
#         h = (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
#         h = h[is.finite(h)]
#         n = length(h)
#         A = -n - mean(h)
#         AA = (1 + 0.75/n + 2.25/n^2) * A
#         if (AA < 0.2) {
#             PVAL = 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
#         }
#         else if (AA < 0.34) {
#             PVAL = 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
#         }
#         else if (AA < 0.6) {
#             PVAL = exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
#         }
#         else {
#             PVAL = exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
#         }
#         if (PVAL > 1) {
#             PVAL = 1
#             W = NA
#         }
#     }
#     else {
#         A <- Inf
#         PVAL <- 0
#     }
#     names(PVAL) = ""
#     test = list(statistic = c(A = A), p.value = PVAL, method = "Anderson - Darling Normality Test", 
#         data.name = DNAME)
#     if (is.null(title)) 
#         title = "Anderson - Darling Normality Test"
#     if (is.null(description)) 
#         description = paste(as.character(date()), "by user:", Sys.getenv("USERNAME"));
#                       #description()
# 
# # Making it work as regular R function sans class    
# #    ans = new("fHTEST", call = call, data = list(x = x), test = test, 
# #        title = as.character(title), description = as.character(description))
#   return(test);
# }
