#' Determinant Structure specification
#' 
#' These functions can be used to specify a determinant structure: a
#' hierarchical structure of determinants that can then be conveniently plotted
#' and analysed, for example using \code{\link{detStructCIBER}}.
#' 
#' These functions are made to be used together; see the example and the
#' forthcoming article for more information.
#' 
#' This family of functions will be explained more in detail in a forthcoming
#' paper.
#' 
#' \code{plot} and \code{print} methods plot and print a determinantStructure
#' object.
#' 
#' @aliases determinantStructure determinantVar subdeterminants
#' subdeterminantProducts plot.determinantStructure print.determinantStructure
#' @param name The name of the variable that is specified.
#' @param selection A regular expression to use to select the variables in a
#' dataframe that are considered items that together form this variable. For
#' \code{determinantStructure}, a list can be provided that also contains a
#' named regular expression with the name 'behaviorRegEx', which specifies the
#' name of the behavior to which this determinant structure pertains.
#' @param x The \code{determinantStructure} object to print or plot.
#' @param useDiagrammeR Whether to simply use \code{print(plot(x))} (if
#' \code{FALSE}) or whether to use \code{\link{ToDiagrammeRGraph}}, tweak it a
#' bit, by setting global graph attributes, and then using
#' \code{\link{render_graph}} (if \code{TRUE}).
#' @param \dots Any additional arguments are other determinant structure
#' building functions. These are used to construct the determinant structure
#' 'tree'.
#' @return A determinantStructure object, which is a \code{\link{data.tree}}
#' object.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{detStructAddVarLabels}},
#' \code{\link{detStructAddVarNames}}, \code{\link{detStructComputeProducts}},
#' \code{\link{detStructComputeScales}}, \code{\link{detStructCIBER}}
#' @references (Forthcoming)
#' @keywords misc
#' @examples
#' 
#' determinantStructure('using R',
#'                      list('using R',
#'                           behaviorRegEx = 'some RegEx'),
#'                      determinantVar("Intention",
#'                                     "another RegEx",
#'                                     determinantVar("Attitude",
#'                                                    "third RegEX",
#'                                                    subdeterminants("Likelihood",
#'                                                                    "4th RegEx"),
#'                                                   subdeterminants("Evaluation",
#'                                                                   "5th RegEx"),
#'                                                   subdeterminantProducts("attProduct",
#'                                                                          c("4th RegEx",
#'                                                                            "5th RegEx"))),
#'                                     determinantVar("perceivedNorm",
#'                                                    "6th RegEx",
#'                                                    subdeterminants("Approval",
#'                                                                    "7th RegEx"),
#'                                                    subdeterminants("Motivation to comply",
#'                                                                    "8th RegEx"),
#'                                                    subdeterminantProducts("normProduct",
#'                                                                           c("7th RegEx",
#'                                                                             "8th RegEx"))),
#'                                               determinantVar("pbc",
#'                                                              "9th RegEx",
#'                                                              subdeterminants("Control beliefs",
#'                                                                              "10th RegEx"))));
#' 
#' @export determinantStructure
determinantStructure <- behaviorchange::determinantStructure;
determinantVar <- behaviorchange::determinantVar;
subdeterminants <- behaviorchange::subdeterminants;
subdeterminantProducts <- behaviorchange::subdeterminantProducts;

plot.determinantStructure <- behaviorchange::plot.determinantStructure;

print.determinantStructure <- behaviorchange::print.determinantStructure;

