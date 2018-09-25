#' Determinant Structure specification
#'
#' These functions can be used to specify a determinant structure: a
#'   hierarchical structure of determinants that can then be
#'   conveniently plotted and analysed, for example using
#'   [detStructCIBER]. These functions are made to be used together;
#'   see the example and the forthcoming article for more information.
#'
#' This family of functions will be explained more in detail in a
#'   forthcoming paper.
#'
#'   `plot` and `print` methods plot and print a
#'   `determinantStructure` object.
#'
#'
#' @param name The name of the variable that is specified.
#' @param selection A regular expression to use to select the
#'   variables in a dataframe that are considered items that
#'   together form this variable. For `determinantStructure`, a
#'   list can be provided that also contains a named regular
#'   expression with the name 'behaviorRegEx', which specifies
#'   the name of the behavior to which this determinant structure
#'   pertains.
#' @param x The `determinantStructure` object to print or plot.
#' @param useDiagrammeR Whether to simply use `print(plot(x))`
#'   (if `FALSE`) or whether to use [data.tree::ToDiagrammeRGraph],
#'   tweak it a bit, by setting global graph attributes, and then
#'   using [DiagrammeR::render_graph] (if `TRUE`).
#' @param ... Any additional arguments are other determinant structure
#'   building functions. These are used to construct the determinant
#'   structure 'tree'.
#'
#' @return A `determinantStructure` object, which is a
#'   [data.tree] object.
#' @author Gjalt-Jorn Peters, \email{gjalt-jorn@@a-bc.eu}
#' @seealso \code{\link{detStructAddVarLabels}},
#'   \code{\link{detStructAddVarNames}},
#'   \code{\link{detStructComputeProducts}},
#'   \code{\link{detStructComputeScales}},
#'   \code{\link{detStructCIBER}}
#' @examples determinantStructure('using R',
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
#' @export

determinantStructure <-
  function(name,
           selection = NULL,
           ...) {
    
    type <- curfnfinder();
    
    res <- list(name = name,
                type = type);
    
    if (is.null(selection))
      selection <- name;
    
    if (is.list(selection) && 'behaviorRegEx' %in% names(selection)) {
      ### A behaviorRegEx is specified, as well
      res <- c(res, list(behaviorRegEx = selection$behaviorRegEx));
      selection <- as.character(selection[names(selection)!='behaviorRegEx']);
    }
    
    res <- c(res, list(selection = selection));
    
    if (length(list(...)) > 0) {
      res <- c(res, list(...));
    }
    
    if (type == 'determinantStructure') {
      res <- data.tree::as.Node(res);
      ### Arrows from children to parents
      data.tree::SetEdgeStyle(res, dir='back');
      ### Plot from right to left; note that this doesn't work for some reason;
      ### there's a 'solution' in the print method
      data.tree::SetGraphStyle(res, rankdir='RL');
      ### Set class and return result
      class(res) <- c('determinantStructure', class(res));
      return(res);
    } else {
      return(res);
    }
  }

#'@rdname determinantStructure
#'@export
determinantVar <- determinantStructure;

#'@rdname determinantStructure
#'@export
subdeterminants <- determinantStructure;

#'@rdname determinantStructure
#'@export
subdeterminantProducts <- determinantStructure;

#'@rdname determinantStructure
#'@export
plot.determinantStructure <- function(x, useDiagrammeR = FALSE, ...) {
  if (useDiagrammeR) {
    xGraph <- data.tree::ToDiagrammeRGraph(x, direction = "descend");
    xGraph <- DiagrammeR::add_global_graph_attrs(xGraph, "layout", "dot", "graph");
    xGraph <- DiagrammeR::add_global_graph_attrs(xGraph, "rankdir", "RL", "graph");
    DiagrammeR::render_graph(xGraph);
    invisible(xGraph);
  } else {
    class(x) <- 'Node';
    print(graphics::plot(x));
  }
}

#'@rdname determinantStructure
#'@export
print.determinantStructure <- function(x, ...) {
  # if (plot) {
  #   y <- plot(x);
  # }
  class(x) <- c('Node', 'R6');
  print(x, ...);
  class(x) <- c('determinantStructure', 'Node', 'R6');
  # if (plot) invisible(y);
}



# determinantStructure <- behaviorchange::determinantStructure;
# determinantVar <- behaviorchange::determinantVar;
# subdeterminants <- behaviorchange::subdeterminants;
# subdeterminantProducts <- behaviorchange::subdeterminantProducts;
# plot.determinantStructure <- behaviorchange:::plot.determinantStructure;
# print.determinantStructure <- behaviorchange:::print.determinantStructure;

