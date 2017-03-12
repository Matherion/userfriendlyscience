determinantStructure <-
  determinantVar <-
  subdeterminants <-
  subdeterminantProducts <- function(name,
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
      res <- as.Node(res);
      ### Arrows from children to parents
      SetEdgeStyle(res, dir='back');
      ### Plot from right to left
      SetGraphStyle(res, rankdir='RL');
      ### Doesn't work for some reason; this does
      res <- add_global_graph_attrs(res, "rankdir", "RL", "graph")
      ### Set class and return result
      class(res) <- c('determinantStructure', class(res));
      return(res);
    } else {
      return(res);
    }
  }
