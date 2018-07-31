### Note: this is a more generic version of http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/
### All credits to Max Gordon



#' Automatic caption numbering knitr hooks for figures and tables
#' 
#' These function implement ideas by Max Gordon and DeanK (see Details) to add
#' \code{\link{knitr}} hooks to automate the numbering of figures and tables
#' when generating R Markdown documents.
#' 
#' The figure caption function is basically the one designed by Max Gordon (see
#' \url{http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/}.
#' 
#' The table caption function is an implementation of the ideas of DeanK (see
#' \url{http://stackoverflow.com/questions/15258233/using-table-caption-on-r-markdown-file-using-knitr-to-use-in-pandoc-to-convert-t})
#' combined with Max Gordon's function.
#' 
#' @aliases setFigCapNumbering setTabCapNumbering
#' @param captionName The name of the caption, used in the \code{\link{knitr}}
#' chunk options to provide the caption text.
#' @param figure_counter_str,table_counter_str The string in which to add the
#' number of the figure or table. The text '\%s' will be replaced by the
#' number.
#' @param figureClass Optionally, a css class to pass to the <fig> HTML element
#' that surrounds the <img>.
#' @param imgClass Optionall, a css class to pass to the <img> HTML element.
#' @param figureInlineStyle Any css style to pass to the figure element
#' directly ('inline').
#' @param imgInlineStyle Any css style to pass to the image element directly
#' ('inline').
#' @param optionName The name of the option to use to retrieve and set the
#' counter. This can be used, for example, to have multiple caption types use
#' the same counter.
#' @param resetCounterTo If not \code{NULL} and numeric, the counter will start
#' at this number.
#' @return Nothing is returned; the correct hooks are configured for
#' \code{\link{knitr}}.
#' @author Max Gordon (setFigCapNumbering) and DeanK (setTabCapNumbering);
#' implemented by Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{knitr}}
#' @keywords utils
#' @examples
#' 
#' \dontrun{
#'   setFigCapNumbering("This is figure number %s, with caption text: ");
#' }
#' 
#' @export setFigCapNumbering
setFigCapNumbering <- function(captionName = 'fig.cap',
                               figure_counter_str = "Figure %s: ",
                               figureClass = "",
                               imgClass = "",
                               figureInlineStyle = c("display:block"),
                               imgInlineStyle = NULL,
                               optionName = paste0('setCaptionNumbering_', captionName),
                               resetCounterTo = 1) {

  if (!is.null(resetCounterTo) && is.numeric(resetCounterTo)) {
    do.call('options', as.list(structure(resetCounterTo,
                                         names=optionName)));
  }

  knit_hooks$set(plot = function(x, options) {
    fig_fn = paste0(opts_knit$get("base.url"),
                    paste(x, collapse = "."));

    # Style and additional options that should be included in the img tag
    style<-c("display: block",
            sprintf("margin: %s;",
                    switch(options$fig.align,
                           left = 'auto auto auto 0',
                           center = 'auto',
                           right = 'auto 0 auto auto')));
    # Certain arguments may not belong in style,
    # for instance the width and height are usually
    # outside if the do not have a unit specified
    addon_args = "";

    # This is perhaps a little overly complicated prepared
    # with the loop but it allows for a more out.parameters if necessary
    if (any(grepl("^out.(height|width)", names(options)))){
      on <- names(options)[grep("^out.(height|width)", names(options))]
      for(out_name in on){
        dimName <- substr(out_name, 5, nchar(out_name));
        if (grepl("[0-9]+(em|px|%|pt|pc|in|cm|mm)", out_name))
          style=append(style, paste0(dimName, ": ", options[[out_name]]))
        else if (length(options$out.width) > 0)
          addon_args = paste0(addon_args, dimName, "='", options[[out_name]], "'");
      }
    }

    ### Get counter value, set to 1 if just set to TRUE or FALSE
    cntr <- getOption(optionName, 1);
    if (!is.numeric(cntr)) cntr <- 1;

    fig_number_txt <-
      sprintf(getOption("figure_counter_str", figure_counter_str),
              ifelse(getOption("figure_counter_roman", FALSE),
                     as.character(as.roman(cntr)), as.character(cntr)));

    ### Increment counter
    do.call('options', as.list(structure(cntr+1, names=optionName)));

    ### Prepare HTML stuff
    fullFigureClass <- ifelse(nchar(figureClass) > 0,
                              paste0("class='", figureClass, "'"), "");
    fullImgClass <- ifelse(nchar(imgClass) > 0,
                           paste0("class='", imgClass, "'"), "");

    fullFigureInlineStyle <- ifelse(is.null(figureInlineStyle), "",
                                    paste0("style = '",
                                           paste0(figureInlineStyle, collapse="; "),
                                           "' "));

    fullImgInlineStyle <- ifelse(is.null(imgInlineStyle), "",
                                 paste0("style = '",
                                        paste0(imgInlineStyle, collapse="; "),
                                        "' "));

    # Put it all together
    ### (Specified caption text is in options[[captionName]])
    return(paste0("<figure ", fullFigureClass, " ", fullFigureInlineStyle, ">",
                  "<img src='", fig_fn, "' ", fullImgClass, fullImgInlineStyle, addon_args,
                  " alt='", fig_number_txt, options[[captionName]], "' />\n",
                  "<figcaption>", fig_number_txt, options[[captionName]], "</figcaption></figure>\n"));
  });
}
