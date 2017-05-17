### Note: this is a more generic version of http://gforge.se/2014/01/fast-track-publishing-using-knitr-part-iii/
### All credits to Max Gordon

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
