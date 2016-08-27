dualplot <- function(x1, y1, y2, x2 = x1, 
                     col = c("#C54E6D", "#009380"),
                     lwd = c(1, 1), colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = paste(substitute(y1), collapse = ""), 
                     ylab2 = paste(substitute(y2), collapse = ""),
                     nxbreaks = 5, 
                     yleg1 = paste(gsub("\n$", "", ylab1), "(left axis)"), 
                     yleg2 = paste(ylab2, "(right axis)"),
                     ylim1 = NULL, ylim2 = NULL, ylim.ref = NULL,
                     xlab = "", main = NULL, legx = "topleft", legy = NULL, 
                     silent = FALSE, bty = "n", ...){
   # Base graphics function for drawing dual axis line plot.
   # Assumed to be two time series on a conceptually similar, non-identical scale 
   #
   # Assumes data is in sequence of x1 and of x2 ie ordered by time
   #
   # Use with caution! 
   # Please don't use to show growth rates and the original
   # series at the same time!
   #
   # Peter Ellis, 16-27 August 2016, GNU GPL-3
   # most parameters should be obvious:
   # x1 and y1 are the x and y coordinates for first line
   # x2 and y2 are the x and y coordinates for second line.  Often x2 will == x1, but can be overridden
   # ylim1 and ylim2 are the vertical limits of the 2 axes.  Recommended NOT to use these, as
   #    the default algorithm will set them in a way that makes the axes equivalent to using an index (for 
   #    positive data) or mean of each series +/- 3 standard deviations (if data include negatives)
   # ylim.ref the two numbers in the two series to use as the reference point for converting them to indices
   #    when drawing on the page.  If both elements are 1, both series will start together at the left of the plot.
   # nbreaks is number of breaks in horizontal axis
   # lwd and mar are graphics parameters (see ?par)
   # colgrid is colour of gridlines; if NULL there are no gridlines
   # ylab1 and ylab2 are the labels for the two y axes
   # yleg1 and yleg2 are the labels for the two series in the legend
   # xlab and main are for x label and main title as in plot()
   # legx and legy are x and y position fed through to legend()
   # ... is parameters to pass to legend()
   # Note that default colours were chosen by colorspace::rainbow_hcl(2, c = 80, l = 50)
   
   # strip excess attributes (eg xts etc) from the two vertical axis variables
   ylab1 <- as.character(ylab1)
   ylab2 <- as.character(ylab2)
   y1 <- as.numeric(y1)
   y2 <- as.numeric(y2)
   
   # is ylim.ref is NULL, calculate a good default
   if(is.null(ylim.ref)){
      if (length(y1) == length(y2)){
         ylim.ref <- c(1, 1)
      } else {
         if (min(x1) >  min(x2)){
            ylim.ref <- c(1, which(abs(x2 - min(x1)) == min(abs(x2 - min(x1)))))
         } else {
            ylim.ref <- c(which(abs(x1 - min(x2)) == min(abs(x1 - min(x2)))), 1)
         }
      }

      
   }

   
   oldpar <- par(mar = mar)
   xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
   
   # unless ylim1 or ylim2 were set, we set them to levels that make it equivalent
   # to a graphic drawn of indexed series (if all data positive), or to the mean
   # of each series +/- three standard deviations if some data are negative
   if(is.null(ylim1) & is.null(ylim2)){
      if(min(c(y1, y2), na.rm = TRUE) < 0){
         message("With negative values ylim1 or ylim2 need to be chosen by a method other than treating both series visually as though they are indexed. Defaulting to mean value +/- 3 times the standard deviations.")
         ylim1 <- c(-3, 3) * sd(x1, na.rm = TRUE) + mean(x1, na.rm = TRUE)
         ylim2 <- c(-3, 3) * sd(x2, na.rm = TRUE) + mean(x2, na.rm = TRUE)
      }
      
      
      if(ylim.ref[1] > length(y1)){
         stop("ylim.ref[1] must be a number shorter than the length of the first series.")
      }
      if(ylim.ref[2] > length(y2)){
         stop("ylim.ref[2] must be a number shorter than the length of the second series.")
      }
      
      if(!silent) message("The two series will be presented visually as though they had been converted to indexes.")
      
      # convert the variables to indexes (base value of 1 at the time specified by ylim.ref)
      ind1 <- as.numeric(y1) / y1[ylim.ref[1]]
      ind2 <- as.numeric(y2) / y2[ylim.ref[2]]
      
      # calculate y axis limits on the "index to 1" scale
      indlimits <- range(c(ind1, ind2), na.rm = TRUE)
      
      # convert that back to the original y axis scales
      ylim1 = indlimits * y1[ylim.ref[1]]
      ylim2 = indlimits * y2[ylim.ref[2]]
   } else {
      if(!silent) warning("You've chosen to set at least one of the vertical axes limits manually.  Up to you, but it is often better to leave it to the defaults.")
   }
   
   # draw first series - with no axes.
   plot(x1, y1, type = "l", axes = FALSE, lwd = lwd[1],
        xlab = xlab, ylab = "", col = col[1], main = main, 
        xlim = range(xbreaks), ylim = ylim1)
   
   # add in the gridlines if wanted:
   if(!is.null(colgrid)){
      grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
      abline(v = xbreaks, col = colgrid)
   }
   
   # add in the left hand vertical axis and its label
   axis(2, col = col[1], col.axis= col[1], las=1 )  ## las=1 makes horizontal labels
   mtext(paste0("\n", ylab1, "\n"), side = 2, col = col[1], line = 1.5) 
   
   # Allow a second plot on the same graph
   par(new=TRUE)
   
   # Plot the second series:
   plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd[2],
        col = col[2], xlim = range(xbreaks), ylim = ylim2)
   
   ## add second vertical axis (on right) and its label
   mtext(paste0("\n", ylab2, "\n"), side = 4, col = col[2], line = 4.5) 
   axis(4,  col = col[2], col.axis = col[2], las=1)
   
   # Draw the horizontal time axis
   axis(1, at = xbreaks, labels = xbreaks)
   
   # Add Legend
   legend(x = legx, y = legy, legend=c(yleg1, yleg2),
          text.col = col, lty = c(1, 1), lwd = lwd, col = col,
          bty = bty, ...)
   
   par(oldpar)
}