# Goal: share a legend between multiple plots that do not also share axes

# 1. create four plots p1, p2, p3 and p4
# 2. save the legend of p1 as a separate grob
# 3. strip the legends from the plots
# 4. draw the four plots and the legend below them


library(ggplot2)
library(gridExtra)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}



# example
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data=dsamp, colour=clarity)
p2 <- qplot(cut, price, data=dsamp, colour=clarity)
p3 <- qplot(color, price, data=dsamp, colour=clarity)
p4 <- qplot(depth, price, data=dsamp, colour=clarity)
grid_arrange_shared_legend(p1, p2, p3, p4)


# my example NOT WORKING --------------------
output <- vector(mode = "list", length = length(e6))
names(output) <- names(e6)

# create graphs with legends
for(i in startI:endI) {
  what <- names(e6[i])
  plot <- seed.univscatter4(e6, e6[i], e6$lineSimple, e6$lineComplete, e6$ExpName, what, "total", "umol g-1 DW", 0.1, 0)
  print(plot)
  output[[i]] <- plot
}

objects <- output[startI:endI] ## remove empty elements of the output list
alph <- objects[order(names(objects))] ## alphabetize the objects




grid_arrange_shared_legend(alph[[1]], alph [[2]], ncol = 2, position = "right") # works great!
grid_arrange_shared_legend(alph[[1]], alph [[2]], alph[[3]], alph [[4]], ncol = 4, position = "bottom") # works great!
# but still passing graphs individually

grid_arrange_shared_legend(alpha, ncol = 4, position = "bottom") # doesn't work

unlist.alpha <- unlist(alpha, recursive = FALSE, use.names = TRUE)
grid_arrange_shared_legend(unlist.alpha, ncol = 4, position = "bottom") # doesn't work




