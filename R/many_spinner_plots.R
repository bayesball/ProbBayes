many_spinner_plots <- function(list_regions){
  n <- length(list_regions)
  if(n > 6) stop("Too many spinners to plot")
  Letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  plots <- vector(mode = "list", length = n)
  for(j in 1:n){
    plots[[j]] <- spinner_plot(list_regions[[j]],
                       title=paste("Spinner", Letters[j]))
  }
  if (n == 2) rows.cols <- c(1, 2) else
    if (n %in% c(3, 4)) rows.cols <- c(2, 2) else
      if (n %in% c(5, 6)) rows.cols <- c(3, 2)

  grid.arrange(grobs=plots,
        nrow=rows.cols[1], ncol=rows.cols[2])
}
