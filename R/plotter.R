#' Plotter
#'
#' User submitted numeric dataframe of 4 columns is plotted as a line graph.
#'
#' @param c.table Dataframe of 4 columns consisting of positive numbers.
#'
#' @return Uses myseq_n function to plot the sequence of numbers of nth element.
#' @export
#'
#' @examples my_data <- tibble::tribble(
#' ~x, ~y, ~z, ~n,
#' 2,4,3,3,
#' 2,4,3,4,
#' 2,4,3,5,
#' 2,4,3,6,
#' 2,4,3,7,
#' 2,4,3,8,
#' 2,4,3,9,
#' 2,4,3,10,
#' 2,4,3,12)
plotter <- function(c.table) {
  n <- NULL
  sqnc <- NULL
  if (is.character(c.table)) stop("Enter dataframe with 4 numeric columns")
  if (ncol(c.table) != 4) stop("Dataframe columns must equal 4")
  colnames(c.table) <- c("x","y","z","n")
  sqnc_answer <- vector(mode = "numeric")
  for (i in 1:nrow(c.table)) {
    sqnc_answer[[i]] <- myseq_n(x = c(c.table$x[[i]], c.table$y[[i]],
                                      c.table$z[[i]]),
                                n = c.table$n[[i]])
  }
  c.table$sqnc <- round(sqnc_answer, digits = 3)
  mytitle <- c("My Sequence")
  plotting <- ggplot2::ggplot(c.table, mapping = aes(x = n, y = sqnc))+
    ggplot2::geom_line(color = "darkorchid4", size = 1.5, na.rm = TRUE)+
    ggplot2::theme_minimal()+
    ggplot2::labs(x = "nth Element", y = "Sequence Output",
                  title = mytitle)+
    ggplot2::theme(plot.title = element_text(hjust = 0.5, color = "black", size = 14))
  return(plotting)
  return(sqnc_answer)
}
