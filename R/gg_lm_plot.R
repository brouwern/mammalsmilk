#' gg_lm_plot
#' Code adapated from https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
#'
#' @export

gg_lm_plot <- function(mod,
                       xlab. = NULL,
                       ylab. = NULL,
                       rnd = 2,
                       annotate. = "summary"){

  require(ggplot2)
  require(cowplot)

  dat <- mod$model

  R2 <- signif(summary(mod)$r.squared, rnd)
  B0 <- signif(mod$coef[[1]],rnd )
  B1 <- signif(mod$coef[[2]], rnd)
  P  <- signif(summary(mod)$coef[2,4],rnd)
  summary. <- paste("R2 = ",R2,
              " B0 =",B0,
              " B1 =",B1,
              " P =",P)

  equation. <- paste(names(dat)[1],
                     " = ",B0,"+",B1,"*",
                     names(dat)[2])

  if(is.null(xlab.) == TRUE){
    xlab. <- names(dat)[2]
  }

  if(is.null(ylab.) == TRUE){
    ylab. <- names(dat)[1]
  }



  plot. <- ggplot2::ggplot(dat,
         aes_string(x = names(dat)[2],
                    y = names(dat)[1])) +
    geom_point() +
    stat_smooth(method = "lm",
                col = "red") +
    xlab(xlab.) +
    ylab(ylab.)

  if(annotate. == "summary"){
    plot. <- cowplot::add_sub(plot., summary.)
  }

  if(annotate. == "equation"){
    plot. <- cowplot::add_sub(plot., equation.)
  }

  cowplot::ggdraw(plot.)
}
