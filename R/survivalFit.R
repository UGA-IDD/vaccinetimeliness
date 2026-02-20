#' Fit a boubly-censored survival model
#'
#' @param datainput a data.frame similar to `getDhsdata()`
#' @param plotoutput logical, default is FALSE
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param ... additional arguments for `plot()`
#'
#' @return list, cumulative probabilities with confidence intervals
#' @export
#'


survivalFit <- function(datainput, plotoutput = FALSE, xlab = "", ylab = "", ...) {
  df1 <- datainput
  df1 <- df1 |>
    dplyr::mutate(
      prop = ifelse(Status %in% c(1, 2), 1, 0)
    )


  kefit <- survival::survfit(survival::Surv(followup, followup, Status, type = "interval") ~ 1,
    data = df1, weight = wgt
  )

  ## Crude Estimate -------------------------------------------------

  df1_cum <- df1 |>
    dplyr::arrange(followup) |>
    dplyr::summarise(
      vsum = sum(prop),
      n = dplyr::n(),
      propind = vsum / n,
      .by = followup
    )

  prop.time <- df1_cum$followup
  prop.vaccinated <- df1_cum$propind

  #--------------------------------------------------------------

  cdf.list <- list(
    time = kefit$time, surv.prob = kefit$surv,
    surv.low = kefit$lower,
    surv.high = kefit$upper
  )

  df1_range <- range(df1$followup)

  if (plotoutput == TRUE) {
    cdf.low <- kefit$lower
    cdf.high <- kefit$upper

    plot(cdf.list$time, 1 - cdf.list$surv.prob,
      type = "s", col = "red", ylim = c(0, 1),
      xlab = xlab, ylab = ylab, ...
    ) # turnbull cdf
    axis(side = 1, at = seq(0, df1_range[[2]], by = 5))
    lines(prop.time, prop.vaccinated, type = "b", pch = 16, col = "black") # crude estimate
    lines(cdf.list$time, 1 - cdf.low, type = "s", col = "blue", lty = 2)
    lines(cdf.list$time, 1 - cdf.high, col = "blue", type = "s", lty = 2)
  }

  return(cdf.list)
}
