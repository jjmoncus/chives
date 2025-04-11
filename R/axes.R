

#' Helper for adjusting a continuous x axis
#'
#' @export
#'
#' @importFrom ggplot2 waiver scale_x_continuous
adjust_x_continuous <- function(end, end_nudge, mids = NULL, labels = waiver()) {

  if (is.null(mids)) mids <- end/2

  scale_x_continuous(
    # fixes the axes to meet at (0, 0)
    expand = c(0, 0),
    # ensures `end` is present on the line
    limits = c(0, end + end_nudge),
    # dictates which values get marked on the x axis
    # we default here to (0, end/2, end), but we should probably expand this
    breaks = c(0, mids, end),
    labels = labels)
}
