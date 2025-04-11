#' For setting up y variable in some ggplot functions
#'
#' @keywords internal
#'
#' @importFrom forcats fct_rev
y_setup <- function(y, sort, sort_by) {
  quo_y <- enquo(y)
  quo_by <- enquo(sort_by)

  if (sort) y_expr <- expr(reorder(!!quo_y, !!quo_by)) else
    y_expr <- expr(fct_rev(!!quo_y))

  return(y_expr)
}

#' A generic template for a layer of "bars + data labels + neat y-axis"
#'
#' `label_pos` represents a single unit movement of the label along the
#' x-axis. Bear in mind that x-axes with small units (i.e. from 0 to 1) will
#' need only small nudges, while larger units (i.e. 0 to 100) will need larger
#' ones.
#'
#' @export
#'
#' @importFrom rlang enquo
#' @importFrom ggplot2 geom_col geom_text aes scale_y_discrete
#' @importFrom stringr str_wrap
#'
normal_bars <- function(
  x,
  y,
  sort = TRUE,
  bar_width = 1,
  bar_fill = "#002856",
  label_color = "#000000",
  label_weight = "plain",
  label_pos = 10,
  y_wrap_width = 20,
  label_size = 12
) {
  quo_x <- enquo(x)
  quo_y <- enquo(y)

  y_expr <- y_setup(!!quo_y, sort = sort, sort_by = !!quo_x)

  list(
    geom_col(
      aes(x = !!quo_x, y = eval(y_expr)),
      fill = bar_fill,
      width = bar_width
    ),
    geom_text(
      aes(y = !!quo_y, x = !!quo_x + label_pos, label = paste0(!!quo_x, "%")),
      color = label_color,
      size = label_size / .pt,
      fontface = label_weight
    ),
    scale_y_discrete(labels = function(x) str_wrap(x, width = y_wrap_width))
  )
}


#' A bars within bars chart
#'
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom ggplot2 labs
bars_in_bars <- function(
  data,
  x_outer,
  x_inner,
  y,
  outer_fill = "#002856",
  outer_label_pos = 35,
  outer_width = .9,
  inner_fill = "#009AD7",
  inner_label_pos = 15,
  inner_width = .75,
  y_wrap_width = 20,
  text_size = 12,
  bar_width = .75,
  x_end = NULL,
  x_end_nudge = 1,
  x_mids = NULL
) {
  quo_x_outer <- enquo(x_outer)
  quo_x_inner <- enquo(x_inner)
  quo_y <- enquo(y)

  if (is.null(x_end)) {
    # round down to nearest multiple of 10
    x_max <- data %>% pull(!!quo_x_outer) %>% max()
    remainder <- x_max %% 10
    x_end <- x_max - remainder
  }

  if (is.null(x_end_nudge)) x_end_nudge <- 1
  if (is.null(x_mids)) x_mids <- x_end / 2

  ggplot(data) +
    normal_bars(
      x = !!quo_x_outer,
      y = !!quo_y,
      sort = TRUE,
      bar_fill = outer_fill,
      label_color = "black",
      label_pos = outer_label_pos,
      label_size = text_size,
      y_wrap_width = y_wrap_width,
      bar_width = outer_width
    ) +
    normal_bars(
      x = !!quo_x_inner,
      y = !!quo_y,
      sort = TRUE,
      bar_fill = inner_fill,
      label_color = "white",
      label_pos = inner_label_pos,
      label_size = text_size,
      y_wrap_width = y_wrap_width,
      bar_width = inner_width
    ) +
    labs(x = NULL, y = NULL) +
    theme_priorities_poll(text_size = text_size) +
    adjust_x_continuous(end = x_end, end_nudge = x_end_nudge, mids = x_mids)
}
