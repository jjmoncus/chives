
#' A dotplot displaying change over time
#'
#' This differs from other dotplots in that dots are connected by an arrow
#' conveying direction from "from" to "to"
#'
#' @param label_spread How far away from the dots you would like
#' corresponding data labels to sit
#' @param dot_size The size of dots
#' @param sort The name of the variable to sort y-axis labels by. Can be any
#' variable passed to `to` or `from`, the name `change` (if
#' `change_col = TRUE`), or `NULL`, in which case it sorts by the default
#' for `y`'s data type.
#'
#' Need to include an error message for when sort = change and
#' change_col = FALSE
#'
#' @importFrom rlang quo_is_null
#'
#' @export

change_dotplot <- function(data, y, from, to,
                           change_col = TRUE,
                           sort_by = `change`,
                           change_col_x = 8,
                           dot_size = 3,
                           label_spread = 3.5,
                           x_lims = c(0, 110),
                           x_breaks = c(0, 50, 100)) {

  quo_y <- enquo(y)
  quo_from <- enquo(from)
  quo_to <- enquo(to)
  quo_sort_by <- enquo(sort_by)

  if (change_col) {
    data <- data %>%
      mutate(change = !!quo_to - !!quo_from)
  }

  # if anything but NULL, we indeed sort
  do_we_sort <- !quo_is_null(quo_sort_by)
  y_expr <- y_setup(y = !!quo_y,
                    sort = do_we_sort,
                    # if quo_sort is NULL,
                    # sort_by is never evaluated
                    # so we dont have to worry about errors here
                    sort_by = !!quo_sort_by)

  p <- data |>
    ggplot() +
    # the lines connecting dots
    # (important to add this first so dots appear on top)
    geom_segment(aes(x = !!quo_from,
                     xend = !!quo_to - dot_size/3,
                     y = eval(y_expr),
                     yend = eval(y_expr)),
                 linewidth = 1,
                 color = g_colors$steel,
                 arrow = arrow(length = unit(.075, "in"),
                               ends = "last",
                               type = "closed")
    ) +
    # current value dots
    geom_point(aes(x = !!quo_from,
                   y = eval(y_expr)),
               fill = g_colors$blue,
               color = g_colors$blue,
               size = dot_size,
               shape = 21,
               stroke = 2) +
    geom_text(aes(x = !!quo_from - label_spread,
                  y = eval(y_expr),
                  label = paste0(!!quo_from, "%")),
              color = "black",
              fontface = "bold",
              size = 12/.pt) +
    # to value dots
    geom_point(aes(x = !!quo_to,
                   y = eval(y_expr)),
               fill = g_colors$tangerine,
               color = g_colors$tangerine,
               size = dot_size,
               shape = 21,
               stroke = 2) +
    geom_text(aes(x = !!quo_to + label_spread,
                  y = eval(y_expr),
                  label = paste0(!!quo_to, "%")),
              colour = "black",
              fontface = "bold",
              size = 12/.pt) +
    scale_x_continuous(limits = x_lims, breaks = x_breaks) +
    theme_priorities_poll() +
    labs(x = NULL, y = NULL)


  if (change_col) {

    # default position is "right-most x_break" + `change_col_x`
    col <- geom_text(mapping = aes(x = x_breaks[length(x_breaks)] + change_col_x,
                                # if change_col = TRUE,
                                # we want the labels to aligned with the
                                # appropriate y values, so we copy the
                                # same y_expr from above
                                y = eval(y_expr),
                                label = change),
                     data = data,
                     color = "black",
                     fontface = "bold.italic",
                     size = 12/.pt)
  } else {
    col <- NULL
  }

  p + col
}
