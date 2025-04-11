

#' A stacked bar chart
#'
#' Later, allow users to turn totals on and off
#'
#' @export
#'
#' @importFrom dplyr mutate arrange desc
#' @importFrom ggplot2 ggplot
#' @importFrom tibble add_column
#' @importFrom stats reorder
#' @importFrom rlang arg_match
#'

stacked_bars <- function(data, x_first, x_second, y,
                         sort_by = "first",
                         first_fill = "#002856",
                         second_fill = "#009AD7",
                         bar_width = .9,
                         y_wrap_width = 20,
                         first_label_pos = 20,
                         total_nudge = 2.5,
                         x_end = NULL, x_end_nudge = 1, x_mids = NULL) {


  # capture arguments
  quo_first <- enquo(x_first)
  quo_second <- enquo(x_second)
  quo_y <- enquo(y)
  sort_by <- arg_match(sort_by,
                    values = c("first", "total"),
                    multiple = FALSE)

  # write total column
  data <- data %>%
    mutate(total = sum(!!quo_first, !!quo_second))

  # clarify chart end-points
  if (is.null(x_end)) {
    # round up to nearest multiple of 10
    x_max <- data %>% pull(total) %>% max()
    diff <- 10 - (x_max %% 10)
    x_end <- x_max + diff
  }

  if (is.null(x_end_nudge)) x_end_nudge <- 1
  if (is.null(x_mids)) x_mids <- x_end/2

  # clarify how we'll sort the chart
  if (sort_by == "total") {
    # sort by total before first
    criteria <- exprs(desc(total), desc(!!quo_first), !!quo_y)
  } else { # i.e. if sort == "first"
    # sort by "first" before total
    criteria <- exprs(desc(!!quo_first), desc(total), !!quo_y)
  }

  data %>%
    # a somewhat janky way of handling sorting
    arrange(!!!criteria) %>%
    add_column(ordering = c(1:nrow(data))) %>%
    ggplot() +
    geom_col(aes(x = total,
                 y = reorder(!!quo_y, desc(ordering))),
             fill = second_fill,
             width = bar_width) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = y_wrap_width)) +
    geom_text(aes(y = !!quo_y,
                  x = !!quo_first + (total - !!quo_first)/2,
                  label = paste0(!!quo_second, "%")),
              color = "#FFFFFF") +
    geom_col(aes(x = !!quo_first,
                 y = reorder(!!quo_y, !!quo_first)),
             fill = first_fill,
             width = bar_width) +
    geom_text(aes(y = !!quo_y,
                  x = !!quo_first - max(!!quo_first)/first_label_pos,
                  label = paste0(!!quo_first, "%")),
              color = "#FFFFFF") +
    geom_text(aes(y = !!quo_y,
                  x = !!quo_first + !!quo_second + total_nudge,
                  label = paste0(total, "%")),
              fontface = "bold") +
    labs(x = NULL,
         y = NULL) +
    theme_priorities_poll() +
    adjust_x_continuous(end = x_end, end_nudge = x_end_nudge,
                        mids = x_mids)
}
