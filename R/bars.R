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
    theme_main(text_size = text_size) +
    adjust_x_continuous(end = x_end, end_nudge = x_end_nudge, mids = x_mids)
}

#' Create a One-Variable Bar Chart
#'
#' Generate a bar chart for a single variable from a dataset, with options for
#' sorting, axis inclusion, and customization.
#'
#' @param data A data frame
#' @param var The variable from the dataset to be plotted as bars.
#' @param sort Logical, whether the bars should be sorted. Default is `TRUE`.
#' @param complete Logical, whether to include all factor levels, even those not present in the data. Default is `FALSE`.
#' @param exclude_y_axis Logical, whether to exclude the y-axis text. Default is `FALSE`.
#' @param question_subtitle Logical, whether to include the variable's label as a subtitle. Default is `TRUE`.
#' @param label_pos Numeric, position for the labels on the bars. Default is `10`.
#' @param y_wrap_width Numeric, width for wrapping y-axis labels. Default is `30`.
#' @param x_end Numeric, the endpoint for the x-axis. Default is `100`.
#' @param subtitle_wrap Numeric, width for wrapping the subtitle text. Default is `80`.
#'
#' @return A ggplot object
#'
#' @details `var` must be submitted as a name, not a character string.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' p <- one_bars(data = my_data, var = my_variable)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang enquo
#' @importFrom stringr str_wrap
one_bars <- function(
  data,
  var,
  sort = TRUE,
  complete = FALSE,
  exclude_y_axis = FALSE,
  question_subtitle = TRUE,
  label_pos = 10,
  y_wrap_width = 30,
  x_end = 100,
  subtitle_wrap = 80
) {
  sym_var <- enquo(var)

  label <- data %>% pull(!!sym_var) %>% attr("label")

  p <- noodles::totals(
    data,
    !!sym_var,
    digits = 0,
    na.rm = TRUE,
    complete = complete
  ) %>%
    ggplot() +
    normal_bars(
      x = value,
      y = !!sym_var,
      sort = sort,
      bar_width = .66,
      label_pos = label_pos,
      y_wrap_width = y_wrap_width
    ) +
    labs(x = NULL, y = NULL) +
    adjust_x_continuous(end = x_end, end_nudge = x_end / 10 + 2) +
    theme_main() +
    # this has to go here and not in the question_subtitle chunk, i dont understand why
    theme(
      plot.title.position = "panel",
      plot.title = element_text(family = "Arial Black", hjust = 0)
    )

  y_axis_choice <- if (exclude_y_axis) easy_remove_y_axis(what = "text") else
    geom_blank()

  subtitle_choice <- if (question_subtitle)
    labs(subtitle = str_wrap(label, width = subtitle_wrap)) else geom_blank()

  p + y_axis_choice + subtitle_choice
}


#' Create Bar Chart for Battery Variables
#'
#' Generates a bar chart for variables associated with a specified battery,
#' with options for axis inclusion, customization, and label positioning.
#'
#' @param data A data frame
#' @param batt A character string to match variable names against.
#' @param value_to_find A character string representing the value to filter the totals by. Default is `"Selected"`.
#' @param exclude_y_axis Logical,  whether to exclude the y-axis text. Default is `FALSE`.
#' @param question_subtitle Logical, whether to include the variable's label as a subtitle. Default is `TRUE`.
#' @param exclude_x_axis Logical, whether to exclude the x-axis. Default is `FALSE`.
#' @param label_pos Numeric, position for the labels on the bars. Default is `10`.
#' @param y_wrap_width Numeric, width for wrapping y-axis labels. Default is `30`.
#' @param x_end Numeric, the endpoint for the x-axis. Default is `100`.
#' @param label_regex A regular expression pattern used to extract labels from variable attributes. Default is `"(?<=:).*"` which extracts text following a colon.
#' @param subtitle_wrap Numeric, width for wrapping the subtitle text. Default is `80`.
#'
#' @return A ggplot object.
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot <- batt_bars(data = my_data, batt = "Q23_")
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_extract str_wrap
batt_bars <- function(
  data,
  batt,
  value_to_find = "Selected",
  exclude_y_axis = FALSE,
  question_subtitle = TRUE,
  exclude_x_axis = FALSE,
  label_pos = 10,
  y_wrap_width = 30,
  x_end = 100,
  label_regex = "(?<=:).*$",
  subtitle_wrap = 80
) {
  # # have to annoying go around the embedded match selection
  # n_s <- names(data) %>% str_subset(batt) %>% str_subset("o", negate = TRUE) %>% # this gets you varnames
  #   map_dbl(~n_size(data, .x))
  #
  # n_display <- glue("{min(n_s)} - {max(n_s)}")

  label <- data %>%
    select(matches(batt)) %>%
    # presuming theyre all labeled analogously,
    # pick the first one, as a heuristic
    pull(1) %>%
    attr("label") %>%
    str_extract("^.*(?=:)")

  p <- noodles::batt_totals(data, batt, label_regex, value_to_find) %>%
    ggplot() +
    normal_bars(
      x = value,
      y = item,
      sort = TRUE,
      bar_width = .66,
      label_pos = label_pos,
      y_wrap_width = y_wrap_width
    ) +
    labs(x = NULL, y = NULL) +
    adjust_x_continuous(end = x_end, end_nudge = x_end / 10 + 2) +
    theme_main() +
    # this has to go here and not in the question_subtitle chunk, i dont understand why
    theme(
      plot.title.position = "panel",
      plot.title = element_text(family = "Arial Black", hjust = 0)
    )

  y_axis_choice <- if (exclude_y_axis) easy_remove_y_axis(what = "text") else
    geom_blank()
  x_axis_choice <- if (exclude_x_axis) easy_remove_axes("x") else geom_blank()
  subtitle_choice <- if (question_subtitle)
    labs(subtitle = str_wrap(label, width = subtitle_wrap)) else geom_blank()

  p +
    y_axis_choice +
    x_axis_choice +
    subtitle_choice
}
