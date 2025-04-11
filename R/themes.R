
#' Generic theme for Priorities Poll charts
#'
#' @export
#'
#' @importFrom ggplot2 theme element_text element_blank element_line
#'
theme_priorities_poll <- function(text_size = 12) {

  theme(text = element_text(size = text_size),
        axis.line = element_line(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank())
}
