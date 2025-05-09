% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bars.R
\name{batt_bars}
\alias{batt_bars}
\title{Create Bar Chart for Battery Variables}
\usage{
batt_bars(
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
)
}
\arguments{
\item{data}{A data frame}

\item{batt}{A character string to match variable names against.}

\item{value_to_find}{A character string representing the value to filter the totals by. Default is \code{"Selected"}.}

\item{exclude_y_axis}{Logical,  whether to exclude the y-axis text. Default is \code{FALSE}.}

\item{question_subtitle}{Logical, whether to include the variable's label as a subtitle. Default is \code{TRUE}.}

\item{exclude_x_axis}{Logical, whether to exclude the x-axis. Default is \code{FALSE}.}

\item{label_pos}{Numeric, position for the labels on the bars. Default is \code{10}.}

\item{y_wrap_width}{Numeric, width for wrapping y-axis labels. Default is \code{30}.}

\item{x_end}{Numeric, the endpoint for the x-axis. Default is \code{100}.}

\item{label_regex}{A regular expression pattern used to extract labels from variable attributes. Default is \code{"(?<=:).*"} which extracts text following a colon.}

\item{subtitle_wrap}{Numeric, width for wrapping the subtitle text. Default is \code{80}.}
}
\value{
A ggplot object.
}
\description{
Generates a bar chart for variables associated with a specified battery,
with options for axis inclusion, customization, and label positioning.
}
\examples{
\dontrun{
# Example usage:
plot <- batt_bars(data = my_data, batt = "Q23_")
}

}
