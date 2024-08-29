#' shinycoins
#'
#' An interactive app which produces a plot containing the post/prior/likelihood and adjustable widgets for theta, the prior, and the alpha level
#'
#' @return an interactive plot with labels
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinycoins()}
shinycoins <- function() {
  runApp(system.file("shinycoins",
                     package = "RosaDSA5403lab2"),
         launch.browser = TRUE)
}
