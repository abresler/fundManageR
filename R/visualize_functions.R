#' Data frame visualization
#'
#' This function allows a data frame to
#' be viewed and edited via the \code{listviewer} package.
#'
#' @param data A data frame
#' @param edit \code{TRUE} edit the data frame
#'
#' @return \code{htmlwidget}
#' @export
#' @import listviewer
#' @examples
#' visualize_data_frame(mtcars)
visualize_data_frame <-
  function(data, edit = F) {
    if (edit) {
      json_list <-
        listviewer::jsonedit_gadget(data)
    } else {
      json_list <-
        listviewer::jsonedit(data)
    }
    return(json_list)
  }