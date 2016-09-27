#' Visualize a data frame
#'
#' @param data A data frame
#' @param edit Do you wish to edit the data
#'
#' @return
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