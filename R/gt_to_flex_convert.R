#' Convert a gtsummary table to a flextable with custom designs
#'
#' @param gt_tbl A gtsummary object
#' @param font_name A character string to specify the font type for the
#' flextable, defaults to "Albany AMT"
#' @param font_size_header A numeric value to specify the font size for the
#' table header for the flextable, defaults to 12
#' @param font_size_body A numeric value to specify the font size for the
#' table body for the flextable, defaults to 10
#' @param font_size_footer A numeric value to specify the font size for the
#' table footer for the flextable, defaults to 10
#'
#' @importFrom gtsummary as_flex_table
#' @importFrom flextable align font fontsize fp_border_default hline_top
#' @importFrom magrittr %>%
#' @return A flextable object
#' @export
gt_to_flex_convert <- function(gt_tbl,
                               font_name = "Albany AMT",
                               font_size_header = 12,
                               font_size_body = 10,
                               font_size_footer = 10){

  # Create flextable
  tbl_flx <- gt_tbl %>% gtsummary::as_flex_table()
  tbl_flx <- tbl_flx %>%
    flextable::hline_top(border = flextable::fp_border_default(width = 0),
                         part = "header")  %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body",
                     j = c(2:ncol(tbl_flx$header$dataset))) %>%
    flextable::align(align = "left", part = "body", j = 1) %>%
    flextable::font(fontname = font_name, part = "all") %>%
    flextable::fontsize(size = font_size_header, part = "header") %>%
    flextable::fontsize(size = font_size_body, part = "body") %>%
    flextable::fontsize(size = font_size_footer, part = "footer")
  return(tbl_flx)

}
