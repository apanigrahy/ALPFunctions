#' Create publication style table using gtsummary and flextable
#'
#' @description
#' This function utilizes the gtsummary tbl_summary function to summarize data
#' with custom settings and inputs and can output a flextable object with custom
#' inputs for rendering Rmarkdown word or html reports
#'
#'
#' @param data_frame A dataframe object
#' @param group_by A categorical variable to group the table by
#' @param label_list A list object to specify for variable labels
#' @param digits_list A list object to specify for digits lengths
#' @param value_list A list object to specify summary statistics and values
#' @param type_list A list object to specify variable types
#' @param add_p If TRUE, adds statistic p-values
#' @param add_p_test If not NULL, specify the test statistic type
#' @param add_p_test_args If not NULL, specify test statistic arguments
#' @param add_p_group If not NULL, specify the variable used for ID grouping for paired data
#' @param add_total_col If TRUE, adds the totals for all columns
#' @param header_text Specify header
#' @param return_object Specifies what object
#'
#' @import gtsummary
#' @import flextable
#' @importFrom dplyr arrange case_when count filter mutate pull
#' @importFrom stringr str_replace_na
#' @importFrom magrittr %>%
#'
#' @return Returns a flextable or gtsummary table object
#' @export
gt_flex_table <- function(data_frame,
                          group_by = NULL,
                          label_list = list(),
                          digits_list = list(),
                          value_list = list(),
                          type_list = list(),
                          include = everything(),
                          add_p = FALSE,
                          add_p_test = NULL,
                          add_p_test_args = NULL,
                          add_p_group = NULL,
                          add_total_col = FALSE,
                          header_text = "**Table 1: Insert Table Title**",
                          return_object = "flextable"){
  # If label_list is empty, set to NULL
  if(length(label_list) == 0){
    label_list <- NULL
  }
  # If digits_list is empty, set to NULL
  if(length(digits_list) == 0){
    digits_list <- NULL
  }
  # If value_list is empty, set to NULL
  if(length(value_list) == 0){
    value_list <- NULL
  }

  # Append digist_list if applicable
  digits <- list(all_continuous2() ~ 1)
  digits_list <- c(digits, digits_list)

  # Append type_list if applicable
  types <- list(all_continuous() ~ "continuous2",
                all_dichotomous() ~ "categorical")
  type_list <- c(types, type_list)


  # Replace all NA's for character and factors with "N Missing %"
  for (a in colnames(data_frame)) {
    # Check if variable is character or factor
    if (is.character(data_frame[[a]]) |
      is.factor(data_frame[[a]])) {
      # Count number rows with missing data
      num_missing <- data_frame %>%
        dplyr::filter(is.na(!!rlang::sym(a)))
      # If any rows are missing data
      if (nrow(num_missing) >= 1) {
        # If character variable, pull unique characters as levels,
        # replace NA's, convert variable to factor with those levels and
        # include N Missing
        if (is.character(data_frame[[a]])) {
          levels_before <- data_frame %>%
            dplyr::filter(!is.na(!!rlang::sym(a))) %>%
            dplyr::count(!!rlang::sym(a)) %>%
            dplyr::arrange(!!rlang::sym(a)) %>%
            dplyr::pull(!!rlang::sym(a))
          data_frame <- data_frame %>%
            dplyr::mutate(!!rlang::sym(a) := stringr::str_replace_na(.[[a]], "N Missing (%)")) %>%
            dplyr::mutate(!!rlang::sym(a) := factor(!!rlang::sym(a),
              levels = c(levels_before, "N Missing (%)")
            ))
          rm(levels_before)
        # If already a factor variable, replace NA's and add factor levels back
        # with missing added
        } else if (is.factor(data_frame[[a]])) {
          levels_before <- levels(data_frame[[a]])
          data_frame <- data_frame %>%
            dplyr::mutate(!!rlang::sym(a) := stringr::str_replace_na(.[[a]], "N Missing (%)")) %>%
            dplyr::mutate(!!rlang::sym(a) := factor(!!rlang::sym(a),
              levels = c(levels_before, "N Missing (%)")
            ))
          rm(levels_before)
        }
      } else {
        if (is.character(data_frame[[a]])) {
          data_frame <- data_frame %>%
            dplyr::mutate(!!rlang::sym(a) := as.factor(!!rlang::sym(a)))
        }
      }
    }
  }
  rm(a)

  # Create initial gtsummary table
  gt_tbl <- data_frame %>%
    gtsummary::tbl_summary(
      by = {{group_by}},
      type = type_list,
      label = label_list,
      value = value_list,
      digits = digits_list,
      include = {{include}},
      statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                            "{median} ({p25}-{p75})",
                                            "{min}-{max}"),
                       all_categorical() ~ "{n} ({p}%)"),
      missing_text = "N Missing (%)",
      missing_stat = "{N_miss} ({p_miss}%)"
    )
  # Create function to format p-values
  format_p_value <- function(p) {
    dplyr::case_when(
      is.na(p) ~ " ",
      p < 0.0001 ~ "<0.0001",
      p > 0.9999 ~ ">0.9999",
      TRUE ~ sprintf("%.4f", p) # Display p-values with 4 digits
    )
  }

  # Add statistics with p-values if applicable
  if(add_p == TRUE){
    gt_tbl <- gt_tbl %>%
      gtsummary::add_p(
        test = add_p_test,
        test.args = add_p_test_args,
        group = {{add_p_group}},
        pvalue_fun = format_p_value
      )
  }
  # Add overall column if applicable
  if(add_total_col == TRUE){
    gt_tbl <- gt_tbl %>%
      gtsummary::add_overall()
  }
  # Finalize gt table parameters and add header
  gt_tbl <- gt_tbl %>%
    gtsummary::add_stat_label(label = list(
      all_continuous() ~ c("Mean (SD)", "Median (IQR)","Min-Max"),
      all_categorical() ~"n (%)")) %>%
    gtsummary::modify_header(label ~ "**Variable**",
                             all_stat_cols() ~ "**{level}**\n(n = {n})") %>%
    gtsummary::modify_spanning_header(
      everything() ~ header_text
    ) %>%
    gtsummary::bold_labels()

  # Remove the ", n%" from categorical variable labels
  gt_tbl$table_body$stat_label <- NA

  # Create and return flextable object if applicable
  if(return_object == "flextable"){
    cols <- ncol(gt_tbl)
    tbl_flx <- gt_tbl %>% gtsummary::as_flex_table()
    tbl_flx <- tbl_flx %>%
      flextable::hline_top(border = fp_border_default(width = 0),
                           part = "header")  %>%
      flextable::align(align = "center", part = "header") %>%
      flextable::align(align = "center", part = "body",
                       j = c(2:ncol(tbl_flx$header$dataset))) %>%
      flextable::align(align = "left", part = "body", j = 1) %>%
      flextable::font(fontname = "Albany AMT", part = "all") %>%
      flextable::fontsize(size = 12, part = "header") %>%
      flextable::fontsize(size = 10, part = "body") %>%
      flextable::fontsize(size = 10, part = "footer")
    return(tbl_flx)
  }else{
    return(gt_tbl)
  }
}
