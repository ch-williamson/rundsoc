#' Replace special values with NA
#'
#' Replaces Understanding Society's special "missing" values with NA.
#'
#' @param df A dataframe.
#'
#' @return The same dataframe with Understanding Society's missing values
#'   replaced with NA.
#'
#' @examples
#' test_data = data.frame(a = c(532, -2), b = c(-9, 40))
#' clean_data = make_na(test_data)
#' clean_data
#'
#' @importFrom magrittr %>%
#' @export
make_na = function(df) {

  na_values = c(-9, -8, -7, -2, -1)

  for (value in na_values) {
    df = df %>%
      dplyr::mutate_all(~dplyr::na_if(., value))
  }

  return(df)

}
