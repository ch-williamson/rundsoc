#' Get individual data for a single wave
#'
#' Gets individual response data for a particular wave and set of variables.
#'
#' @param dir The path to the directory containing the subfolders for each
#'   wave. This is named "stata13_se" when downloading from UKDS.
#' @param wave A whole number indicating the desired wave of Understanding
#'   Society data.
#' @param vars A character vector containing the desired variables.
#'   Variables should be specified without the "w_" prefix and "pidp" is
#'   included by default.
#'   If an empty vector is provided, all variables will be returned.
#' @param long The format of the output.
#'   If TRUE, all variable "w_" prefixes are removed and a column is added
#'   indicating the numerical wave of the data.
#'
#' @return A tibble containing individual response data for the desired wave
#'   and variables.
#'
#' @examples
#' \dontrun{
#' get_ind(dir="path/to/directory", wave=10, vars="envhabit1")
#' get_ind(dir="path/to/directory", wave=9)
#' }
#'
#' @importFrom magrittr %>%
#' @export
get_ind = function(dir, wave, vars = c(), long = TRUE) {

  # make the wave prefix
  prefix = paste0(letters[wave], "_")
  # make the wave filename
  wave_filename = paste0(dir, "ukhls_w", wave, "/", prefix, "indresp.dta")
  # read in that wave's data
  wave_data = haven::read_dta(wave_filename)

  if (length(vars) != 0) {
    # add suitable prefix to variable names
    var_names = paste(prefix, vars, sep="")
  } else {
    # take variables from df's colnames
    all_vars = colnames(wave_data)
    var_names = all_vars[all_vars != "pidp"]
  }

  wave_data = wave_data %>%
    dplyr::select(dplyr::all_of(var_names), "pidp")

  if (long) {
    wave_data = wave_data %>%
      dplyr::rename_with(~substring(., 3), var_names) %>%
      dplyr::mutate(wave = wave)
  }

  return(wave_data)

}


#' Get individual data for multiple waves
#'
#' Gets individual response data for multiple waves.
#'
#' @param dir The path to the directory containing the subfolders for each
#'   wave. This is named "stata13_se" when downloading from UKDS.
#' @param waves A vector of whole numbers indicating the desired waves of
#'   Understanding Society data.
#' @param vars A character vector containing the desired variables.
#'   Variables should appear in all specified waves.
#'   Variables should be specified without the "w_" prefix and "pidp" is
#'   included by default.
#'   If an empty vector is provided, all variables will be returned.
#' @param long The format of the output.
#'   If TRUE, data is returned in "long" format with all variable "w_"
#'   prefixes removed and with a column to indicate the numerical wave of the
#'   data.
#'   Otherwise, data is returned in "wide" format with a column for each wave
#'  and variable.
#'
#' @return A tibble containing individual response data for the desired
#'   waves and variables.
#'
#' @examples
#' \dontrun{
#' get_ind_multiwave(dir="path/to/directory", waves=c(4, 10), vars="envhabit1")
#' get_ind_multiwave(dir="path/to/directory", waves=c(4, 10), vars=c("envhabit1", "envhabit2"), long=FALSE)
#' }
#'
#' @importFrom magrittr %>%
#' @export
get_ind_multiwave = function(dir, waves, vars, long=TRUE) {

  df_list = lapply(waves, get_ind, vars=vars, long=long)

  if (long) {
    full_df = dplyr::bind_rows(df_list)
  } else {
    full_df = df_list %>% purrr::reduce(dplyr::full_join, by="pidp")
  }

  return(full_df)

}


#' Get household data for a single wave
#'
#' Gets household response data for a particular wave and set of variables.
#'
#' @param dir The path to the directory containing the subfolders for each
#'   wave. This is named "stata13_se" when downloading from UKDS.
#' @param wave A whole number indicating the desired wave of Understanding
#'   Society data.
#' @param vars A character vector containing the desired variables.
#'   Variables should be specified without the "w_" prefix.
#'   If an empty vector is provided, all variables will be returned.
#' @param long The format of the output.
#'   If TRUE, all variable "w_" prefixes are removed and a column is added
#'   indicating the numerical wave of the data.
#'
#' @return A tibble containing household response data for the desired wave
#'   and variables.
#'
#' @examples
#' \dontrun{
#' get_hh(dir="path/to/directory", wave=10, vars=c("agechy_dv", "hidp"))
#' get_hh(dir="path/to/directory", wave=9)
#' }
#'
#' @importFrom magrittr %>%
#' @export
get_hh = function(dir, wave, vars = c(), long = TRUE) {

  # make the wave prefix
  prefix = paste0(letters[wave], "_")
  # make the wave filename
  wave_filename = paste0(dir, "ukhls_w", wave, "/", prefix, "hhresp.dta")
  # read in the data
  wave_data = haven::read_dta(wave_filename)

  if (length(vars) != 0) {
    # paste it on all the var names
    var_names = paste(prefix, vars, sep="")
  } else {
    var_names = colnames(wave_data)
  }

  wave_data = wave_data %>% dplyr::select(dplyr::all_of(var_names))

  if (long) {
    wave_data = wave_data %>%
      dplyr::rename_with(~substring(., 3), var_names) %>%
      dplyr::mutate(wave = wave)
  }

  return(wave_data)

}
