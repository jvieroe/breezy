#' Calculate
#'
#' This function
#'
#' @param data xx
#' @param keep_character xx. Defaults to `FALSE`
#' @return lorem ipsum
#' @author Jeppe Vierø
#' @import dplyr tibble labelled rlang janitor
#' @export

cross_breeze <- function(data = NULL,
                         cross_var = NULL,
                         keep_character = FALSE) {

  validate_cross_breeze(data = data,
                        keep_character = keep_character)

  vars <- data %>%
    base::names(.) %>%
    tibble::tibble() %>%
    dplyr::rename(variable = '.')

  temp <- data %>%
    labelled::var_label(.) %>%
    base::unlist() %>%
    tibble::tibble() %>%
    dplyr::rename(label = '.')

  # introduces error if 1+ variable has no label. Fix!

  labs <- base::cbind(vars, temp)

  all_vars <- data %>%
    base::names(.)

  tab_list_temp <- base::list()

  for(i in base::seq_along(all_vars)) {

    temp <- data %>%
      dplyr::rename(var := !!rlang::sym(all_vars[i]))

    tab_temp <- janitor::tabyl(temp$var) %>%
      dplyr::mutate(variable = all_vars[i]) %>%
      dplyr::rename(var = 'temp$var') %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(n_count = sum(n,
                                  na.rm = T)) %>%
      dplyr::mutate(pct = n/n_count) %>%
      dplyr::mutate(pct_validation = sum(pct,
                                         na.rm = T)) %>%
      dplyr::ungroup()

    tab_list_temp[[i]] <- tab_temp

  }

  tab_data_exp <- dplyr::bind_rows(tab_list_temp) %>%
    dplyr::left_join(labs,
                     by = c("variable"))

}
