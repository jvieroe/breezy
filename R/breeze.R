#' Calculate dyadic distances between points `i` and `j` in a spatial dataset
#'
#' This function calculates the geodesic distance between any dyads (pairs of points `i` and `j`) in a spatial dataset and stores the result in a long tibble
#'
#' @param data xx
#' @return lorem ipsum
#' @author Jeppe Vierø
#' @import dplyr tibble labelled rlang janitor
#' @export

breeze <- function(data = NULL) {

  validate_breeze(data = data)

  vars <- df %>%
    base::names(.) %>%
    tibble::tibble() %>%
    dplyr::rename(variable = '.')

  temp <- df %>%
    labelled::var_label(.) %>%
    base::unlist() %>%
    tibble::tibble() %>%
    dplyr::rename(label = '.')

  # introduces error if 1+ variable has no label. Fix!

  labs <- base::cbind(vars, temp)

  all_vars <- df %>%
    base::names(.)

  tab_list_temp <- base::list()

  for(i in base::seq_along(all_vars)) {

    temp <- df %>%
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
