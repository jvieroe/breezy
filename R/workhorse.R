#' @noRd
validate_breeze <- function(data,
                            keep_character) {

  if(base::is.null(data)){
    stop("No data provided")
  }

  if(!keep_character %in% c(TRUE, FALSE)){
    stop("Invalid 'keep_character' provided. Must be logical")
  }


  if(base::any(base::sapply(data, base::is.numeric) == TRUE)){
    warning("Data contains numeric variables. These will be discarded")

    data <- data %>%
      .[ , !base::sapply(., base::is.numeric)]


  }

  if(base::any(base::sapply(data, base::is.character) == TRUE)){

    if(keep_character == TRUE) {

      warning("Data contains character variables. These will be converted to factors")

      data[base::sapply(data, base::is.character)] <- base::lapply(data[base::sapply(data, base::is.character)],
                                                                   base::as.factor)


    } else if (keep_character == FALSE) {

      warning("Data contains character variables. These will be discarded")

      data <- data %>%
        .[ , !base::sapply(., base::is.character)]


    }


  }


}


#' @noRd
validate_cross_breeze <- function(data,
                                  cross,
                                  keep_character) {

  if(base::is.null(data)){
    stop("No data provided")
  }

  if(!keep_character %in% c(TRUE, FALSE)){
    stop("Invalid 'keep_character' provided. Must be logical")
  }

  if(base::any(base::sapply(data, base::is.numeric) == TRUE)){
    warning("Data contains numeric variables. These will be discarded")

    data <- data %>%
      .[ , !base::sapply(., base::is.numeric)]


  }

  if(base::any(base::sapply(data, base::is.character) == TRUE)){

    if(keep_character == TRUE) {

      warning("Data contains character variables. These will be converted to factors")

      data[base::sapply(data, base::is.character)] <- base::lapply(data[base::sapply(data, base::is.character)],
                                                                   base::as.factor)


    } else if (keep_character == FALSE) {

      warning("Data contains character variables. These will be discarded")

      data <- data %>%
        .[ , !base::sapply(., base::is.character)]


    }


  }


}
