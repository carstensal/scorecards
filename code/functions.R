#' Bin numeric variable
#'
#' @param x Character string indicating which variable is being binned 
#' @param y Binary response variable 
#' @param dftrain Data frame containing the training data
#' @param dftest Data frame containing the test data
#' @param cuts Vector of pre-determined cut points for binning
#' @param type Whether to bin using conditional inference trees or use quantiles
#'
#' @return A list of two components, the first component is the binned variable
#'     in the training data and the second component is the binning applied on
#'     the test data
#'     
#'     
binning_numeric <- function(x, y, dftrain, dftest, cuts = NULL,
                            type = "CIT") {
  if (is.null(cuts) && type == "CIT") {
    bin_train <- smbinning(dftrain, y, x)
  } else if (is.null(cuts) && type != "CIT") {
    cuts <- unique(
      c(min(dftrain[[x]], na.rm = TRUE),
        quantiles = quantile(
          dftrain[[x]], probs = seq(0.1, 1, by = 0.1),
          na.rm = TRUE
        )
      )
    )
    bin_train <- smbinning.custom(dftrain, y, x, cuts)
  } else {
    bin_train <- smbinning.custom(dftrain, y, x, cuts)
  }
  if (class(bin_train) != "character") {
    bin_test <- smbinning.custom(dftest, y, x, bin_train$cuts)
  } else {
    bin_test <- bin_train
  }
  list(bin_train = bin_train, bin_test = bin_test)
}
