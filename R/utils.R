
#' @title Print a value on the screen
#' @param value The value to be printed
#' @param log A boolean value indicating whether value should be printed
printLog <- function(value, log)
    if(log) cat(paste0(value, "\n"))

#' @title Convert degrees to meters
#' @param degrees A value in degrees
#' @export
degreesToMeters <- function(degrees) degrees * 111000

#' @title Return a base path according to the configuration of the package
#' @param path A path that within the base directory.
#' @export
mypath <- function(path) paste0(sits_validate.env$base_dir, path)

#' @title Simplify the comparison output
#' @name simplifyOutput
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns a simplified version of
#' a comparison output. It removes all rows and columns with only
#' zeros as values. It also adds a new column and a new row with
#' totals for the respective column and row. Note that if you use
#' this function twice with the same data, it will consider create
#' a new column and row with the totals.
#' @export
simplifyOutput <- function(output){
  # adding the totals as a new line and a new column
  sum_lines <- apply(output, 1, sum)
  sum_columns <- apply(output, 2, sum)

  total <- sum(output)
  sum_columns <- c(sum_columns, total)
  output <- cbind(output, Total = sum_lines)
  output <- rbind(output, Total = sum_columns)

  # removing the lines whose values are all equal to zero
  output <- output[-which(apply(output, 1, function(x) all(x == 0))),]

  # removing the columns whose values are all equal to zero
  output <- output[,-which(apply(output, 2, function(x) all(x == 0)))]

  # final output comparing the two data
  output
}
