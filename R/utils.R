
#' @title Print a value on the screen
#' @description Print the progress of a function.
#' @param value The value to be printed
#' @param progress A boolean value indicating whether value should be printed
printProgress <- function(value, progress)
    if(progress) cat(paste0(value, "\n"))

#' @title Convert degrees to meters
#' @description Converts values from degrees to meters by multiplying them by 111,000.
#' @param degrees A value in degrees
#' @export
degreesToMeters <- function(degrees) degrees * 111000

#' @title Convert from m^2 to Mha
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function divides the input values
#' by 1e10 to convert from m^2 to Millions of hectares.
#' @param value A vector of values.
#' @export
m2ToMha <- function(value) value / 1e10

#' @title Convert from ha to Mha
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function divides the input values
#' by 1e6 to convert from hectares to Millions of hectares.
#' @param value A vector of values.
#' @export
haToMha <- function(value) value / 1e6

#' @title Join tibles summing their areas
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function joins a list of tibbles and sums
#' their areas. It preserves the rows and sums the values whose
#' columns are the same.
#' @param classes A list of tibbles with classifications
#' @export
joinClassifications <- function(classes){
  binded <- plyr::rbind.fill(classes) %>% # fill keeping the same columns
    plyr::ddply("rowname", function(x) colSums(x[-1], na.rm = TRUE))

  output <- tibble::tibble(rowname = sits.validate.env$classes_sits) %>%
    dplyr::full_join(binded, by = "rowname") %>%
    stats::na.omit()

  output
}

#' @title Simplify the comparison output
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function returns a simplified version of
#' a comparison output. It removes all rows and columns with only
#' zeros as values. It also adds a new column and a new row with
#' totals for the respective column and row. Note that if you use
#' this function twice with the same data, it will consider create
#' a new column and row with the totals.
#' @param output The output of a compare function, such as compareTCCerrado() or compareCerradoMask().
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
