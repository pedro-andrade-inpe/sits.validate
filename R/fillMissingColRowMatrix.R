

#' @title Fill missing rows and cols of a matrix with zeros
#' @description A row is missing when there is a col with the same name. The
#' same is applied to cols.
#' @param mat A matrix.
#' @export
#' @examples
#' mat <- matrix(1, 2, 4)
#' colnames(mat) <- 3:6
#' rownames(mat) <- 1:2
#' mat
#' fillMissingColRows(mat)
fillMissingColRows <- function(data)
{
  rn <- rownames(data)
  cn <- colnames(data)

  missing_rows <- setdiff(cn, rn)
  missing_cols <- setdiff(rn, cn)

  if(length(missing_rows) > 0){
    myresult <- matrix(0, nrow = length(missing_rows), ncol = length(cn))
    rownames(myresult) <- missing_rows

    data <- rbind(data, myresult)
    rn <- rownames(data)
  }

  if(length(missing_cols) > 0){
    myresult <- matrix(0, nrow = length(rn), ncol = length(missing_cols))
    colnames(myresult) <- missing_cols

    data <- cbind(data, myresult)
  }

  return(data)
}


