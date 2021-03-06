% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processDirectory.R
\name{processDirectory}
\alias{processDirectory}
\title{Process a classification directory}
\usage{
processDirectory(inputDir, outputDir, whatToDo, overwrite = FALSE)
}
\arguments{
\item{inputDir}{A string with the input directory path.}

\item{outputDir}{A string with the output directory path.}

\item{whatToDo}{A function that takes one or two arguments. The first argument
is the file name. The second one (optional) is a number with the year related
to the file name. The year is extracted from the file name. It is represented
as the last four characters before the file extension.}

\item{overwrite}{Should the output be overwritten?}
}
\description{
Process a classification directory applying a function to
every tif file it belongs. For each file, a new file in the output directory
will be created with the result of a user-defined processing function.
}
