\name{logger.setUseConsole}
\alias{logger.setUseConsole}
\title{Configure logging to console.}
\usage{
  logger.setUseConsole(console, stderr = TRUE)
}
\arguments{
  \item{console}{set to true to print logging to console.}

  \item{stderr}{set to true (default) to use stderr instead
  of stdout for logging}
}
\description{
  Should the logging to be printed to the console or not.
}
\examples{
\dontrun{
logger.setUseConsole(TRUE)
}
}
\author{
  Rob Kooper
}

