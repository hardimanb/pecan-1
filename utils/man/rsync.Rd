\name{rsync}
\alias{rsync}
\title{rsync}
\usage{
  rsync(args, from, to, pattern = "")
}
\arguments{
  \item{args}{rsync arguments (see man rsync)}

  \item{from}{source}

  \item{to}{destination}

  \item{pattern}{file pattern to be matched}
}
\value{
  nothing, transfers files as a side effect
}
\description{
  R implementation of rsync
}
\details{
  rsync is a file copying tool in bash
}
\author{
  David LeBauer

  Shawn Serbin
}

