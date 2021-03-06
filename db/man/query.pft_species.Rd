\name{query.pft_species}
\alias{query.pft_species}
\title{Query species given pft name}
\usage{
  query.pft_species(pft = NULL, con = NULL,
    settings = NULL)
}
\arguments{
  \item{pft}{string pft name}

  \item{con}{database connection}

  \item{settings}{list of settings that may provide pft
  name and database settings, if pft and / or con are null}
}
\value{
  string of species.id for species associated with pft
}
\description{
  select plant id's associated with pft
}
\examples{
\dontrun{
query.pft_species('ebifarm.pavi')
query.pft_species(settings = read.settings("pecan.xml"))
}
}
\author{
  David LeBauer
}

