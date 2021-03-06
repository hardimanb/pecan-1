\name{model2netcdf.BIOCRO}
\alias{model2netcdf.BIOCRO}
\title{Function to convert biocro model output to standard netCDF format}
\usage{
  model2netcdf.BIOCRO(resultDT, outdir, lat = NULL,
    lon = NULL)
}
\arguments{
  \item{resultDT}{output from BioCro model}

  \item{outdir}{Location of model output}

  \item{lat}{Latitude of the site}

  \item{lon}{Longitude of the site}
}
\description{
  Convert BioCro output to netCDF
}
\details{
  Converts BioCro output to netCDF. Modified from on
  model2netcdf.sipnet and model2netcdf.ED2 by Shawn Serbin
  and Mike Dietze
}
\author{
  David LeBauer, Deepak Jaiswal
}

