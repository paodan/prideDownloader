% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectDetail.R
\name{projectDetail}
\alias{projectDetail}
\title{The details of projects returned by searching}
\usage{
projectDetail(accession = c("PXD000001", "PXD000002"),
  accessionRownames = TRUE)
}
\arguments{
\item{accession}{character, a vector of project accessions. The default is
c("PXD000001", "PXD000002")}
}
\value{
a data.frame of project details, including: projectAccession, title,
numAssays, species, tissues, ptmNames, quantificationMethods, numProteins,numPeptides,
numSpectra, numIdentifiedSpectra, numUniquePeptides, instrumentNames, experimentTypes,
keywords, publicationDate, projectDescription, submissionType, projectTags, submitter,
submissionDate, reanalysis, sampleProcessingProtocol, dataProcessingProtocol,
otherOmicsLink, labHeads, doi, and references.
}
\description{
The details of projects returned by searching
}
\examples{
{
\dontrun{
# Search for one accession
projectDetail("PXD000002")

# Search for two accessions
projectDetail(c("PXD000001", "PXD000002"))
}
}
}
