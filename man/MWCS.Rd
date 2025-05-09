\name{MWCS}
\alias{MWCS}
\docType{data}
\title{The Mechanical Waves Conceptual Survey (MWCS) dataset}
\description{
The MWCS is the most important test to date that has been designed to evaluate students’ understanding of four main topics in mechanical waves. It encompasses some concepts surrounding propagation, superposition, reflection, and standing waves within 22 multiple choice items.
}
\usage{data(MWCS)}
\format{
  A data frame of students' responses on the following 22 MWCS multiple choice items.
  \describe{
    \item{\code{MWCS1}}{A student's response on the MWCS item number 1.}
    \item{\code{MWCS2}}{A student's response on the MWCS item number 2.}
    \item{\code{MWCS3}}{A student's response on the MWCS item number 3.}
    \item{\code{MWCS4}}{A student's response on the MWCS item number 4.}
    \item{\code{MWCS5}}{A student's response on the MWCS item number 5.}
    \item{\code{MWCS6}}{A student's response on the MWCS item number 6.}
    \item{\code{MWCS7}}{A student's response on the MWCS item number 7.}
    \item{\code{MWCS8}}{A student's response on the MWCS item number 8.}
    \item{\code{MWCS9}}{A student's response on the MWCS item number 9.}
    \item{\code{MWCS10}}{A student's response on the MWCS item number 10.}
    \item{\code{MWCS11}}{A student's response on the MWCS item number 11.}
    \item{\code{MWCS12}}{A student's response on the MWCS item number 12.}
    \item{\code{MWCS13}}{A student's response on the MWCS item number 13.}
    \item{\code{MWCS14}}{A student's response on the MWCS item number 14.}
    \item{\code{MWCS15}}{A student's response on the MWCS item number 15.}
    \item{\code{MWCS16}}{A student's response on the MWCS item number 16.}
    \item{\code{MWCS17}}{A student's response on the MWCS item number 17.}
    \item{\code{MWCS18}}{A student's response on the MWCS item number 18.}
    \item{\code{MWCS19}}{A student's response on the MWCS item number 19.}
    \item{\code{MWCS20}}{A student's response on the MWCS item number 20.}
    \item{\code{MWCS21}}{A student's response on the MWCS item number 21.}
    \item{\code{MWCS22}}{A student's response on the MWCS item number 22.}
  }
}
\source{
Santoso, P. H. et al. SPHERE: Students' performance dataset of conceptual understanding, scientific ability, and learning attitude in physics education research (PER). \emph{Mendeley Data, V1}, (2024). doi: 10.17632/88d7m2fv7p.1.
}
\references{
Barniol, P. & Zavala, G. Mechanical waves conceptual survey: Its modification and conversion to a standard multiple-choice test. \emph{Phys Rev Phys Educ Res 12}, 010107 (2016).
}
\examples{
library(spheredata)
# Call the data from spheredata package
get("MWCS")
# Open the data
MWCS
}
