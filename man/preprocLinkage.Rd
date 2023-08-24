\name{preprocLinkage}
\alias{preprocLinkage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Record Linkage with Data Preprocessing
}
\description{
This function records linkage  along with  data preprocessing.  It has been meticulously executed
 to cover a wide range of datasets, ensuring that variable names are standardized using synonyms. 
This approach facilitates seamless data integration and analysis across various datasets.
}
\usage{
preprocLinkage(d1,d2,chz="NULL",var=c("age","sex"),threshold=0.9,res1="res1",res2="res2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d1}{
A data frame.
}
  \item{d2}{
A data frame.
}
  \item{chz}{
the number of the name of the variable that the user does not want to change based on the output of the \code{\link{preproc}} function.
}
  \item{var}{
The vector of the names of the blocked variables that the user chooses based on the output of the \code{selVar} function that gives the vector of the names of the common variables between the two data sets.
}
  \item{threshold}{
A numeric value between 0 and 1. 
}
  \item{res1}{
the output results file name for recode linkage with shared variables.
}
  \item{res2}{
the output results file name for with all variables.
}
}
\details{
The results are stored in the .csv files, but if the number of records exceeds one million, they are stored in the rdata files.
}
\value{
Two csv files or two rdata files.
}
\author{
Hossein Hassani and and Leila Marvian Mashhad.
}

\note{
Note that, to see the results in the created file, first call the data.table package.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{selVar}}, \code{\link{chzInput}}
}
\examples{
  d1 = RLdata500
  d2 = RLdata10000
  preprocLinkage(d1, d2, var = "by")
 
}
