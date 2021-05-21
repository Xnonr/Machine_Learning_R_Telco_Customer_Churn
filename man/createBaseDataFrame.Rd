\name{createBaseDataFrame}
\alias{createBaseDataFrame}
\title{createBaseDataFrame}
\description{Reads a '.csv' file into a data frame and then returns it.}
\usage{createBaseDataFrame(x)}
\arguments{
  \item{baseCSVFile}{
   The name of the '.csv' file being imported and read.
   As the program was designed specifically for the 'Telco_Customer_Churn.csv'
   this is set as the default variable value, although any other file can be
   read through.}
}
\value{
  \item{baseDF}{
   An otherwise unmodified data frame version of a read through
   '.csv' file.}
}
\author{Maxence Brette}
\examples{
createBaseDataFrame()
createBaseDataFrame('Telco_Customer_Churn.csv')
createBaseDataFrame('2021_Store_Sales.csv')
}
