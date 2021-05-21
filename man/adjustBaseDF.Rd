\name{adjustBaseDF}
\alias{adjustBaseDF}
\title{adjustBaseDF}
\description{
  Removes the following columns from the baseDF data frame deemed irrelevant
  for modeling and price optimization purposes:

  customerID, PaperlessBilling, PaymentMethod, TotalCharges

  Also sets the row IDs to that of the values in the customerID column.
}
\usage{adjustBaseDF(baseDF)}
\arguments{
  \item{baseDF}{
   The data frame created and returned by the createBaseDataFrame function.
   It must be one built from the Telco_Customer_Churn or else an error
   will occur if this function is invoked.
}
}
\value{
  \item{adjustedDF}{
   A data frame with the now modified row IDs and certain deemed irrelevant
   columns removed.
   }
}
\author{Maxence Brette}
\examples{
adjustedDF <- adjustBaseDF(baseDF)
}
