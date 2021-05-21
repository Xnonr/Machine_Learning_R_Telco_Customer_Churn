\name{loadTrueResults}
\alias{loadTrueResults}
\title{loadTrueResults}
\description{
  Reads 2 csv files called True_TCC_Optimal_Price_Matrix &
  True_TCC_Model_Results into 2 correspondig data frames. These data frames
  will then be used later on for automated testing purposes, acting as base
  results to ensure that the rest of the code has not been compromised
  if changed. Used for CI purposes
}
\usage{loadTrueResults()}
\details{
  Does not actually return any variables. Instead, the 2 matrices are assigned
  to the global environment instead.
}
\value{
  \item{trueOptPriceMatrix}{
   A small 6x3 data frame matrix with 18 values of the true optimal prices the
   priceOptimizer & calcOptimalPrices functions should output no matter the
   code changes if the base Telco_Customer_Churn file is used.
   }
  \item{trueEResultsComplete}{
   A small 6x6 data frame matrix with 36 values with the true modeling results
   the createEnsembleMethodsModel should output no matter the code changes if
   the base Telco_Customer_Churn file is used. It incorporates all the
   results from the Decision Tree, Logistic Regression, K-Nearest Neighbors,
   Naive Bayes Classifier and Ensemble Models, as well as the base actual churn
   results of the original dataset itself for easy comparison.
   }
}
\author{Maxence Brette}
\examples{
##loadTrueResults()
}
