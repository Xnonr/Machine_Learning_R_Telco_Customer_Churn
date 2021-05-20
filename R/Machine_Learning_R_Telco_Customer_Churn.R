#By Maxence Brette

rm(list = ls()) #Clears the current work space & R Studio Global Environment
cat("\014") #Clears the R Studio Console

#Installs & imports the desired libraries
#install.packages('dplyr') #Remains commented out following instillation
#install.packages('e1071')
#install.packages('caret')
#install.packages('rpart')
#install.packages('taRifx')
#install.packages('adabag')
#install.packages('stringr')
#install.packages('compare')
#install.packages('cluster')
#install.packages('rpart.plot')

library(dplyr)
library(e1071)
#library(caret)
library(rpart)
library(taRifx)
#library(adabag)
library(stringr)
library(compare)
library(cluster)
library(rpart.plot)

#Adjusts the working directory as needed
setwd("~/Documents/github/RMachineLearningTelcoCustomerChurn")

#Loads the original 'Telco_Customer_Churn.csv' base data file into a data frame
createBaseDataFrame <- function(baseCSVFile = 'Telco_Customer_Churn.csv') {
   baseDF <- read.csv(file = baseCSVFile, stringsAsFactors = FALSE)
   return(baseDF)
}

#Loads the True Optimal Prices & Modeling Results for automated testing
loadTrueResults <-function() {
   trueOptPriceMatrix <- read.csv(file = 'True_TCC_Optimal_Price_Matrix.csv',
                                  stringsAsFactors = FALSE)
   trueOptPriceMatrix <- head(trueOptPriceMatrix, 6)
   trueOptPriceMatrix <- subset(trueOptPriceMatrix[2:4])

   trueEResultsComplete <- read.csv(file = 'True_TCC_Model_Results.csv',
                                    stringsAsFactors = FALSE)
   trueEResultsComplete <- subset(trueEResultsComplete[2:7])

   assign('trueOptPriceMatrix', trueOptPriceMatrix, envir = .GlobalEnv)
   assign('trueEResultsComplete', trueEResultsComplete, envir = .GlobalEnv)
}

#Removes unwanted variables from 'baseDF' to create a new adjusted data frame
adjustBaseDF <- function(baseDF) {
   #Sets the row names to those of the customerID column
   rowID <- baseDF[,1]
   row.names(baseDF) <- rowID

   #Removes customerID, PaperlessBilling, PaymentMethod & TotalCharges columns
   adjustedDF <- baseDF[,!(names(baseDF) %in% c("customerID",
                                                "PaperlessBilling",
                                                "PaymentMethod",
                                                "TotalCharges"))]

   return(adjustedDF)
}

#Binarizes a data frame, functions yet is inefficient in terms of lines of code
binarizeDF <- function(toBinarizeDF, baseDF) {
   #Transforms the string variable values into numerical binary ones
   #Needed for clustering & methods that cannot use traditional factored columns
   toBinarizeDF$gender[toBinarizeDF$gender == "Female"] <- 0
   toBinarizeDF$gender[toBinarizeDF$gender == "Male"] <- 1

   toBinarizeDF$Partner[toBinarizeDF$Partner == "No"] <- 0
   toBinarizeDF$Partner[toBinarizeDF$Partner == "Yes"] <- 1

   toBinarizeDF$Dependents[toBinarizeDF$Dependents == "No"] <- 0
   toBinarizeDF$Dependents[toBinarizeDF$Dependents == "Yes"] <- 1

   toBinarizeDF$PhoneService[toBinarizeDF$PhoneService == "No"] <- 0
   toBinarizeDF$PhoneService[toBinarizeDF$PhoneService == "Yes"] <- 1

   toBinarizeDF$MultipleLines[toBinarizeDF$MultipleLines == "No"] <- 0
   toBinarizeDF$MultipleLines[toBinarizeDF$MultipleLines == "No phone service"] <- 0
   toBinarizeDF$MultipleLines[toBinarizeDF$MultipleLines == "Yes"] <- 1

   toBinarizeDF$ISDSL <- ifelse(toBinarizeDF$InternetService == "DSL", 1, 0)
   toBinarizeDF$ISFiberOptic <- ifelse(toBinarizeDF$InternetService == "Fiber optic", 1, 0)
   #Removes the InternetService column after having created the dummy variables
   toBinarizeDF <- toBinarizeDF[,!(names(toBinarizeDF) %in% c("InternetService"))]

   toBinarizeDF$OnlineSecurity[toBinarizeDF$OnlineSecurity == "No"] <- 0
   toBinarizeDF$OnlineSecurity[toBinarizeDF$OnlineSecurity == "No internet service"] <- 0
   toBinarizeDF$OnlineSecurity[toBinarizeDF$OnlineSecurity == "Yes"] <- 1

   toBinarizeDF$OnlineBackup[toBinarizeDF$OnlineBackup == "No"] <- 0
   toBinarizeDF$OnlineBackup[toBinarizeDF$OnlineBackup == "No internet service"] <- 0
   toBinarizeDF$OnlineBackup[toBinarizeDF$OnlineBackup == "Yes"] <- 1

   toBinarizeDF$DeviceProtection[toBinarizeDF$DeviceProtection == "No"] <- 0
   toBinarizeDF$DeviceProtection[toBinarizeDF$DeviceProtection == "No internet service"] <- 0
   toBinarizeDF$DeviceProtection[toBinarizeDF$DeviceProtection == "Yes"] <- 1

   toBinarizeDF$TechSupport[toBinarizeDF$TechSupport == "No"] <- 0
   toBinarizeDF$TechSupport[toBinarizeDF$TechSupport == "No internet service"] <- 0
   toBinarizeDF$TechSupport[toBinarizeDF$TechSupport == "Yes"] <- 1

   toBinarizeDF$StreamingTV[toBinarizeDF$StreamingTV == "No"] <- 0
   toBinarizeDF$StreamingTV[toBinarizeDF$StreamingTV == "No internet service"] <- 0
   toBinarizeDF$StreamingTV[toBinarizeDF$StreamingTV == "Yes"] <- 1

   toBinarizeDF$StreamingMovies[toBinarizeDF$StreamingMovies == "No"] <- 0
   toBinarizeDF$StreamingMovies[toBinarizeDF$StreamingMovies == "No internet service"] <- 0
   toBinarizeDF$StreamingMovies[toBinarizeDF$StreamingMovies == "Yes"] <- 1

   toBinarizeDF$MtMContract <- ifelse(toBinarizeDF$Contract == "Month-to-month", 1, 0)
   toBinarizeDF$OneYContract <- ifelse(toBinarizeDF$Contract == "One year", 1, 0)
   toBinarizeDF$TwoYContract <- ifelse(toBinarizeDF$Contract == "Two year", 1, 0)
   #Removes the Contract column after having created the dummy variables
   toBinarizeDF <- toBinarizeDF[,!(names(toBinarizeDF) %in% c("Contract"))]

   toBinarizeDF$Churn[toBinarizeDF$Churn == "No"] <- 0
   toBinarizeDF$Churn[toBinarizeDF$Churn == "Yes"] <- 1

   #Transforms all columns from character to numeric type
   toBinarizeDF <- as.data.frame(apply(toBinarizeDF, 2, as.numeric))
   row.names(toBinarizeDF) <- baseDF[,1]
   #sapply(baseDF, class) #Checks if all columns have been properly converted

   return(toBinarizeDF)
}

#Manual Segmentation:
#To be compared with to the K-Means & Hierarchical Clusters
#Customer Segments: 16 Total possible combinations, 8 Male & 8 Female based on
#all the possible combinations of the customer demographics:
# Male (M = 1) & Female (F = 0)
# SeniorCitizen (S True = 1 & S False = 0)
# Partner (P True = 1 & P False = 0)
# Dependents (D True = 1 & D False = 0)
manualCustomerDemographicsSegmenter <- function(binarizedDF) {
   #Creates a comparitor value column for an easier time filtering
   binarizedDF$CusDemComparitor <- paste(binarizedDF$gender,
                                         binarizedDF$SeniorCitizen,
                                         binarizedDF$Partner,
                                         binarizedDF$Dependents, sep = "")

   #Male Segments
   cusDemSegMSPD <- subset(binarizedDF, CusDemComparitor == '1111')
   cusDemSegMSPD <- aggregate(cusDemSegMSPD,
                              list(cusDemSegMSPD$CusDemComparitor), mean)

   cusDemSegMSP <- subset(binarizedDF, CusDemComparitor == '1110')
   cusDemSegMSP <- aggregate(cusDemSegMSP,
                             list(cusDemSegMSP$CusDemComparitor), mean)

   cusDemSegMSD <- subset(binarizedDF, CusDemComparitor == '1101')
   cusDemSegMSD <- aggregate(cusDemSegMSD,
                             list(cusDemSegMSD$CusDemComparitor), mean)

   cusDemSegMS <- subset(binarizedDF, CusDemComparitor == '1100')
   cusDemSegMS <- aggregate(cusDemSegMS,
                            list(cusDemSegMS$CusDemComparitor), mean)

   cusDemSegMPD <- subset(binarizedDF, CusDemComparitor == '1011')
   cusDemSegMPD <- aggregate(cusDemSegMPD,
                             list(cusDemSegMPD$CusDemComparitor), mean)

   cusDemSegMP <- subset(binarizedDF, CusDemComparitor == '1010')
   cusDemSegMP <- aggregate(cusDemSegMP,
                            list(cusDemSegMP$CusDemComparitor), mean)

   cusDemSegMD <- subset(binarizedDF, CusDemComparitor == '1001')
   cusDemSegMD <- aggregate(cusDemSegMD,
                            list(cusDemSegMD$CusDemComparitor), mean)

   cusDemSegM <- subset(binarizedDF, CusDemComparitor == '1000')
   cusDemSegM <- aggregate(cusDemSegM,
                           list(cusDemSegM$CusDemComparitor), mean)

   #Female Segments
   cusDemSegFSPD <- subset(binarizedDF, CusDemComparitor == '0111')
   cusDemSegFSPD <- aggregate(cusDemSegFSPD,
                              list(cusDemSegFSPD$CusDemComparitor), mean)

   cusDemSegFSP <- subset(binarizedDF, CusDemComparitor == '0110')
   cusDemSegFSP <- aggregate(cusDemSegFSP,
                             list(cusDemSegFSP$CusDemComparitor), mean)

   cusDemSegFSD <- subset(binarizedDF, CusDemComparitor == '0101')
   cusDemSegFSD <- aggregate(cusDemSegFSD,
                             list(cusDemSegFSD$CusDemComparitor), mean)

   cusDemSegFS <- subset(binarizedDF, CusDemComparitor == '0100')
   cusDemSegFS <- aggregate(cusDemSegFS,
                            list(cusDemSegFS$CusDemComparitor), mean)

   cusDemSegFPD <- subset(binarizedDF, CusDemComparitor == '0011')
   cusDemSegFPD <- aggregate(cusDemSegFPD,
                             list(cusDemSegFPD$CusDemComparitor), mean)

   cusDemSegFP <- subset(binarizedDF, CusDemComparitor == '0010')
   cusDemSegFP <- aggregate(cusDemSegFP,
                            list(cusDemSegFP$CusDemComparitor), mean)

   cusDemSegFD <- subset(binarizedDF, CusDemComparitor == '0001')
   cusDemSegFD <- aggregate(cusDemSegFD,
                            list(cusDemSegFD$CusDemComparitor), mean)

   cusDemSegF <- subset(binarizedDF, CusDemComparitor == '0000')
   cusDemSegF <- aggregate(cusDemSegF,
                           list(cusDemSegF$CusDemComparitor), mean)

   #Creates a customer segmented based data frame with aggregated mean values
   cusDemSegDF <- bind_rows(cusDemSegMSPD, cusDemSegMSP, cusDemSegMSD,
                            cusDemSegMS, cusDemSegMPD, cusDemSegMP,
                            cusDemSegMD, cusDemSegM,
                            cusDemSegFSPD, cusDemSegFSP, cusDemSegFSD,
                            cusDemSegFS, cusDemSegFPD, cusDemSegFP,
                            cusDemSegFD, cusDemSegF)

   #Assigns the proper matching column names instead of the default numbering
   row.names(cusDemSegDF) <- c("MSPD", "MSP", "MSD", "MS",
                               "MPD","MP", "MD","M",
                               "FSPD", "FSP", "FSD", "FS",
                               "FPD", "FP", "FD", "F")
   row.names(cusDemSegDF) <- paste(cusDemSegDF$Group.1, row.names(cusDemSegDF),
                                   sep = "_")

   #Removes the now extraneous 'CusDemComparitor' & 'Group.' columns for clarity
   cusDemSegDF <- cusDemSegDF[,!(names(cusDemSegDF)
                                 %in% c("Group.1", "CusDemComparitor"))]

   assign('cusDemSegDF', cusDemSegDF, envir = .GlobalEnv)
}

#Writes a '.csv' file in order to save the data frame results
writeCustomerDemographicBasedSegmentationsCSVFile <- function() {
   write.csv(cusDemSegDF, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/Manual_Customer_Demographic_Segments.csv")
}

#Automatically generates all the possible customer demographic segments
determineCustomerDemographicSegments <- function() {
   cusDemSegs <- data.frame(expand.grid(rep(list(0:1), 4)))

   #Assigns the proper matching column names instead of the default 'Var.'
   colnames(cusDemSegs) <- c("Gender", "SeniorCitizen", "Partner", "Dependents")

   cusDemSegs$Comparitor <- paste(cusDemSegs$Gender,
                                  cusDemSegs$SeniorCitizen,
                                  cusDemSegs$Partner,
                                  cusDemSegs$Dependents, sep = "")

   return(cusDemSegs)
}

#Automatically generates all the possible product bundle combination segments
determineProductBundleCombinationSegments <- function() {
   #Creates a data frame of all possible product combinations
   prodBundCombs <- data.frame(expand.grid(rep(list(0:1), 10)))

   #Removes those combinations which are not possible in context such as:
   #A customer can't have Multiple Lines if they have no Phone Service
   #A customer can't have any of the 6 internet services if they have no internet
   rowsToRemove <- c()
   for (i in 1:nrow(prodBundCombs)) {
      if (prodBundCombs[i,1] == 0 & prodBundCombs[i,2] == 1) {
         rowsToRemove <- c(rowsToRemove, i)
      }

      if (prodBundCombs[i,9] == 0 & prodBundCombs[i,10] == 0) {
         if (prodBundCombs[i,3] == 1 | prodBundCombs[i,4] == 1 |
             prodBundCombs[i,5] == 1 | prodBundCombs[i,6] == 1 |
             prodBundCombs[i,7] == 1 | prodBundCombs[i,8] == 1) {
            rowsToRemove <- c(rowsToRemove, i)
          }
       }
    }

   prodBundCombs <- filter(prodBundCombs,
                           !(rownames(prodBundCombs) %in% rowsToRemove))

   #Assigns the proper matching column names instead of the default 'Var.'
   colnames(prodBundCombs) <- c("PhoneService", "MultipleLines",
                                "OnlineSecurity", "OnlineBackup",
                                "DeviceProtection", "TechSupport",
                                "StreamingTV", "StreamingMovies",
                                "ISDSL", "ISFiberOptic")

   #Creates a concatenated column for much faster comparisons & filtering
   prodBundCombs$Comparitor <- paste(prodBundCombs$PhoneService,
                                     prodBundCombs$MultipleLines,
                                     prodBundCombs$OnlineSecurity,
                                     prodBundCombs$OnlineBackup,
                                     prodBundCombs$DeviceProtection,
                                     prodBundCombs$TechSupport,
                                     prodBundCombs$StreamingTV,
                                     prodBundCombs$StreamingMovies,
                                     prodBundCombs$ISDSL,
                                     prodBundCombs$ISFiberOptic, sep = "")

   rownames(prodBundCombs) <- NULL

   return(prodBundCombs)
}

createComparitorDF <- function(binarizedDF) {
   comparitorDF <- binarizedDF
   comparitorDF$ProdComparitor <- paste(comparitorDF$PhoneService,
                                        comparitorDF$MultipleLines,
                                        comparitorDF$OnlineSecurity,
                                        comparitorDF$OnlineBackup,
                                        comparitorDF$DeviceProtection,
                                        comparitorDF$TechSupport,
                                        comparitorDF$StreamingTV,
                                        comparitorDF$StreamingMovies,
                                        comparitorDF$ISDSL,
                                        comparitorDF$ISFiberOptic,
                                        sep = "")

   comparitorDF$CusComparitor <- paste(comparitorDF$gender,
                                       comparitorDF$SeniorCitizen,
                                       comparitorDF$Partner,
                                       comparitorDF$Dependents,
                                       sep = "")

   return(comparitorDF)
}

#Creates the empty optimal price and customer count matrices for all possible
#customer and product segment combinations
createOptimalPriceAndAssociatedMatrices <- function(prodBundCombs, cusDemSegs) {
   optPriceMatrix <- data.frame(matrix(0, nrow = nrow(prodBundCombs),
                                       ncol = nrow(cusDemSegs)))
   colnames(optPriceMatrix) <- cusDemSegs$Comparitor
   rownames(optPriceMatrix) <- prodBundCombs$Comparitor

   potentialDemandMatrix <- optPriceMatrix
   optDemandMatrix <- optPriceMatrix
   maxRevenueMatrix <- optPriceMatrix

   assign('optPriceMatrix', optPriceMatrix, envir = .GlobalEnv)
   assign('potentialDemandMatrix', potentialDemandMatrix, envir = .GlobalEnv)
   assign('optDemandMatrix', optDemandMatrix, envir = .GlobalEnv)
   assign('maxRevenueMatrix', maxRevenueMatrix, envir = .GlobalEnv)
}

calcMaxMonthlyCharges <- function(comparitorDF) {
   maxMonthlyCharges <- max(comparitorDF$MonthlyCharges)
   return(maxMonthlyCharges)
}

priceOptimizer <- function(iterationSize, selectionPriceMean,
                           selectionPriceSTDEV, selectionPotentialDemand) {
   bestRevenue = 0
   for (optPrice in iterationSize) {

      #Calculates the optimized price for a specific 'prodBundCombs' combination
      #across all customers, based on which results in the highest revenue
      selectionProbabilityNoPurchase <- dnorm(optPrice,
                                              selectionPriceMean,
                                              selectionPriceSTDEV,
                                              log = FALSE)
      selectionDemand <- (selectionPotentialDemand *
                          selectionProbabilityNoPurchase)
      selectionOptPriceMaxRevenue <- (selectionDemand * optPrice)

      if (selectionOptPriceMaxRevenue >= bestRevenue) {
         bestDemand <- selectionDemand
         bestRevenue <- selectionOptPriceMaxRevenue
         bestPrice <- optPrice
         results <- c(bestPrice, bestRevenue, bestDemand)
      }
   }
   return(results)
}

#The looping process below takes several hours, avoid running this to test
#the rest of the program fully

#Finds all customer records which made use of the specific customer demographic
#& products combination, selected from 'prodBundCombs' and 'cusDemSegs'
calcOptimalPrices <- function(prodBundCombs, cusDemSegs, comparitorDF,
                              maxCharge, quickTest) {
   maxNumCol = nrow(cusDemSegs)
   maxNumRow = nrow(prodBundCombs)
   if (quickTest == TRUE) {
      maxNumCol = 3
      maxNumRow = 6
   }
   for (i in 1:maxNumCol) {
      for (j in 1:maxNumRow) {
         selection <- data.frame()
         for (k in 1:nrow(comparitorDF)) {
            if (((compare(prodBundCombs[j,11], comparitorDF[k,21], allowAll = TRUE))[[1]]) &
                ((compare(cusDemSegs[i,5], comparitorDF[k,22], allowAll = TRUE))[[1]])) {
               selection<- rbind(selection, comparitorDF[k,14])
            }
         }
         potentialDemandMatrix[j,i] <- nrow(selection)
         assign('potentialDemandMatrix', potentialDemandMatrix,
                envir = .GlobalEnv)
         bestResults <- c()
         bestDemand <- 0
         bestRevenue <- 0
         bestPrice <- 0
         if (nrow(selection) == 0) {
            bestPrice <- NA
         }
         if (nrow(selection) == 1) {
            bestPrice <- sum(selection$MonthlyCharges)
         }
         if (nrow(selection) > 1) {
            #Extracts the necessary values for Price Optimization
            colnames(selection) <- c("MonthlyCharges")
            selectionPriceMean <- mean(selection$MonthlyCharges)
            selectionPriceSTDEV <- sd(selection$MonthlyCharges)
            selectionPotentialDemand <- nrow(selection)

            largePriceIter <- seq(0, maxCharge + 11.25, by = 10)
            bestPrice <- (priceOptimizer(largePriceIter, selectionPriceMean, selectionPriceSTDEV, selectionPotentialDemand))[1]
            smallPriceIter <- seq(bestPrice - 10, bestPrice + 10, by = 1)
            bestPrice <- (priceOptimizer(smallPriceIter, selectionPriceMean, selectionPriceSTDEV, selectionPotentialDemand))[1]
            smallerPriceIter <- seq(bestPrice - 1, bestPrice + 1, by = 0.1)
            bestPrice <- (priceOptimizer(smallerPriceIter, selectionPriceMean, selectionPriceSTDEV, selectionPotentialDemand))[1]
            smallestPriceIter <- seq(bestPrice - 0.1, bestPrice + 0.1, by = 0.01)
            bestResults <- priceOptimizer(smallestPriceIter, selectionPriceMean, selectionPriceSTDEV, selectionPotentialDemand)
            bestPrice <- bestResults[1]
            bestRevenue <- bestResults[2]
            bestDemand <- bestResults[3]
         }
         optPriceMatrix[j,i] <- bestPrice
         maxRevenueMatrix[j,i] <- bestRevenue
         optDemandMatrix[j,i] <- bestDemand

         assign('optPriceMatrix', optPriceMatrix, envir = .GlobalEnv)
         assign('optDemandMatrix', optDemandMatrix, envir = .GlobalEnv)
         assign('maxRevenueMatrix', maxRevenueMatrix, envir = .GlobalEnv)

         print(paste0("Customer Count: ", nrow(selection), " Product: ",
                      prodBundCombs[j,11]," in Demographic: ", cusDemSegs[i,5],
                      " Optimal Price: $", bestPrice, " " , optPriceMatrix[j,i], " ", j, " ", i))
      }
   }
}

#For testing purposes, to save work already carried out
saveBackupMatrices <- function() {
   savedOptPriceMatrix <- optPriceMatrix
   savedPotentialDemandMatrix <- potentialDemandMatrix
   savedOptDemandMatrix <- optDemandMatrix
   savedMaxRevenueMatrix <- maxRevenueMatrix

   assign('savedOptPriceMatrix', savedOptPriceMatrix, envir = .GlobalEnv)
   assign('savedPotentialDemandMatrix', savedPotentialDemandMatrix,
          envir = .GlobalEnv)
   assign('savedOptDemandMatrix', savedOptDemandMatrix, envir = .GlobalEnv)
   assign('savedMaxRevenueMatrix', savedMaxRevenueMatrix, envir = .GlobalEnv)
}

#Sums the matrices customer count to insure no errors within the program
testTotalCustomerCount <- function() {
   totalCustomerCount <- sum(savedPotentialDemandMatrix)
   print(paste0("Total Customer Count: ", totalCustomerCount))
}

#Orders the matrices by the best selling bundles according to customer count
#Uses row sums & does not take into consideration customer churn
#savedPotentialDemandMatrix <- savedpotentialDemandMatrix[order(rowSums(-savedpotentialDemandMatrix)),]
#savedOptPriceMatrix <- savedOptPriceMatrix[match(rownames(savedpotentialDemandMatrix),
#                                                 rownames(savedOptPriceMatrix)),]

#Adds TotalCount & AverageOptimalPrice columns to the respective matrices
#savedPotentialDemandMatrix$TotalCount <- rowSums(savedPotentialDemandMatrix)
#savedOptPriceMatrix$AverageOptimalPrice <- round(rowMeans(savedOptPriceMatrix, na.rm = TRUE), 2)


#prodBundles <- savedPotentialDemandMatrix[rowSums(savedPotentialDemandMatrix[])>0,]
#uniqueProdBundles <- nrow(prodBundles) #Count of unique product bundles used

#Writes CSV files in order to save the optimal price data frame results
writeOptimalPriceRelatedMatricesCSVFiles <- function() {
   write.csv(savedOptPriceMatrix, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/TCC_Optimal_Price_Matrix.csv")
   write.csv(savedPotentialDemandMatrix, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/TCC_Customer_Count_Potential_Demand_Matrix.csv")
   write.csv(savedOptDemandMatrix, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/TCC_Optimal_Demand_Matrix.csv")
   write.csv(savedMaxRevenueMatrix, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/TCC_Max_Revenue_Matrix.csv")
}

#Changes the following relevant columns & variables into factors as directed
#More efficient and quicker than above method, may yield different results
factorizeDF <- function(toFactorizeDF) {
   toFactorizeDF$gender <- as.factor(toFactorizeDF$gender)
   toFactorizeDF$SeniorCitizen <- as.factor(toFactorizeDF$SeniorCitizen )
   toFactorizeDF$Partner <- as.factor(toFactorizeDF$Partner)
   toFactorizeDF$Dependents <- as.factor(toFactorizeDF$Dependents)
   toFactorizeDF$PhoneService <- as.factor(toFactorizeDF$PhoneService)
   toFactorizeDF$MultipleLines <- as.factor(toFactorizeDF$MultipleLines)
   toFactorizeDF$InternetService <- as.factor(toFactorizeDF$InternetService)
   toFactorizeDF$OnlineSecurity <- as.factor(toFactorizeDF$OnlineSecurity)
   toFactorizeDF$OnlineBackup <- as.factor(toFactorizeDF$OnlineBackup)
   toFactorizeDF$DeviceProtection <- as.factor(toFactorizeDF$DeviceProtection)
   toFactorizeDF$TechSupport <- as.factor(toFactorizeDF$TechSupport)
   toFactorizeDF$StreamingTV <- as.factor(toFactorizeDF$StreamingTV)
   toFactorizeDF$StreamingMovies <- as.factor(toFactorizeDF$StreamingMovies)
   toFactorizeDF$Contract <- as.factor(toFactorizeDF$Contract)
   toFactorizeDF$Churn <- as.factor(toFactorizeDF$Churn)

   return(toFactorizeDF)
}

#Splits the data frame into the 2 separate testing and training data frames
splitIntoTrainingTestingDFs <- function(toSplitDF) {
   #Sets a predetermined seed for reproducible results in future repeat tests
   set.seed(123)

   #Randomly selects 2/3 of the data frame's rows as a new sub-index
   trainDF <- sample(1:nrow(toSplitDF), nrow(toSplitDF) * (2/3))

   #Uses the 'train' index in order to split the 'toSplitDF' data set into 2 parts
   toSplitDF.train <- toSplitDF[trainDF,] #Builds the model
   toSplitDF.test <- toSplitDF[-trainDF,] #Trains the model

   trainTestDFs <- list('trainDF' = toSplitDF.train, 'testDF' = toSplitDF.test)

   return(trainTestDFs)
}

#Asks whether or not the user would like to print out the Model CM & its Results
printCMResultsQuestionaire <- function() {
   i = 0
   while (i != 1) {
      decision <- readline(prompt = "Would you like to print out the Model's Confusion Matrix & Results? (Y | N): ")

      if (is.character(decision) == FALSE) {
         print("This input is invalid. Please try again.")
      }
      if (is.character(decision) == TRUE) {
         decision <- toupper(decision)
         if (decision != 'Y' & decision != 'N') {
            print("This input is invalid. Please try again.")
         }
         if (decision == 'Y') {
            i = 1
            return(TRUE)
         }
         if (decision == 'N') {
            i = 1
            return(FALSE)
         }
      }
   }
}

#Creates a base from the actual test data frame churn for easy model comparison
createBaseResults <- function(trainTestDFsList) {
   testingDF <- trainTestDFsList$testDF

   baseTP <- sum(str_count(testingDF$Churn, 'Yes'))
   baseTN <- sum(str_count(testingDF$Churn, 'No'))
   baseFP <- 0
   baseFN <- 0

   baseSensitivity <- (baseTP / (baseTP + baseFN))
   baseSpecificity <- (baseTN / (baseTN + baseFP))
   baseAccuracy <- (baseTP + baseTN) / (baseTP + baseTN + baseFP + baseFN)
   baseBalancedAccuracy <- ((baseSensitivity + baseSpecificity) / 2)

   baseResults <- list('bSen' = baseSensitivity,
                       'bSpe' = baseSpecificity,
                       'bAcc' = baseAccuracy,
                       'bBalAcc' = baseBalancedAccuracy,
                       'bTP' = baseTP, 'bTN' = baseTN,
                       'bFP' = baseFP, 'bFN' = baseFN)

   return(baseResults)
}

#Decision Tree Model
createDecisionTreeModel <- function(trainTestDFsList, printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   #Grows the Tree, with 'Churn' as the variable attempting to be modeled
   treeFit <- rpart(Churn ~ ., #Formula uses all data frame variables
                    data = trainingDF,
                    method = "class",
                    control = rpart.control(xval = 10, minsplit = 50),
                    parms = list(split = "gini"))

   #Creates the confusion matrix called 'treeCM'
   #Extracts the vector of predicted class for each observation in 'testingDF'
   treePred <- predict(treeFit, testingDF, type = "class")
   #Extracts the actual class of each observation in 'testingDF'
   treeActual <- testingDF$Churn

   #Builds the confusion matrix 'treeCM', otherwise known as the contingency
   #matrix of predicted vs actual
   treeCM <- table(treePred, treeActual)

   #Extracts the True Positive (TP), True Negative (TN), False Positive (FP)
   #& False Negative (FN) values from the confusion matrix 'treeCM'
   treeTP <- treeCM[2,2]
   treeTN <- treeCM[1,1]
   treeFP <- treeCM[2,1]
   treeFN <- treeCM[1,2]

   #Calculates the tree model recall / sensitivity
   #(TPR = TP / P = TP / (TP + FN)) of the model
   treeSensitivity <- (treeTP / (treeTP + treeFN))

   #Calculates the tree model specificity
   #(TNR = TN / N = TN / (TN + FP)) of the model
   treeSpecificity <- (treeTN / (treeTN + treeFP))

   #Calculates the tree model accuracy of the model (A = (TP + TN) / S)
   treeAccuracy <- (treeTP + treeTN) / (treeTP + treeTN + treeFP + treeFN)

   #Calculates the tree model balanced accuracy of the model
   #(BA = (((TP / P) + (TN / N)) / 2))
   treeBalancedAccuracy <- ((treeSensitivity + treeSpecificity) / 2)

   #Plots & prints out the Decision Tree Model results
   #Plots the decision tree in a more aesthetically pleasing fashion
   rpart.plot(treeFit, type = 1,
              extra = 1,
              main = "Classification Tree for 'Churn' Prediction")

   #Prints out the confusion matrix 'treeCM', using the predicted
   #before actual results, along with margins
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Decision Tree Model: ")
      print(addmargins(treeCM))

      print(paste("Sensitivity of the Decision Tree Model: ", treeSensitivity))
      print(paste("Specificity of the Decision Tree Model: ", treeSpecificity))
      print(paste("Accuracy of the Decision Tree Model: ", treeAccuracy))
      print(paste("Balanced Accuracy of the Decision Tree Model: ",
                  treeBalancedAccuracy))
   }

   decisionTreeResults <- list('dtPred' = treePred,
                               'dtSen' = treeSensitivity,
                               'dtSpe' = treeSpecificity,
                               'dtAcc' = treeAccuracy,
                               'dtBalAcc' = treeBalancedAccuracy,
                               'dtTP' = treeTP, 'dtTN' = treeTN,
                               'dtFP' = treeFP, 'dtFN' = treeFN)

   return(decisionTreeResults)
}

#Logistic Regression Model
createLogisticRegressionModel <- function(trainTestDFsList, printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   logitReg <- glm(Churn ~ ., #Formula uses all data frame variables
                   data = trainingDF,
                   family = "binomial")

   #Computes the predicted probabilities
   logitRegPred <- predict(logitReg, testingDF, type = "response")
   testingDF$logitRegPred <- logitRegPred
   #Chooses 0.5 as the cutoff for 1 VS. 0 classes
   testingDF$logitRegChurnPred <- ifelse(testingDF$logitRegPred > 0.5, 1, 0)

   logitRegPred <- testingDF$logitRegChurnPred
   logitRegActual <- testingDF$Churn
   logitRegCM <- table(logitRegPred, logitRegActual)

   testingDF <- testingDF[, !(names(testingDF) %in% c("logitRegPred",
                                                      "logitRegChurnPred"))]

   logitRegTP <- logitRegCM[2,2]
   logitRegTN <- logitRegCM[1,1]
   logitRegFP <- logitRegCM[2,1]
   logitRegFN <- logitRegCM[1,2]

   logitRegSensitivity <- (logitRegTP / (logitRegTP + logitRegFN))
   logitRegSpecificity <- (logitRegTN / (logitRegTN + logitRegFP))
   logitRegAccuracy <- ((logitRegTP + logitRegTN) /
                        (logitRegTP + logitRegTN + logitRegFP + logitRegFN))
   logitRegBalancedAccuracy <- ((logitRegSensitivity + logitRegSpecificity) / 2)

   #Prints out the Logistic Regression Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Logistic Regression Model: ")
      print(addmargins(logitRegCM))

      print(paste("Sensitivity of the Logistic Regression Model: ",
                  logitRegSensitivity))
      print(paste("Specificity of the Logistic Regression Model: ",
                  logitRegSpecificity))
      print(paste("Accuracy of the Logistic Regression Model: ",
                  logitRegAccuracy))
      print(paste("Balanced Accuracy of the Logistic Regression Model: ",
                  logitRegBalancedAccuracy))
   }

   logitRegResults <- list('lgPred' = logitRegPred,
                           'lgSen' = logitRegSensitivity,
                           'lgSpe' = logitRegSpecificity,
                           'lgAcc' = logitRegAccuracy,
                           'lgBalAcc' = logitRegBalancedAccuracy,
                           'lgTP' = logitRegTP, 'lgTN' = logitRegTN,
                           'lgFP' = logitRegFP, 'lgFN' = logitRegFN)

   return(logitRegResults)
}

#K-Nearest Neighbors Model
createKNearestNeighborsModel <- function(trainTestDFsList, printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   knnCtrl <- trainControl(method = "cv", number = 10)

   knnFit <- train(Churn ~ ., data = trainingDF, method = "knn",
                   trControl = knnCtrl, preProcess = c("center","scale"),
                   tuneGrid = expand.grid(k = 1:10))
   plot(knnFit)

   knnPred <- predict(knnFit, testingDF)
   knnActual <- testingDF$Churn
   knnCM <- table(knnPred, knnActual)

   knnTP <- knnCM[2,2]
   knnTN <- knnCM[1,1]
   knnFP <- knnCM[2,1]
   knnFN <- knnCM[1,2]

   knnSensitivity <- (knnTP / (knnTP + knnFN))
   knnSpecificity <- (knnTN / (knnTN + knnFP))
   knnAccuracy <- (knnTP + knnTN) / (knnTP + knnTN + knnFP + knnFN)
   knnBalancedAccuracy <- ((knnSensitivity + knnSpecificity) / 2)

   #Prints out the K-Nearest Neighbor Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the K-Nearest Neighbor Model: ")
      print(addmargins(knnCM))

      print(paste("Sensitivity of the K-Nearest Neighbor Model: ",
                  knnSensitivity))
      print(paste("Specificity of the K-Nearest Neighbor Model: ",
                  knnSpecificity))
      print(paste("Accuracy of the K-Nearest Neighbor Model: ", knnAccuracy))
      print(paste("Balanced Accuracy of the K-Nearest Neighbor Model: ",
                  knnBalancedAccuracy))
   }

   knnResults <- list('knnPred' = knnPred,
                      'knnSen' = knnSensitivity,
                      'knnSpe' = knnSpecificity,
                      'knnAcc' = knnAccuracy,
                      'knnBalAcc' = knnBalancedAccuracy,
                      'knnTP' = knnTP, 'knnTN' = knnTN,
                      'knnFP' = knnFP, 'knnFN' = knnFN)

   return(knnResults)
}

#Naive Bayes Classifier Model
createNaiveBayesClassifierModel <- function(trainTestDFsList, printCMResults) {
   testingDF <- trainTestDFsList$testDF
   trainingDF <- trainTestDFsList$trainDF

   nBayesFit <- naiveBayes(Churn ~ ., #Formula uses all data frame variables
                           data = trainingDF)

   nBayesPred <- predict(nBayesFit, testingDF, type = "raw")
   nBayesPredClass <- predict(nBayesFit, testingDF, type = "class")
   nBayesActual <- testingDF$Churn
   nBayesCM <- table(nBayesPredClass, nBayesActual)

   nBayesTP <- nBayesCM[2,2]
   nBayesTN <- nBayesCM[1,1]
   nBayesFP <- nBayesCM[2,1]
   nBayesFN <- nBayesCM[1,2]

   nBayesSensitivity <- (nBayesTP / (nBayesTP + nBayesFN))
   nBayesSpecificity <- (nBayesTN / (nBayesTN + nBayesFP))
   nBayesAccuracy <- ((nBayesTP + nBayesTN) /
                      (nBayesTP + nBayesTN + nBayesFP + nBayesFN))
   nBayesBalancedAccuracy <- ((nBayesSensitivity + nBayesSpecificity) / 2)

   #Prints out the Naive Bayes Classifier Model results
   if (printCMResults == TRUE) {
      print("Confusion Matrix of the Naive Bayes Classifier Model: ")
      print(addmargins(nBayesCM))

      print(paste("Sensitivity of the Naive Bayes Classifier Model: ",
                  nBayesSensitivity))
      print(paste("Specificity of the Naive Bayes Classifier Model: ",
                  nBayesSpecificity))
      print(paste("Accuracy of the Naive Bayes Classifier Model: ",
                  nBayesAccuracy))
      print(paste("Balanced Accuracy of the Naive Bayes Classifier Model: ",
                  nBayesBalancedAccuracy))
   }

   nBayesResults <- list('nbPred' = nBayesPredClass,
                         'nbSen' = nBayesSensitivity,
                         'nbSpe' = nBayesSpecificity,
                         'nbAcc' = nBayesAccuracy,
                         'nbBalAcc' = nBayesBalancedAccuracy,
                         'nbTP' = nBayesTP, 'nbTN' = nBayesTN,
                         'nbFP' = nBayesFP, 'nbFN' = nBayesFN)

   return(nBayesResults)
}

#Ensemble Methods Model
createEnsembleMethodsModel <- function(dtR, lgR, knnR, nbR, bR,
                                       trainTestDFsList, printCMResults) {

   testingDF <- trainTestDFsList$testDF

   ensembleDF <- subset(testingDF, select = c("Churn"))

   #Assembles all the predicted Churn results from every model previously ran
   ensembleDF$treeChurnPred <- dtR$dtPred
   ensembleDF$logitChurnRegPred <- lgR$lgPred
   ensembleDF$knnChurnPred <- knnR$knnPred
   ensembleDF$nBayesChurnPred <- nbR$nbPred

   #Factors become 'NAN' values when setting them to binary values; must be removed
   ensembleDF <- remove.factors(ensembleDF)

   ensembleDF$Churn[ensembleDF$Churn == "No"] <- 0
   ensembleDF$Churn[ensembleDF$Churn == "Yes"] <- 1

   ensembleDF$treeChurnPred[ensembleDF$treeChurnPred == "No"] <- 0
   ensembleDF$treeChurnPred[ensembleDF$treeChurnPred == "Yes"] <- 1

   ensembleDF$knnChurnPred[ensembleDF$knnChurnPred == "No"] <- 0
   ensembleDF$knnChurnPred[ensembleDF$knnChurnPred == "Yes"] <- 1

   ensembleDF$nBayesChurnPred[ensembleDF$nBayesChurnPred == "No"] <- 0
   ensembleDF$nBayesChurnPred[ensembleDF$nBayesChurnPred == "Yes"] <- 1

   #Ensures no other problems by transforming all columns to numeric data type
   ensembleDF$Churn <- as.numeric(ensembleDF$Churn)
   ensembleDF$treeChurnPred <- as.numeric(ensembleDF$treeChurnPred)
   ensembleDF$knnChurnPred <- as.numeric(ensembleDF$knnChurnPred)
   ensembleDF$nBayesChurnPred <- as.numeric(ensembleDF$nBayesChurnPred)

   #Calculates 2 Vote Combinations' Summed Accuracy
   treeLogReg <- dtR$dtAcc + lgR$lgAcc #1100
   treeKnn <- dtR$dtAcc  + knnR$knnAcc #1010
   treeNBayes <- dtR$dtAcc  + nbR$nbAcc #1001
   logRegKnn <- lgR$lgAcc + knnR$knnAcc #0110
   logRegNBayes <- lgR$lgAcc + nbR$nbAcc #0101
   knnNBayes <- knnR$knnAcc + nbR$nbAcc #0011

   #Calculates which 2 vote combinations trump their opposites
   treeLogRegVSknnNBayes <- treeLogReg - knnNBayes #1100 vs 0011 #1100 Wins
   treeKnnVSlogRegNBayes <- treeKnn - logRegNBayes #1010 vs 0101 #1010 Wins
   treeNBayesVSlogRegKnn <- treeNBayes - logRegKnn #1001 vs 0110 #0110 Wins

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$logitChurnRegPred == 2,
                                          1, 0)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$knnChurnPred == 2,
                                          1, 0)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$nBayesChurnPred == 2,
                                          0, 1)

   ensembleDF$ensembleChurnPred <- ifelse(ensembleDF$treeChurnPred
                                          + ensembleDF$logitChurnRegPred
                                          + ensembleDF$knnChurnPred
                                          + ensembleDF$nBayesChurnPred >= 3,
                                          1, 0)


   ensemblePred <- ensembleDF$ensembleChurnPred
   ensembleActual <- ensembleDF$Churn
   ensembleCM <- table(ensemblePred, ensembleActual)

   ensembleTP <- ensembleCM[2,2]
   ensembleTN <- ensembleCM[1,1]
   ensembleFP <- ensembleCM[2,1]
   ensembleFN <- ensembleCM[1,2]

   ensembleSensitivity <- (ensembleTP / (ensembleTP + ensembleFN))
   ensembleSpecificity <- (ensembleTN / (ensembleTN + ensembleFP))
   ensembleAccuracy <- ((ensembleTP + ensembleTN) /
                        (ensembleTP + ensembleTN + ensembleFP + ensembleFN))
   ensembleBalancedAccuracy <- ((ensembleSensitivity + ensembleSpecificity) / 2)

   #Collects the various model results into easily comparable tables
   eResultsAccuracy <- c(dtR$dtAcc, lgR$lgAcc, knnR$knnAcc,
                         nbR$nbAcc, ensembleAccuracy, bR$bAcc)
   eResultsBalancedAccuracy <- c(dtR$dtBalAcc,
                                 lgR$lgBalAcc,
                                 knnR$knnBalAcc,
                                 nbR$nbBalAcc,
                                 ensembleBalancedAccuracy,
                                 bR$bBalAcc)
   eResultsOverall <- rbind(eResultsAccuracy, eResultsBalancedAccuracy)

   rownames(eResultsOverall) <- c("Accuracy",
                                  "Balanced Accuracy")
   colnames(eResultsOverall) <- c("Decision Tree",
                                  "Logistic Regression",
                                  "K-Nearest Neighbors",
                                  "Naive Bayes Classifier",
                                  "Ensemble",
                                  "Actual Churn Results")

   #Row binds all TP, TN, FP & FN counts into one combined Confusion Matrix
   eResultsTP <- c(dtR$dtTP, lgR$lgTP, knnR$knnTP, nbR$nbTP, ensembleTP, bR$bTP)
   eResultsTN <- c(dtR$dtTN, lgR$lgTN, knnR$knnTN, nbR$nbTN, ensembleTN, bR$bTN)
   eResultsFP <- c(dtR$dtFP, lgR$lgFP, knnR$knnFP, nbR$nbFP, ensembleFP, bR$bFP)
   eResultsFN <- c(dtR$dtFN, lgR$lgFN, knnR$knnFN, nbR$nbFN, ensembleFN, bR$bFN)
   eResultsCM <- rbind(eResultsTP, eResultsTN, eResultsFP, eResultsFN)

   #Renames the rows and columns of the 'eResultsCM' accordingly
   rownames(eResultsCM) <- c("True Positives",
                             "True Negatives",
                             "False Positives",
                             "False Negatives")
   colnames(eResultsCM) <- c("Decision Tree",
                             "Logistic Regression",
                             "K-Nearest Neighbors",
                             "Naive Bayes Classifier",
                             "Ensemble",
                             "Actual Churn Results")

   eResultsComplete <- rbind(eResultsOverall, eResultsCM)
   eResultsComplete <- as.data.frame.matrix(eResultsComplete)

   if (printCMResults == TRUE) {
      #Prints out the Ensemble Method results
      print("Model Results Comparison Tables: ")
      #print(eResultsCM)
      #print(eResultsOverall)
      print(eResultsComplete)
   }

   assign('eResultsComplete', eResultsComplete, envir = .GlobalEnv)
}

#Exports the churn modeling results as a '.csv' file
writeChurnModelingResultsCSVFile <- function() {
   write.csv(eResultsComplete, "~/Documents/github/Machine_Learning_R_Telco_Customer_Churn/Telco_Customer_Churn_Modeling.csv")
}

compareDFs <- function(testDF, trueDF) {
   equal <- isTRUE(all.equal(testDF, trueDF, check.attributes = FALSE))
   print(paste0("The dataframes are equal: ", equal))
   return(equal)
}

main <- function(autoTest) {
   if (autoTest) {
      baseDF <- createBaseDataFrame()
      loadTrueResults()
   }
   else {
      baseDF <- createBaseDataFrame()
   }
   adjustedDF <- adjustBaseDF(baseDF)

   binarizedDF <- binarizeDF(adjustedDF, baseDF)

   manualCustomerDemographicsSegmenter(binarizedDF)

   customerSegments <- determineCustomerDemographicSegments()
   productBundles <- determineProductBundleCombinationSegments()

   comparitorDF <- createComparitorDF(binarizedDF)
   createOptimalPriceAndAssociatedMatrices(productBundles, customerSegments)

   maximumMonthlyCharge <- calcMaxMonthlyCharges(comparitorDF)
   calcOptimalPrices(productBundles, customerSegments, comparitorDF,
                     maximumMonthlyCharge, autoTest)

   factorizedDF <- factorizeDF(adjustedDF)

   trainTestDFsList <- splitIntoTrainingTestingDFs(factorizedDF)

   printResults = TRUE
   if (!autoTest) {
      printResults <- printCMResultsQuestionaire()
   }

   bR <- createBaseResults(trainTestDFsList)
   dtMR <- createDecisionTreeModel(trainTestDFsList, printResults)
   lgMR <- createLogisticRegressionModel(trainTestDFsList, printResults)
   knnMR <- createKNearestNeighborsModel(trainTestDFsList, printResults)
   nbMR <- createNaiveBayesClassifierModel(trainTestDFsList, printResults)
   eMR <- createEnsembleMethodsModel(dtMR, lgMR, knnMR, nbMR, bR,
                                     trainTestDFsList, printResults)

   if (autoTest) {
      optPriceMatrix <- head(optPriceMatrix, 6)
      optPriceMatrix <- subset(optPriceMatrix[1:3])
      assign('optPriceMatrix', optPriceMatrix, envir = .GlobalEnv)

      optPriceResults <- compareDFs(optPriceMatrix, trueOptPriceMatrix)
      eResults <- compareDFs(eMR, trueEResultsComplete)
      if (optPriceResults == FALSE | eResults == FALSE) {
         stop('Mismatch in results')
      }
   }
}

main(TRUE)
