library(class)

CSV_DATA <- "wisc_bc_data.csv"

# Import csv file data
wisc <- read.csv(CSV_DATA)

# Create normalize function
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

# Remove id variable
wisc <- wisc[-1]

# Redefine diagnosis variable as factor with labels
wisc$diagnosis <- factor(wisc$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# Print true results regarding the diagnosis from the whole data
print("Diagnosis results in the whole dataset")
print(round(prop.table(table(wisc$diagnosis)) * 100, digits = 1))

# Apply normalize function to df in order to normalize data
wisc_n <- as.data.frame(lapply(wisc[2:31], normalize))

# Separate dataset to train and test data
wisc_train <- wisc_n[1:469,]
wisc_test <- wisc_n[470:569,]

# Extract train and test labels
wisc_train_labels <- wisc[1:469,1]
wisc_test_labels <- wisc[470:569,1]

# Test knn algorithm with multiple k values
k_values <- c(3, 7, 10, 21, 27, 30, 50, 100)
false_negatives <- c()
false_positives <- c()
error_rate <- c()

# Apply knn algorithm to the train data for the k_values
for (k_val in k_values) {
    wisc_test_pred <- knn(train = wisc_train, 
                          test = wisc_test, 
                          cl = wisc_train_labels, 
                          k = k_val)

    freq <- table(wisc_test_labels, wisc_test_pred)
    false_negatives <- c(false_negatives, freq[2,1])
    false_positives <- c(false_positives, freq[1,2])
    error_rate <- c(error_rate, tail(false_negatives, 1) + tail(false_positives, 1))
}


# Create results dataframe
results <- data.frame(K_values = k_values, False_Negatives = false_negatives,
                      False_Positives = false_positives, Error_Rate = error_rate)

# print results
print(results)
