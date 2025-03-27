# Load required libraries
library(C50)       # For Decision Tree
library(e1071)     # For SVM
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation

# Function for Decision Tree (C5.0)
decision_tree_model <- function() {
  # Load the dataset
  lenses_data <- read.csv("C:/prgms/lenses.csv")
  
  # Convert categorical columns to factors
  lenses_data$age <- factor(lenses_data$age, levels = c("young", "pre-presbyopic", "presbyopic"))
  lenses_data$prescription <- factor(lenses_data$prescription, levels = c("myope", "hypermetrope"))
  lenses_data$astigmatic <- factor(lenses_data$astigmatic, levels = c("no", "yes"))
  lenses_data$tear_rate <- factor(lenses_data$tear_rate, levels = c("reduced", "normal"))
  lenses_data$class <- factor(lenses_data$class, levels = c("no contact lenses", "soft contact lenses", "hard contact lenses"))
  
  # Train a C5.0 decision tree model
  model <- C5.0(class ~ age + prescription + astigmatic + tear_rate, data = lenses_data)
  
  # Visualize the decision tree
  plot(model, main = "Decision Tree Visualization", type = "simple")
  
  # Function for custom user input
  get_user_input <- function() {
    cat("Enter the following options:\n")
    cat("1. Age (1: young, 2: pre-presbyopic, 3: presbyopic): ")
    age <- as.integer(readline())
    age <- c("young", "pre-presbyopic", "presbyopic")[age]
    
    cat("2. Prescription (1: myope, 2: hypermetrope): ")
    prescription <- as.integer(readline())
    prescription <- c("myope", "hypermetrope")[prescription]
    
    cat("3. Astigmatic (1: no, 2: yes): ")
    astigmatic <- as.integer(readline())
    astigmatic <- c("no", "yes")[astigmatic]
    
    cat("4. Tear rate (1: reduced, 2: normal): ")
    tear_rate <- as.integer(readline())
    tear_rate <- c("reduced", "normal")[tear_rate]
    
    return(data.frame(
      age = factor(age, levels = c("young", "pre-presbyopic", "presbyopic")),
      prescription = factor(prescription, levels = c("myope", "hypermetrope")),
      astigmatic = factor(astigmatic, levels = c("no", "yes")),
      tear_rate = factor(tear_rate, levels = c("reduced", "normal"))
    ))
  }
  
  # Get user input and make a prediction
  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
}

# Function for SVM Classifier
svm_classifier <- function() {
  # Load the dataset
  dataset_path <-"C:/prgms/banknote.csv"
  banknote_data <- read.csv(dataset_path)
  
  # Rename columns if necessary
  colnames(banknote_data) <- c("variance", "skewness", "kurtosis", "entropy", "class")
  
  # Ensure the class column is a factor
  banknote_data$class <- factor(banknote_data$class, levels = c(0, 1), labels = c("Authentic", "Forged"))
  
  # Split the data into training and testing sets
  set.seed(123)  # For reproducibility
  train_indices <- sample(1:nrow(banknote_data), 0.8 * nrow(banknote_data))
  train_data <- banknote_data[train_indices, ]
  test_data <- banknote_data[-train_indices, ]
  
  # Train the SVM classifier with an RBF kernel
  svm_model <- svm(class ~ variance + skewness, data = train_data, kernel = "radial")
  
  # Test the model on the test data
  test_predictions <- predict(svm_model, test_data)
  accuracy <- mean(test_predictions == test_data$class) * 100
  cat("Model Accuracy on Test Data:", accuracy, "%\n")
  
  # Function for user input
  get_user_input <- function() {
    variance <- as.numeric(readline(prompt = "Variance: "))
    skewness <- as.numeric(readline(prompt = "Skewness: "))
    
    # Create a data frame for the input
    user_input <- data.frame(
      variance = variance,
      skewness = skewness
    )
    return(user_input)
  }
  
  # Predict using user input
  cat("Provide input values for prediction:\n")
  user_input <- get_user_input()
  prediction <- predict(svm_model, user_input)
  cat("Predicted Class:", as.character(prediction), "\n")
  
  # Plotting the decision boundary with contours
  x_range <- seq(min(train_data$variance) - 1, max(train_data$variance) + 1, length.out = 200)
  y_range <- seq(min(train_data$skewness) - 1, max(train_data$skewness) + 1, length.out = 200)
  grid <- expand.grid(variance = x_range, skewness = y_range)
  grid_predictions <- predict(svm_model, grid)
  grid$class <- grid_predictions
  
  print(ggplot() +
          geom_tile(data = grid, aes(x = variance, y = skewness, fill = class), alpha = 0.3) +
          geom_contour(data = grid, aes(x = variance, y = skewness, z = as.numeric(class)), color = "black", binwidth = 0.5) +
          geom_point(data = train_data, aes(x = variance, y = skewness, color = class), size = 2) +
          scale_fill_manual(values = c("lightblue", "pink")) +
          scale_color_manual(values = c("blue", "red")) +
          labs(
            title = "SVM Decision Boundary",
            x = "Variance",
            y = "Skewness",
            fill = "Class",
            color = "Class"
          ) +
          theme_minimal())
}

# Main Menu
while (TRUE) {
  cat("Menu:\n")
  cat("1. Decision Tree (Lens Classification)\n")
  cat("2. SVM Classifier (Currency Forgery Classification)\n")
  cat("3. Exit\n")
  
  choice <- as.numeric(readline(prompt = "Enter your choice (1/2/3): "))
  
  if (choice == 1) {
    decision_tree_model()
  } else if (choice == 2) {
    svm_classifier()
  } else if (choice == 3) {
    cat("Exiting the program\n")
    break
  } else {
    cat("Invalid choice. Please enter 1, 2, or 3.\n")
  }
}
