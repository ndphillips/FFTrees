# Data processing

# load all data sets
load("C:/Users/jelen/R_WD/FFTrees/data/blood.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/breastcancer.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/car.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/contraceptive.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/creditapproval.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/fertility.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/forestfires.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/heartcost.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/heartdisease.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/irisv.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/mushrooms.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/sonar.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/titanic.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/voting.RData")
load("C:/Users/jelen/R_WD/FFTrees/data/wine.RData")

# List of dataset names
dataset_names <- c("blood", "breastcancer", "car", "contraceptive",
                   "creditapproval", "fertility", "forestfires", "heartdisease",
                   "iris.v", "mushrooms", "sonar", "titanic", "voting", "wine")



# replace ambiguous missing data with NA

## create function
find_NAs <- function(data) {
  data[] <- lapply(data, function(x) {
    if(is.factor(x)) {
      x <- as.character(x)  # Convert factor to character
      x[x == "?"] <- NA
      x <- as.factor(x)  # Convert character back to factor
    } else if (is.character(x)) {
      x[x == "?"] <- NA
    }
    return(x)
  })
  return(data)
}

# simplify binary factor predictors to logical predictor

## create function for y,n
convert_binary_to_logical_1 <- function(data) {
  data[] <- lapply(data, function(x) {
    if (is.factor(x) && length(levels(x)) == 2 && all(levels(x) %in% c("y", "n"))) {
      x <- as.character(x)
      x[x == "y"] <- TRUE
      x[x == "n"] <- FALSE
      x <- as.logical(x)
    }
    return(x)
  })
  return(data)
}


## create function for f,t
convert_binary_to_logical_2 <- function(data) {
  data[] <- lapply(data, function(x) {
    if (is.factor(x) && length(levels(x)) == 2 && all(levels(x) %in% c("f", "t"))) {
      x <- as.character(x)
      x[x == "f"] <- TRUE
      x[x == "t"] <- FALSE
      x <- as.logical(x)
    }
    return(x)
  })
  return(data)
}


# Apply functions to each dataset
for (name in dataset_names) {
  # Get the dataset
  data <- get(name)

  # Apply functions
  data <- find_NAs(data)
  data <- convert_binary_to_logical_1(data)
  data <- convert_binary_to_logical_2(data)

  # Assign back to the original variable
  assign(name, data)
}


# # Overwrite the original datasets and save them to .Rdata files
# for (i in seq_along(dataset_names)) {
#   save(list = dataset_names[i], file = paste0("C:/Users/jelen/R_WD/FFTrees/data/", dataset_names[i], ".RData"))
# }



