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



# write function to replace a vector of strings by another vector of strings in a complete dataframe:

converting_data <- function(data, vec1, vec2, convert_to_logical = TRUE, convert_back_to_factor = TRUE, strict_logical_conversion = FALSE) {

  # verifying whether vectors have the same length
  testthat::expect_true(length(vec1) == length(vec2), "Vectors need to be of same lengths for the function to work.")

  # Check which columns were factors
  was_factor <- sapply(data, is.factor) # returns logical simplifies vector

  # Change factor into character variables
  data[was_factor] <- lapply(data[was_factor], as.character) # returns same data structure (no simplification)

  # define that function should be applied to every column
  for (i in seq_along(data)){

    ## Check the unique values in the column (ignoring NA)
    unique_values <- unique(na.omit(data[[i]]))

    ## check if strict_logical_conversion is TRUE and if the column has only the values to be replaced
    if (strict_logical_conversion && !all(unique_values %in% vec1)) {

      ### If not (strict_logical_conversion is TRUE but the column has other values too), skip the current column and proceed with the next
      next
    }
      ### if both is TRUE (or if strict_logical_conversion is FALSE):

    # apply replacement-function to all columns and change content in (vec1) to content in (vec2)
    for (j in seq_along(vec1)) {
      data[[i]][data[[i]] == vec1[j]] <- vec2[j]
    }

    # change type of variables back

    ## if only TRUE/FALSE AND convert_to_logical is TRUE, then covert into logical
    ### therefore check if the column contains only "TRUE" and "FALSE" (allowing for NAs to still be there) -> !when applying, first change unknowns into NAs and than factors into logical variables for both to work!
    if (convert_to_logical && all(na.omit(data[[i]]) %in% c("TRUE", "FALSE"))) {

      ### then Convert the column to logical
      data[[i]] <- as.logical(data[[i]])

      ## if it was a factor AND convert_back_to_factor is TRUE change back
    } else if (convert_back_to_factor && was_factor[i]) {
      data[[i]] <- as.factor(data[[i]])
    }

  }

  return(data)
}


# Apply function with necessary inputs to all datasets


# Apply functions to each dataset
for (name in dataset_names) {

  # Get the dataset
  dataframe <- get(name)


  # Apply the converting_data function to the dataframe to replace "?" with NAs
  dataframe <- converting_data(dataframe, c("?"), c(NA), strict_logical_conversion = FALSE)

  # Apply the converting_data function to replace "y" and "n" with TRUE and FALSE
  dataframe <- converting_data(dataframe, c("y", "n"), c(TRUE, FALSE), strict_logical_conversion = TRUE)

  # Apply the converting_data function again to replace "t" and "f" with TRUE and FALSE
  dataframe <- converting_data(dataframe, c("t", "f"), c(TRUE, FALSE), strict_logical_conversion = TRUE)


  # Apply the converting_data function again to replace "N" and "O" with TRUE and FALSE
  dataframe <- converting_data(dataframe, c("N", "O"), c(TRUE, FALSE), strict_logical_conversion = TRUE)


  # Assign back to the original variable
  assign(name, dataframe, envir = .GlobalEnv)
}


# Overwrite the original datasets and save them to .Rdata files
for (name in dataset_names) {
  save(list = name, file = paste0("C:/Users/jelen/R_WD/FFTrees/data/", name, ".RData"))
}



