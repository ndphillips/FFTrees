## Building function that randomly converts XX% of one row (looping over several columns) of the data in a dataframe to other values (mostly NAs).  ----


# IMPORTANT NOTE: Up until now I did not yet include counting NAs that are already in the columns. In my view we could do without, if necessary, have to add!



# requirements of the function:
## Inputs should be: 1. Data frame
##                   2. Variable or vector of variables where NAs should be added (by name or number of position)
##                   3. Percentage of NAs that should be added to that variable (sample 1:nrow, replace by)
##                   4. What the replacing input is (Default should be NA, but changeable)
##                   5 additionally: should be possible to specify that certain levels of a factor/character variable should have more (a specific percentage) NAs than other levels of the variable (which should have a specified lower percentage).



# Way the function works:

## For data insert data frame
## For cols insert vector of multiple or single columns
## For amount insert vector with percentages or total number of values that you want to replace values in corresponding columns

# optional:
## For replacement insert value that you want inserted (default = NA)
## For levels_amount insert a named list of named lists specifying percentages or total numbers for each category for each factor variable (for example, list('column1'=list('category1'=0.1, 'category2'=0.2)'))
## this will simply override the percentage/number that exists in "amount".

replace_values <- function(data, cols, amount, replacement = NA, levels_amount = NULL) {


  # Verify inputs ----
  testthat::expect_true(is.data.frame(data), info = "Data should be a dataframe.") # check that data is a data frame.
  testthat::expect_true(all(cols %in% names(data)), info = "All column names should be present in the data.") # check that columns is/are all variables in the data frame.
  testthat::expect_true(length(cols) == length(amount), info = "Number of columns and percentages have to match.") # check if columns and percentages are of the same length.
  testthat::expect_true(all(is.numeric(amount) & (amount >= 0) ), info = "All amounts should be numeric values.") # check that percentages are all numbers between 0 and 1, or higher (than treat as number of to be replaced values.
  testthat::expect_true(is.character(replacement) | is.numeric(replacement) | is.logical(replacement) | is.na(replacement), info = "Replacement value should be of a valid data type (character, nummeric, logical or NA).") # check that replacement is a valid data type.
  testthat::expect_true(is.null(levels_amount) | is.list(levels_amount), info = "levels_amount should be a list") # check that levels_amount is a list if it's not NULL.
  if (!is.null(levels_amount)) {
    testthat::expect_true(all(names(levels_amount) %in% cols), info = "All names in levels_amount have to correspond to a column in the data.") # check if all elements in levels_amount correspond to a column in the data frame.
    for (col in names(levels_amount)) {
      if (is.factor(data[[col]])) {
        testthat::expect_true(all(names(levels_amount[[col]]) %in% levels(data[[col]])), info = "All levels in levels_amount have to be present in the corresponding column of the data.") # check if all sub-elements in each element are valid levels in the corresponding factor variable.

         }

      else if (is.character(data[[col]])) {
        testthat::expect_true(all(names(levels_amount[[col]]) %in% unique(data[[col]])), info = "All levels in levels_amount have to be present in the corresponding column of the data.")

         }

     }

  }


  # loop function over all columns that are inserted in form of a vector and corresponding percentages:
  for (i in seq_along(cols)) {

    col <- cols[i]
    perc <- amount[i]


    # Check if the specific column is a factor and has levels_amount defined:
    if (is.factor(data[[col]]) || is.character(data[[col]]) && !is.null(levels_amount) && col %in% names(levels_amount)) {

      # if all these conditions apply for the column, code is executed:
      # get list of different replacement percentages of levels in column:
      lev_amount <- levels_amount[[col]]

      # loop over levels in current column:
      for (cat in names(lev_amount)) {

        # get replacement percentage for current category:
        replace_perc <- lev_amount[[cat]]

        # get rows in column for current category:
        rows <- which(data[[col]] == cat)

        # Calculate how many values should be replaced in category:
        num_replace <-  ifelse(amount <= 1, round(replace_perc * length(rows), 0), amount)

        # Use sample to replace specified percentage of category with replacement input:
        replace_rows <- sample(rows, size = num_replace[1],  replace = FALSE)
        data[replace_rows, col] <- replacement

      }

    } else {

      # Calculate how many values should be replaced:
      num_values <- nrow(data)
      num_replace <-  ifelse(amount <= 1, round(amount * num_values, 0), amount)

      # Use sample to replace specified percentage with replacement input:
      replace_rows <- sample(1:num_values,size = num_replace[1],  replace = FALSE)
      data[replace_rows, col] <- replacement

    }

  }

  # Output: ----

  return(data) # as data frame.

} # replace_values().
