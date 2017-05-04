[![Build Status](https://travis-ci.org/ndphillips/FFTrees.svg?branch=master)](https://travis-ci.org/ndphillips/FFTrees)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FFTrees)](https://CRAN.R-project.org/package=FFTrees)
[![Rdoc](http://www.rdocumentation.org/badges/version/FFTrees)](http://www.rdocumentation.org/packages/FFTrees)
[![Downloads](http://cranlogs.r-pkg.org/badges/FFTrees?color=brightgreen)](http://www.r-pkg.org/pkg/FFTrees)

# FFTrees
An R package to create and visualize Fast and Frugal decision trees (FFTrees) like this one below:

```R
# Create the trees
titanic.fft <- FFTrees(formula = survived ~., 
                       data = titanic)
                       
# Plot the best tree
plot(titanic.fft,
     main = "Surviving the Titanic", 
     decision.names = c("Died", "Survived"))
```

![](https://dl.dropboxusercontent.com/u/7618380/titanicfft.png)



### Package updates

1.2.3

- Added `sens.w` argument to allow differential weighting of sensitivities and specificities when selecting and applying trees.

- Fixed but in calculating importance weightings from `FFForest()` outputs.

1.2.0

- Changed wording of statistics throughout package. `hr` (hit rate) and `far` (false alarm rate) are now `sens` for sensitivity, and `spec` for specificity (1 - false alarm rate)

- The `rank.method` argument is now depricated. Use `algorithm` instead.

- Added `stats` argument to `plot.FFTrees()`. When `stats = FALSE`, only the tree will be plotted without reference to any statistical output.

- Grouped all competitive algorithm results (regression, cart, random forests, support vector machines) to the new `x.fft$comp` slot rather than a separate first level list for each algorithm. Also replaced separate algorithm wrappers with one general `comp.pred()` wrapper function.

- Added `FFForest()`, a function for creating forests of ffts, and `plot.FFForest()`, for visualizing forests of ffts. This function is very much still in development.

- Added random forests and support vector machines for comparison in `FFTrees()` using the `randomForest` and `e1071` packages.

- Changed logistic regression algorithm from the default `glm()` version to `glmnet()` for a regularized version.

- `predict.FFTrees()` now returns a vector of predictions for a specific tree rather than creating an entirely new FFTrees object.

- You can now plot cue accuracies within the `plot.FFTrees()` function by including the `plot.FFTrees(what = 'cues')` argument. This replaces the former `showcues()` function.

- Many cosmetic changes to `plot.FFTrees()` (e.g.; gray levels, more distinct classification balls). You can also control whether the results from competing algorithms are displayed or not with the `comp` argument.

- Bug-fixes
    - Fixed a bug where levels with no classifications are not plotted correctly.

1.1.7

- Trees can now use the same cue multiple times within a tree. To do this, set `rank.method = "c"` and `repeat.cues = TRUE`.

- Bug-fixes
   - You can (and should!) now have a column of NAs for the criterion in test datasets to represent data where the criterion is unknown.
   - `FFTrees()` now supports a single predictor (e.g.; `formula = diagnosis ~ age`) which previously did not work.

1.1.6

- Streamlined code to improve cohesion between functions. This may cause issues with FFTrees objects created with earlier versions of the package. They will need to be re-created.
- Updated, clearer `print.FFTrees()` method to see important info about an FFTrees object in matrix format.
- Training and testing statistics are now always in seperate objects (e.g.; `data$train`, `data$test`) to avoid confusion.

- Bug-fixes
    - `predict.FFTrees()` now works much better by passing a new dataset (`data.test`) as a test dataset for an existing FFTrees object.

1.1.5

- Bug-fixes
    - Plotting parameters `mar` and `layout` are now reset after running `plot.FFTrees()`

1.1.4

- Bug-fixes
    - Plotting no longer fails when there is only one branch in the tree.
    - Changed `which.tree` argument in `plot.FFTrees()` to `tree` to conform to blog posts.
    - `predict.FFTrees()` now works better with `tibble` inputs.
    
- Changed the `fft` label to `FFTrees` throughout the package to avoid confusion with fast fourier transform. Thus, the main tree building function is now `FFTrees()` and the new tree object class is `FFTrees`


