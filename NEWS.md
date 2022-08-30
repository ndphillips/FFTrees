
# FFTrees 1.7.0

**FFTrees** version 1.7.0 was released [on CRAN](https://CRAN.R-project.org/package=FFTrees) [on 2022-08-31]. 
This version contains numerous bug fixes and improves or revises existing functionality. 

<!-- Log of changes: --> 

Changes since last release: 

## Major changes

<!-- print.FFTrees(): --> 

- Improved functionality of `print.FFTrees()`:
    - Added `data` argument to print an FFT's training performance (by default) or prediction performance (when test data is available). 
    - Enabled setting `tree` to `"best.train"` or `"best.test"` (as when plotting FFTs).  
    - Reporting `bacc` or `wacc` in _Accuracy_ section (and `sens.w`, if deviating from the default of 0.50). 
    - Improved readability of 2x2 confusion table (by right-justifying digits). 
    - Moved expected cost information from _Accuracy_ to _Speed, Frugality, and Cost_ section. 

<!-- plot.FFTrees(): --> 

- Fixed bugs and improved functionality of `plot.FFTrees()`: 
    - Improved plot for `what = 'ROC'` analogous to `what = 'cues'`. 
    - Reporting `bacc` or `wacc` in _Accuracy_ section (and `sens.w` value, if deviating from the default of 0.50). 
    - Fixed bug to re-enable setting `tree` to `"best.train"` or `"best.test"`.
    - Fixed bug to show correct point labels in ROC curve panel. 
    
<!-- showcues(): --> 

- Fixed bugs and improved functionality of `showcues()`: 
    - Using current goal of object `x` as cue ranking criterion (rather than always using `wacc`).
    - Subtitle now shows `sens.w` value when `goal == 'wacc'`.
    - Cue legend now accommodates 0 < `top` < 10.
    - Removed redundant `data` argument (as `FFTrees` objects only contain cue training data).
    - Added `alt.goal` argument (to allow ranking cue accuracies by alternative goals).
    - Added `quiet` argument (to hide feedback messages).
    - Added subtitle (to signal current cue accuracy ranking criterion).

<!-- summary.FFTrees(): --> 

- Improved version of `summary.FFTrees()`:
    - Print tree performance summary and goal information (on the console). 
    - Return tree `definitions` and `stats` (as a list). 

<!-- fftrees_wordstofftrees(): --> 

- Fixed a bug that forced reversals of final exits in the final node when manually creating FFTs with `my.tree` or `fftrees_wordstofftrees()`.

<!-- Blank line. --> 


## Minor changes 

- Changed tree statistics for test data from data frames to tibbles.
- Improved feedback on missing decision labels when creating FFTs from descriptions with `my.tree` or `fftrees_wordstofftrees()`. 
- Deprecated `store.data` argument in `FFTrees()`. 

<!-- Blank line. --> 


## Details 

- Changed primary package maintainer to Hansjoerg Neth, but Nathaniel Phillips is still on board.
- Revised text, examples, and links in vignettes.
- Reduced clutter by recycling code and combining files.
- Cleanup of code and documentation.


<!-- Development version: --> 

The current development version is available at <https://github.com/ndphillips/FFTrees>. 


<!-- Previous versions --> 

------ 

# FFTrees 1.6

**FFTrees** version 1.6.6 was released [on CRAN](https://CRAN.R-project.org/package=FFTrees) [on 2022-07-18].

<!-- Log of changes: --> 

Changes since last release: 

## 1.6.6

- Fixed bug causing `plot.FFTrees()` to not display plots properly.

## 1.6.5

- Cleanup to get package back on CRAN after failed submission on July 17, 2022. 

## 1.6.4

- Cleanup to get package back on CRAN after failed submission on July 16, 2022. 

## 1.6.3

- Additional cleanup to get package back on CRAN after failed submission on July 12, 2022. 

## 1.6.2

- Internal cleanup to get package back on CRAN.

## 1.6.1

- `plot.FFTrees()` no longer saves graphic params changed in `par()`. 
- `plot.FFTRrees()`: When `test = 'best.test'` and no test data are provided, the information text is no returned with `message()` rather than `print()`. 
- Deprecation notes in `plot.FFTrees()` are now returned as warnings, not messages. 


------ 

# FFTrees 1.5

## 1.5.7

- Officially deprecated "max" and "zigzag" algorithms. 
- Minor cleanup throughout. 

## 1.5.0

- Fixed warnings for CRAN submission. 
- FFTrees objects have a nicer internal structure.
- Added tests throughout (finally). 
- Extensive code cleanup, which should speed things up.
- New print method includes an ASCII confusion matrix. 

------ 

# FFTrees 1.4

## 1.4.0

- Big under the hood changes to make code more efficient (and prepare for C++). Code should be ~50% faster.
- Many inputs such as `cost.cues` and `cost.outcomes` are now specified as named lists to avoid confusion.
- New cost outputs separate costs from cues, outcomes, and total costs.
- Changes to input defaults for `goal` and `goal.chase`.

------ 

# FFTrees 1.3

### 1.3.6

- Bug fixes.

### 1.3.5

- Bug fixes.

### 1.3.4

- Added class probability predictions with `predict.FFTrees(type = "prob")`. 

- Updated `print.FFTrees()` to display FFT #1 'in words' (from the `inwords(x)` function). 

### 1.3.3

- Added `show.X` arguments to `plot.FFTrees()` that allow you to selectively turn on or turn off elements when plotting an `FFTrees` object.

- Added `label.tree`, `label.performance` arguments to `plot.FFTrees()` that allow you to specify plot (sub) labels. 

- Bug fixes: 
    - Issues when passing an existing `FFTrees` object to a new call to `FFTrees()`.


### 1.3.0

- Many additional vignettes (e.g.; _Accuracy Statistics and Heart Disease Tutorial_) and updates to existing vignettes.

- Added `cost.outcomes` and `cost.cues` to allow the user to specify specify the cost of outcomes and cues. Also added a `cost` statistic throughout outputs.

- Added `inwords()`, a function that converts an `FFTrees` object to words.

- Added `my.tree` argument to `FFTrees()` that allows the user to specify an FFT verbally.  
E.g., `my.tree = 'If age > 30, predict True. If sex = {m}, predict False. Otherwise, predict True'`.

- Added positive predictive value `ppv`, negative predictive value `npv` and balanced predictive value `bpv`, as primary accuracy statistics throughout.

- Added support for two FFT construction algorithms from Martignon et al. (2008): `"zigzag"` and `"max"`. 
The algorithms are contained in the file `heuristic_algorithm.R` and can be implemented in `FFTrees()` as arguments to `algorithm`.


------ 

# FFTrees 1.2

## 1.2.3

- Added `sens.w` argument to allow differential weighting of sensitivities and specificities when selecting and applying trees.

- Fixed bug in calculating importance weightings from `FFForest()` outputs.

## 1.2.0

- Changed wording of statistics throughout package. `hr` (_hit rate_) and `far` (_false alarm rate_) are now `sens` for _sensitivity_, and `spec` for _specificity_ ($1 - $false alarm rate). 

- The `rank.method` argument is now deprecated. Use `algorithm` instead.

- Added `stats` argument to `plot.FFTrees()`. When `stats = FALSE`, only the tree will be plotted without reference to any statistical output.

- Grouped all competitive algorithm results (regression, cart, random forests, support vector machines) to the new `x.fft$comp` slot rather than a separate first level list for each algorithm. Also replaced separate algorithm wrappers with one general `comp.pred()` wrapper function.

- Added `FFForest()`, a function for creating forests of FFTs, and `plot.FFForest()`, for visualizing forests of FFTs. 
This function is very much still in development.

- Added random forests and support vector machines for comparison in `FFTrees()` using the **randomForest** and **e1071** packages.

- Changed logistic regression algorithm from the default `glm()` version to `glmnet()` for a regularized version.

- `predict.FFTrees()` now returns a vector of predictions for a specific tree rather than creating an entirely new `FFTrees` object.

- You can now plot cue accuracies within the `plot.FFTrees()` function by including the `plot.FFTrees(what = 'cues')` argument. 
This replaces the former `showcues()` function.

- Many cosmetic changes to `plot.FFTrees()` (e.g.; gray levels, more distinct classification balls). 
You can also control whether the results from competing algorithms are displayed or not with the `comp` argument.

- Bug-fixes: 
    - Fixed a bug where levels with no classifications are not plotted correctly.

------ 

# FFTrees 1.1

## 1.1.7

- Trees can now use the same cue multiple times within a tree. To do this, set `rank.method = "c"` and `repeat.cues = TRUE`.

- Bug-fixes: 
   - You can (and should!) now have a column of NAs for the criterion in test datasets to represent data where the criterion is unknown.
   - `FFTrees()` now supports a single predictor (e.g.; `formula = diagnosis ~ age`) which previously did not work.

## 1.1.6

- Streamlined code to improve cohesion between functions. This may cause issues with `FFTrees` objects created with earlier versions of the package. They will need to be re-created.
- Updated, clearer `print.FFTrees()` method to see important info about an `FFTrees` object in matrix format.
- Training and testing statistics are now in separate objects (e.g., `data$train` vs. `data$test`) to avoid confusion.

- Bug-fixes:  
    - `predict.FFTrees()` now works much better by passing a new dataset (`data.test`) as a test dataset for an existing `FFTrees` object.

## 1.1.5

- Bug-fixes:  
    - Plotting parameters `mar` and `layout` are now reset after running `plot.FFTrees()`

## 1.1.4

- Bug-fixes:  
    - Plotting no longer fails when there is only one branch in the tree.
    - Changed `which.tree` argument in `plot.FFTrees()` to `tree` to conform to blog posts.
    - `predict.FFTrees()` now works better with `tibble` inputs.
    
- Changed the `fft` label to `FFTrees` throughout the package to avoid confusion with fast fourier transform. 
Thus, the main tree building function is now `FFTrees()` and the new tree object class is `FFTrees`. 

<!-- footer: --> 

------ 

[File `NEWS.md` last updated on 2022-08-31.] 

<!-- eof. -->
