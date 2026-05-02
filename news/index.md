# Changelog

## FFTrees 2.1.0

CRAN release: 2025-09-02

**FFTrees** version 2.1.0 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2025-09-03\].
This version reduces non-essential functionality, increases robustness,
and fixes some bugs.

### Breaking changes

- Deprecated all applications of non-FFT competitive algorithms (i.e.,
  LR, SVM, RF) in FFT creation. From now on, consider using dedicated
  packages (such as **parsnip** or **tidymodels**) to create and
  evaluate these models.

### Major changes

- Added support for handling NA values.

### Minor changes

Plotting:

- [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  now labels row of 2x2 confusion matrix as “Prediction” when using
  “test” data.
- [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  now has a `truth.labels` argument which, if set, distinguishes labels
  of true (signal vs. noise) cases from decision outcomes.
- [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  now has a `grayscale` argument which, if TRUE, creates a gray scale
  plot.

Decision costs:

- Increased value of `cost_cues_default` from 0 to 1, so that default
  cue costs correspond to `mcu`.

### Details

- Fixed bug for missing plot title.
- Added `@aliases FFTrees-package` to documentation of main
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  function.
- Removed redundant `data_old` folder.

The current development version of **FFTrees** is available at
<https://github.com/ndphillips/FFTrees>.

------------------------------------------------------------------------

## FFTrees 2.0.0

CRAN release: 2023-06-05

**FFTrees** version 2.0.0 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2023-06-06\].
This version adds functionality, improves consistency, and increases
robustness.

Changes since last release:

### Major changes

- Enabled conversions between tree definition formats and manipulating
  FFT definitions:
  - tree definition and conversion functions: `get_fft_df`,
    `read_fft_df`, `write_fft_df`, `add_fft_df`
  - tree trimming functions: `add_nodes`, `drop_nodes`, `edit_nodes`,
    `flip_exits`, `reorder_nodes`, `select_nodes`

&nbsp;

- Growing FFTs:
  - enabled `stopping.rule = "statdelta"`
  - fixed a bug in
    [`fftrees_grow_fan()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_grow_fan.md)
    that prevented `ifan` algorithm from stopping when finding a perfect
    FFT (given the current `goal.chase` parameter)

&nbsp;

- Handling missing inputs (`NA` values) in data:
  - `NA` values in categorical (i.e., character/factor/logical)
    predictors are treated as `<NA>` factor levels  
  - `NA` values in numeric predictors are either *ignored* (by default)
    or *imputed* (as the mean of the corresponding predictor) when
    creating and using FFTs to decide/predict (if possible)
  - `NA` values in the criterion variable are yet to be dealt with

### Minor changes

- Added utility functions (and corresponding verification functions):
  - [`get_best_tree()`](https://www.nathanieldphillips.co/FFTrees/reference/get_best_tree.md)
    retrieves the ID of the best tree in an `FFTrees` object (given
    `goal`)
  - [`get_exit_type()`](https://www.nathanieldphillips.co/FFTrees/reference/get_exit_type.md)
    converts a vector of exit descriptions into FFT exits (given
    `exit_types`)
  - [`get_fft_df()`](https://www.nathanieldphillips.co/FFTrees/reference/get_fft_df.md)
    retrieves the tree definitions of an `FFTrees` object
- Added cost information when printing FFTs (with
  [`print.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)).
- Improved user feedback (by making `quiet` a list with four options).
- Increased vocabulary for interpreting verbal FFT descriptions (using
  `my.tree`).
- Improved documentation of included data (e.g., in
  [`FFTrees.guide()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.guide.md)).

### Details

- Added global constants and utility functions.
- Added progress bar of **cli** package (removing dependency on
  **progress**).
- Added `exit_types` as global constant.
- Improved data cleaning (consistent for training and test data).
- Revised documentation, vignettes, and tests.

------------------------------------------------------------------------

## FFTrees 1.9

### 1.9.0

**FFTrees** version 1.9.0 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2023-02-08\].
Apart from adding functionality and fixing minor bugs, this version
improves consistency, robustness, and transparency.

Changes since last release:

#### Major changes

- Enabled optimizing for a user-defined `my.goal` on cue and tree levels
  (as defined by `my.goal.fun`).
- Enabled optimizing for `dprime` on cue and tree levels (by using
  `"dprime"` as `goal.threshold`, `goal.chase`, or `goal` values).
- Increased vocabulary for interpreting verbal FFT descriptions (using
  `my.tree`).
- Improved
  [`summary.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md)
  function:
  - Included current goal and cost values (if `"cost"` occurs in goals).
  - Included criterion base rates (in performance statistics on train
    and test data).

#### Minor changes

- Included `dprime` values in cue level statistics (`x$cues$thresholds`
  and `x$cues$stats`).
- Included `dprime` values in competition statistics
  (`x$competition$train` and `x$competition$test`).
- Improved user feedback on combinations of goal and cost values.
- Prepared for modular tree translation and editing functions
  (`util_gfft.R`).
- Prepared for global tree notation separator (`fft_node_sep`).
- Added decision outcome and cue costs to `asif_results` (in
  [`fftrees_grow_fan()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_grow_fan.md)).

#### Details

- Added verification functions (for checking integrity of objects and
  validity of inputs).
- Deprecated the `rounding` argument of
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).
- Re-arranged arguments of key functions
  ([`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  and
  [`fftrees_create()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_create.md))
  by functionality.
- Re-arranged and cleaned code (in main and helper functions).
- Re-defined local constants as global constants (in `util_const.R`).
- Revised status badges in `README`.
- Tweaked plotting parameters.
- Fixed bugs and revised vignettes.

------------------------------------------------------------------------

## FFTrees 1.8

### 1.8.0

**FFTrees** version 1.8.0 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2023-01-06\].
This version mostly extends and improves existing functionality.

Changes since last release:

#### Major changes

- Enabled manually defining FFTs with `tree.definitions` or using FFTs
  of `object` in
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).
- Enabled setting `goal = 'dprime'` to select FFTs in
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).
- Added and improved user feedback (when `quiet = FALSE`).

#### Minor changes

- Plotting FFTs with
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md):
  - Show `n.per.icon` legend when `what = 'icontree'`.
  - Bug fix: Removed clipping of titles and labels.
  - Tweaked spacing parameters.

&nbsp;

- Trimmed white space from elements in tree definitions (in
  `fftrees_apply.R`).

- Added check that cues occur in current data (in
  `verify_all_cues_in_data()`).

#### Details

- Removed `anova` from **stats** imports.  
- Replaced `expect_is()` by more precise **testthat** inheritance
  functions.
- Replaced **crayon** package by **cli** package.
- Revised documentation and vignettes.
- Fixed bugs and revised code to increase robustness.

------------------------------------------------------------------------

## FFTrees 1.7

### 1.7.5

**FFTrees** version 1.7.5 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2022-09-15\].
This version contains mostly bug fixes, but also improves and revises
existing functionality.

Changes since last release:

#### Major changes

- Added distinctions between FFTs that “decide” vs. “predict” by using
  corresponding labels in plots and verbal descriptions.

- Improved plotting and printing FFTs (with
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  and
  [`print.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)):

  - Added new plotting options (e.g., `what = 'all'` vs. `what = 'tree'`
    and `what = 'icontree'`).
  - Added distinction in header of icon guide between FFTs that “decide”
    (for training data) vs. “predict” (for test data).
  - Enabled applying a tree to new test data when providing a data frame
    as `data`.
  - Enabled passing some graphical parameters (e.g., `col`, `font`,
    `adj`) to text of panel titles.
  - Return an invisible `FFTrees` object `x` (to allow re-assigning to
    global `x` when using new test data).

#### Minor changes

- Added `wacc` to measures computed for competing algorithms.

- Plotting with
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md):

  - Adjusted space for title to width of `main` argument.
  - Deprecated the `stats` argument.
  - Moved utility functions to `helper_plot.R`.

#### Details

- Revised documentation and vignettes.
- Renamed internal functions and variables.

### 1.7.0

**FFTrees** version 1.7.0 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2022-08-31\].
This version contains numerous bug fixes and improves or revises
existing functionality.

Changes since last release:

#### Major changes

- Improved functionality of
  [`print.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md):
  - Added `data` argument to print an FFT’s training performance (by
    default) or prediction performance (when test data is available).
  - Enabled setting `tree` to `"best.train"` or `"best.test"` (as when
    plotting FFTs).  
  - Reporting `bacc` or `wacc` in *Accuracy* section (and `sens.w`, if
    deviating from the default of 0.50).
  - Improved readability of 2x2 confusion table (by right-justifying
    digits).
  - Moved cost information from *Accuracy* to *Speed, Frugality, and
    Cost* section.

&nbsp;

- Fixed bugs and improved functionality of
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md):
  - Improved plot for `what = 'ROC'` analogous to `what = 'cues'`.
  - Reporting `bacc` or `wacc` in *Accuracy* section (and `sens.w`
    value, if deviating from the default of 0.50).
  - Fixed bug to re-enable setting `tree` to `"best.train"` or
    `"best.test"`.
  - Fixed bug to show correct point labels in ROC curve panel.

&nbsp;

- Fixed bugs and improved functionality of
  [`showcues()`](https://www.nathanieldphillips.co/FFTrees/reference/showcues.md):
  - Using current goal of object `x` as cue ranking criterion (rather
    than always using `wacc`).
  - Subtitle now shows `sens.w` value when `goal == 'wacc'`.
  - Cue legend now accommodates 0 \< `top` \< 10.
  - Removed redundant `data` argument (as `FFTrees` objects only contain
    cue training data).
  - Added `alt.goal` argument (to allow ranking cue accuracies by
    alternative goals).
  - Added `quiet` argument (to hide feedback messages).
  - Added subtitle (to signal current cue accuracy ranking criterion).

&nbsp;

- Improved version of
  [`summary.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/summary.FFTrees.md):
  - Print tree performance summary and goal information (on the
    console).
  - Return tree `definitions` and `stats` (as a list).

&nbsp;

- Fixed a bug that forced reversals of final exits in the final node
  when manually creating FFTs with `my.tree` or
  [`fftrees_wordstofftrees()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md).

#### Minor changes

- Changed tree statistics for test data from data frames to tibbles.
- Improved feedback on missing decision labels when creating FFTs from
  descriptions with `my.tree` or
  [`fftrees_wordstofftrees()`](https://www.nathanieldphillips.co/FFTrees/reference/fftrees_wordstofftrees.md).
- Deprecated the `store.data` argument of
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).

#### Details

- Changed primary package maintainer to Hansjoerg Neth, but Nathaniel
  Phillips is still on board.
- Revised text, examples, and links in vignettes.
- Reduced clutter by recycling code and combining files.
- Cleanup of code and documentation.

------------------------------------------------------------------------

## FFTrees 1.6

**FFTrees** version 1.6.6 was released [on
CRAN](https://CRAN.R-project.org/package=FFTrees) \[on 2022-07-18\].

Changes since last release:

### 1.6.6

- Fixed bug causing
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  to not display plots properly.

### 1.6.5

- Cleanup to get package back on CRAN after failed submission on July
  17, 2022.

### 1.6.4

- Cleanup to get package back on CRAN after failed submission on July
  16, 2022.

### 1.6.3

- Additional cleanup to get package back on CRAN after failed submission
  on July 12, 2022.

### 1.6.2

- Internal cleanup to get package back on CRAN.

### 1.6.1

- [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  no longer saves graphic params changed in
  [`par()`](https://rdrr.io/r/graphics/par.html).
- `plot.FFTRrees()`: When `test = 'best.test'` and no test data are
  provided, the information text is no returned with
  [`message()`](https://rdrr.io/r/base/message.html) rather than
  [`print()`](https://rdrr.io/r/base/print.html).
- Deprecation notes of
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  are now returned as warnings, not messages.

------------------------------------------------------------------------

## FFTrees 1.5

### 1.5.7

- Officially deprecated the `"max"` and `"zigzag"` algorithms.
- Minor cleanup throughout.

### 1.5.0

- Fixed warnings for CRAN submission.
- `FFTrees` objects now have a nicer internal structure.
- Added tests throughout (finally).
- Extensive code cleanup, which should speed things up.
- New print method includes an ASCII confusion matrix.

------------------------------------------------------------------------

## FFTrees 1.4

### 1.4.0

- Big under the hood changes to make code more efficient (and prepare
  for C++). Code should be ~50% faster.
- Many inputs such as `cost.cues` and `cost.outcomes` are now specified
  as named lists to avoid confusion.
- New cost outputs separate costs from cues, outcomes, and total costs.
- Changes to input defaults for `goal` and `goal.chase`.

------------------------------------------------------------------------

## FFTrees 1.3

### 1.3.6

- Bug fixes.

### 1.3.5

- Bug fixes.

### 1.3.4

- Added class probability predictions with
  `predict.FFTrees(type = "prob")`.

- Updated
  [`print.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
  to display FFT [\#1](https://github.com/ndphillips/FFTrees/issues/1)
  ‘in words’ (from the `inwords(x)` function).

### 1.3.3

- Added `show.X` arguments to
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  that allow you to selectively turn on or turn off elements when
  plotting an `FFTrees` object.

- Added `label.tree`, `label.performance` arguments to
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  that allow you to specify plot (sub) labels.

- Bug fixes:

  - Issues when passing an existing `FFTrees` object to a new call to
    [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md).

### 1.3.0

- Many additional vignettes (e.g.; *Accuracy Statistics and Heart
  Disease Tutorial*) and updates to existing vignettes.

- Added `cost.outcomes` and `cost.cues` to allow the user to specify
  specify the cost of outcomes and cues. Also added a `cost` statistic
  throughout outputs.

- Added
  [`inwords()`](https://www.nathanieldphillips.co/FFTrees/reference/inwords.md),
  a function that converts an `FFTrees` object to words.

- Added `my.tree` argument to
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  that allows the user to specify an FFT verbally.  
  E.g.,
  `my.tree = 'If age > 30, predict True. If sex = {m}, predict False. Otherwise, predict True'`.

- Added positive predictive value `ppv`, negative predictive value `npv`
  and balanced predictive value `bpv`, as primary accuracy statistics
  throughout.

- Added support for two FFT construction algorithms from Martignon et
  al. (2008): `"zigzag"` and `"max"`. The algorithms are contained in
  the file `heuristic_algorithm.R` and can be implemented in
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  as arguments to `algorithm`.

------------------------------------------------------------------------

## FFTrees 1.2

### 1.2.3

- Added `sens.w` argument to allow differential weighting of
  sensitivities and specificities when selecting and applying trees.

- Fixed bug in calculating importance weightings from `FFForest()`
  outputs.

### 1.2.0

- Changed wording of statistics throughout package: `hr` (*hit rate*)
  and `far` (*false alarm rate*) (based on the classification frequency
  values `hi` and `fa`), are now `sens` for *sensitivity* and `spec` for
  *specificity* (1 $`-`$`far`), respectively.

- The `rank.method` argument is now deprecated. Use `algorithm` instead.

- Added a `stats` argument to
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md).
  When `stats = FALSE`, only the tree will be plotted without reference
  to any statistical output.

- Grouped all competitive algorithm results (regression, cart, random
  forests, support vector machines) to the new `x.fft$comp` slot rather
  than a separate first level list for each algorithm. Also replaced
  separate algorithm wrappers with one general `comp_pred()` wrapper
  function.

- Added `FFForest()`, a function for creating forests of FFTs, and
  `plot.FFForest()`, for visualizing forests of FFTs. (This function is
  experimental and still in development.)

- Added random forests and support vector machines for comparison in
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  using the **randomForest** and **e1071** packages.

- Changed logistic regression algorithm from the default
  [`glm()`](https://rdrr.io/r/stats/glm.html) version to `glmnet()` for
  a regularized version.

- [`predict.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/predict.FFTrees.md)
  now returns a vector of predictions for a specific tree rather than
  creating an entirely new `FFTrees` object.

- You can now plot cue accuracies within the
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  function by including the `plot.FFTrees(what = 'cues')` argument.
  (This replaces the former
  [`showcues()`](https://www.nathanieldphillips.co/FFTrees/reference/showcues.md)
  function.)

- Many cosmetic changes to
  [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
  (e.g.; gray levels, more distinct classification balls). You can also
  control whether the results from competing algorithms are displayed or
  not with the `comp` argument.

- Bug-fixes:

  - Fixed a bug where levels with no classifications are not plotted
    correctly.

------------------------------------------------------------------------

## FFTrees 1.1

### 1.1.7

- Trees can now use the same cue multiple times within a tree. To do
  this, set `rank.method = "c"` and `repeat.cues = TRUE`.

- Bug-fixes:

  - You can (and should!) now have a column of NAs for the criterion in
    test datasets to represent data where the criterion is unknown.
  - [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
    now supports a single predictor (e.g.; `formula = diagnosis ~ age`)
    which previously did not work.

### 1.1.6

- Streamlined code to improve cohesion between functions. This may cause
  issues with `FFTrees` objects created with earlier versions of the
  package. They will need to be re-created.

- Updated, clearer
  [`print.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/print.FFTrees.md)
  method to see important info about an `FFTrees` object in matrix
  format.

- Training and testing statistics are now in separate objects (e.g.,
  `data$train` vs. `data$test`) to avoid confusion.

- Bug-fixes:

  - [`predict.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/predict.FFTrees.md)
    now works much better by passing a new dataset (`data.test`) as a
    test dataset for an existing `FFTrees` object.

### 1.1.5

- Bug-fixes:
  - Plotting parameters `mar` and `layout` are now reset after running
    [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)

### 1.1.4

- Bug-fixes:
  - Plotting no longer fails when there is only one branch in the tree.
  - Changed `which.tree` argument in
    [`plot.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/plot.FFTrees.md)
    to `tree` to conform to blog posts.
  - [`predict.FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/predict.FFTrees.md)
    now works better with `tibble` inputs.
- Changed the `fft` label to `FFTrees` throughout the package to avoid
  confusion with fast fourier transform. Thus, the main tree building
  function is now
  [`FFTrees()`](https://www.nathanieldphillips.co/FFTrees/reference/FFTrees.md)
  and the new tree object class is `FFTrees`.

------------------------------------------------------------------------

\[File `NEWS.md` last updated on 2025-09-03.\]
