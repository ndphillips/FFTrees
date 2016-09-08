# FFTrees
A package to create and visualize fast and frugal decision trees (FFTrees)


### Package updates

1.1.6

- Streamlined code to improve cohesion between functions.
- Training and testing statistics are now always in seperate objects.

- Bug-fixes
    - `predict.FFTrees()` now works by passing an existing FFTrees object and a new dataset back to FFTrees(). It will use all existing trees in the existing FFTrees object and apply them to the new dataset as a new test dataset.

1.1.5

- Bug-fixes
    - Plotting parameters `mar` and `layout` are now reset after running `plot.FFTrees()`

1.1.4

- Bug-fixes
    - Plotting no longer fails when there is only one branch in the tree.
    - Changed `which.tree` argument in `plot.FFTrees()` to `tree` to conform to blog posts.
    - `predict.FFTrees()` now works better with `tibble` inputs.
    - 
    
- Changed the `fft` label to `FFTrees` throughout the package to avoid confusion with fast fourier transform. Thus, the main tree building function is now `FFTrees()` and the new tree object class is `FFTrees`


