This patch fixes bugs to ensure that values of 0 for skew are handled correctly.
The patch also fixes a bug in replext_ts2_c1.1() to ensure that the p-value
comparison for ANOVA is correct.

The patch also incorporates the 'mmints' R package in order to reduce repetition
in 'shiny' applications by using modules.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

