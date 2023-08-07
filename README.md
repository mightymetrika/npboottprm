
<!-- README.md is generated from README.Rmd. Please edit that file -->

# npboottprm

<!-- badges: start -->
<!-- badges: end -->

The goal of npboottprm is to provide a robust tool for conducting
nonparametric bootstrap tests with pooled resampling. These tests are
ideal for small sample sizes and include the independent t-test, paired
t-test, and F-test. The package employs methods presented in Dwivedi,
Mallawaarachchi, and Alvarado (2017).

## Installation

You can install the released version of npboottprm from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("npboottprm")
```

To install the development version of npboottprm from GitHub, use the
[devtools](https://devtools.r-lib.org/) package:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/npboottprm")
```

## Nonparametric bootstrap t-test

The following example demonstrates how to use the nonparboot() function
to conduct an independent t-test.

``` r
library(npboottprm)

# Use the simulated data included in the package
print(data_t)
#>             x    grp
#> 1  -0.4545644 Group1
#> 2  -1.0203803 Group1
#> 3  -0.8381422 Group1
#> 4   1.2075650 Group1
#> 5   0.6658781 Group1
#> 6   1.7392430 Group2
#> 7  -0.5869082 Group2
#> 8  -0.4396707 Group2
#> 9   0.1153781 Group2
#> 10  2.2982103 Group2

# Run the test
res_t <- nonparboot(data = data_t,
                    x = "x",
                    grp = "grp",
                    nboot = 1000,
                    test = "t",
                    conf.level = 0.95,
                    seed = 183)

# Print the results, excluding the bootstrap distributions
print(res_t[!names(res_t) %in%
              c("bootstrap.stat.dist", "bootstrap.effect.dist")])
#> $p.value
#> [1] 0.341
#> 
#> $orig.stat
#>          t 
#> -0.9742938 
#> 
#> $ci.stat
#>      2.5%     97.5% 
#> -2.421260  2.060161 
#> 
#> $effect.size
#> [1] -0.7131793
#> 
#> $ci.effect.size
#>      2.5%     97.5% 
#> -1.307315  1.243749
```

## Nonparametric bootstrap paired t-test

The following example demonstrates how to use the nonparboot() function
to conduct a paired t-test.

``` r
# Use the simulated data included in the package
print(data_pt)
#>              x          y
#> 1  -0.04210576  0.5743731
#> 2  -0.78606271  0.4955500
#> 3   0.73510435  1.0370887
#> 4  -0.59760139  1.3066557
#> 5   0.73802810  0.6191670
#> 6  -1.84689646 -0.2261067
#> 7  -0.83164309  0.6387438
#> 8  -1.33994927  2.3328887
#> 9   0.05511817  0.5356024
#> 10 -0.38908692  2.0891858

# Run the test
res_pt <- nonparboot(data = data_pt,
                     x = "x",
                     y = "y",
                     nboot = 1000,
                     test = "pt",
                     conf.level = 0.95,
                     seed = 166)

# Print the results, excluding the bootstrap distributions
print(res_pt[!names(res_pt) %in%
               c("bootstrap.stat.dist", "bootstrap.effect.dist")])
#> $p.value
#> [1] 0.005
#> 
#> $orig.stat
#>         t 
#> -3.816044 
#> 
#> $ci.stat
#>      2.5%     97.5% 
#> -2.331819  2.353130 
#> 
#> $effect.size
#> [1] -1.370824
#> 
#> $ci.effect.size
#>       2.5%      97.5% 
#> -0.9424713  0.8814061
```

## Nonparametric bootstrap F-test

The following example demonstrates how to use the nonparboot() function
to conduct an F-test.

``` r
# Use the simulated data included in the package
print(data_f)
#>             x    grp
#> 1  -1.2659165 Group1
#> 2  -0.2102170 Group1
#> 3  -0.2203267 Group1
#> 4  -0.2113811 Group1
#> 5  -1.1601362 Group1
#> 6   1.3723773 Group2
#> 7   0.3764606 Group2
#> 8   0.4393153 Group2
#> 9   0.6279528 Group2
#> 10  0.9103877 Group2
#> 11  2.5440636 Group3
#> 12  1.0701797 Group3
#> 13  0.8459235 Group3
#> 14  2.2931195 Group3
#> 15  2.0675429 Group3

# Run the test
res_f <- nonparboot(data = data_f,
                    x = "x",
                    grp = "grp",
                    nboot = 1000,
                    test = "F",
                    conf.level = 0.95,
                    seed = 397)

# Print the results, excluding the bootstrap distributions
print(res_f[!names(res_f) %in%
              c("bootstrap.stat.dist", "bootstrap.effect.dist")])
#> $p.value
#> [1] 0
#> 
#> $orig.stat
#> [1] 20.46528
#> 
#> $ci.stat
#>       2.5%      97.5% 
#> 0.02517111 4.86060520 
#> 
#> $effect.size
#> [1] 0.7732878
#> 
#> $ci.effect.size
#>        2.5%       97.5% 
#> 0.004177655 0.447544364
```

Please note that the examples provided here use simulated data included
in the package. When using this package with your own data, replace
data_t, data_pt, and data_f with your own data frames, and adjust the x,
y, and grp parameters as needed.

## References

Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). “Analysis of small
sample size studies using nonparametric bootstrap test with pooled
resampling method.” Statistics in Medicine, 36 (14), 2187-2205.
