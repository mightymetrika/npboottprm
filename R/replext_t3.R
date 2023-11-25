#' Replicate and Extend Simulation Results for Statistical Power from Table 3 Cell 1.1
#'
#' This function is tailored to replicate and extend the simulation results for assessing
#' statistical power from Table 3 cell block 1.1 in the paper by Dwivedi et al. (2017).
#' It compares two groups with different means but equal variance and optional skewness.
#' The function is a wrapper around `replext_t2_c1.1`, adapted for statistical power analysis.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), representing statistical power.
#'
#' @examples
#' replext_t3_c1.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c1.1 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 1, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 1.2
#'
#' This function is designed to replicate and extend the statistical power analysis
#' from Table 3 cell block 1.2 in the paper by Dwivedi et al. It focuses on
#' scenarios with normal distribution having different means and unequal variances
#' across two groups. It utilizes `replext_t2_c1.1` for its calculations by setting
#' specific means and standard deviations.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), representing the power analysis.
#'
#' @examples
#' replext_t3_c1.2(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c1.2 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 3, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 2.1
#'
#' This function is geared towards replicating and extending the statistical power analysis
#' from Table 3 cell block 2.1 of the paper by Dwivedi et al. (2017). It deals with
#' scenarios involving skewed distributions with equal variance and different means
#' in the two groups. It acts as a wrapper around `replext_t2_c1.1`, with specific
#' adjustments for skewness parameters and means.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), reflecting the power analysis.
#'
#' @examples
#' replext_t3_c2.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c2.1 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 1, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 2.2
#'
#' This function aims to replicate and extend the statistical power analysis
#' from Table 3 cell block 2.2 in the paper by Dwivedi et al. (2017). It deals with
#' scenarios involving skewed distributions with different variances and means
#' in the two groups. It is a wrapper around `replext_t2_c1.1`, with adjusted means,
#' variances, and skewness parameters.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), representing the power analysis.
#'
#' @examples
#' replext_t3_c2.2(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c2.2 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 3, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 3.1
#'
#' This function is aimed at replicating and extending the statistical power analysis
#' from Table 3 cell block 3.1 in the paper by Dwivedi et al. (2017). It addresses
#' scenarios with different skewed distributions but equal variance and different
#' means in the two groups. It utilizes `replext_t2_c1.1` for calculations by
#' setting specific skewness parameters.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 1.0.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), indicating the power analysis.
#'
#' @examples
#' replext_t3_c3.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c3.1 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 1, Sk1 = 0.8, Sk2 = 1.0,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 3.2
#'
#' This function aims to replicate and extend the statistical power analysis
#' from Table 3 cell block 3.2 in the paper by Dwivedi et al. (2017). It is designed
#' for scenarios involving different skewed distributions with different variances
#' and different means in the two groups. The function is a wrapper around
#' `replext_t2_c1.1`, applying specific skewness parameters, means, and variances.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 1.0.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), indicating the power analysis.
#'
#' @examples
#' replext_t3_c3.2(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c3.2 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 3, Sk1 = 0.8, Sk2 = 1.0,
                            n1 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n2 = c(3,4,5,6,7,8,9,10,15,25,50,100),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 4.1
#'
#' This function is crafted to replicate and extend the statistical power analysis
#' from Table 3 cell block 4.1 in the paper by Dwivedi et al. (2017). It focuses on
#' scenarios with unequal sample sizes, same skewed distribution, and equal variance
#' between the two groups. It utilizes `replext_t2_c1.1` with adjusted skewness parameters,
#' means, and specific sample sizes.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group, specific to unequal sample size scenarios.
#' @param n2 Vector of sample sizes for the second group, corresponding to n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), reflecting the power analysis.
#'
#' @examples
#' replext_t3_c4.1(n1 = c(4), n2 = c(3), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c4.1 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 1, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(4,3,5,4,6,4),
                            n2 = c(2,4,3,5,3,6),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 4.2
#'
#' This function is designed to replicate and extend the statistical power analysis
#' from Table 3 cell block 4.2 in the paper by Dwivedi et al. (2017). It addresses
#' scenarios with unequal sample sizes, the same skewed distribution, but different
#' variances between the two groups. The function acts as a wrapper around
#' `replext_t2_c1.1`, applying specific skewness parameters, variances, and
#' unequal sample sizes.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is 0.8.
#' @param Sk2 Skewness parameter for the second group, default is 0.8.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, designed for unequal sample sizes.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), representing the power analysis.
#'
#' @examples
#' replext_t3_c4.2(n1 = c(6), n2 = c(3), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c4.2 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 3, Sk1 = 0.8, Sk2 = 0.8,
                            n1 = c(4,3,5,4,6,4),
                            n2 = c(2,4,3,5,3,6),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 5.1
#'
#' This function is designed to replicate and extend the statistical power analysis
#' from Table 3 cell block 5.1 in the paper by Dwivedi et al. (2017). It focuses on
#' scenarios with normal distribution and unequal sample sizes, using the same
#' means and variances for both groups. It acts as a wrapper around
#' `replext_t2_c1.1`, with modifications in means and sample sizes.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 1.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of unequal sample sizes for the second group.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), representing the power analysis.
#'
#' @examples
#' replext_t3_c5.1(n1 = c(4), n2 = c(11), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c5.1 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 1, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6),
                            n2 = c(7,11,10,9),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}

#' Replicate and Extend Statistical Power Analysis from Table 3 Cell 5.2
#'
#' This function is tailored to replicate and extend the statistical power analysis
#' from Table 3 cell block 5.2 in the paper by Dwivedi et al. (2017). It covers
#' scenarios with normal distribution, unequal sample sizes, and different variances
#' in the two groups. The function uses `replext_t2_c1.1` for its calculations,
#' with adjusted means, variances, and sample sizes.
#'
#' @param M1 Mean for the first group, default is 5.
#' @param S1 Standard deviation for the first group, default is 1.
#' @param M2 Mean for the second group, default is 7.
#' @param S2 Standard deviation for the second group, default is 3.
#' @param Sk1 Skewness parameter for the first group, default is NULL (normal distribution).
#' @param Sk2 Skewness parameter for the second group, default is NULL (normal distribution).
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of unequal sample sizes for the second group.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds, default is 0.95.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and the proportions
#' of significant p-values for each test (ST, WT, NPBTT, WRST, PTT), indicating the power analysis.
#'
#' @examples
#' replext_t3_c5.2(n1 = c(4), n2 = c(11), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies using
#' nonparametric bootstrap test with pooled resampling method. Stat Med. 2017 Jun 30;36(14):2187-2205.
#' doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @seealso \code{\link{replext_t2_c1.1}}
#'
#' @export
replext_t3_c5.2 <- function(M1 = 5, S1 = 1, M2 = 7, S2 = 3, Sk1 = NULL, Sk2 = NULL,
                            n1 = c(3,4,5,6),
                            n2 = c(7,11,10,9),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95) {

  replext_t2_c1.1(M1, S1, M2, S2, Sk1, Sk2, n1, n2, n_simulations, nboot, conf.level)
}
