#' Replicate and Extend Simulation Results from Table S1 Cell 1.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1' designed
#' to replicate and extend the type I error simulation results from Table S1
#' cell 1.1 of the paper by Dwivedi et al. (2017).
#'
#' @param rdist Distribution type, default is 'rlnorm' (lognormal).
#'        Other options are 'rpois' (Poisson), 'rchisq' (Chi-squared), and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 1.
#' @param par2_1 Second parameter for the first group's distribution,
#'        default is 0.6.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is 1.
#' @param par2_2 Second parameter for the second group's distribution,
#'        default is 0.6.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same
#'        length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds,
#'        default is 0.95.
#'
#' @note When using rlnorm (lognormal distribution), 'par1' represents 'meanlog'
#'       (the mean of the logarithms) and 'par2' represents 'sdlog' (the standard
#'       deviation of the logarithms). For rpois (Poisson distribution), 'par1' is
#'       'lambda' (the rate parameter). In the case of rchisq (Chi-squared distribution),
#'       'par1' is 'df' (degrees of freedom) and 'par2' is 'ncp' (non-centrality parameter).
#'       Lastly, for rcauchy (Cauchy distribution), 'par1' is the 'location' parameter
#'       and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_ts1_c1.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_ts1_c1.1 <- function(rdist = "rlnorm", par1_1 = 1, par2_1 = 0.6,
                            par1_2 = 1, par2_2 = 0.6,
                            n1 = c(5,5,10),
                            n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table S1 Cell 2.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1' designed
#' to replicate and extend the type I error simulation results from Table S1 cell
#' 2.1 of the paper by Dwivedi et al. (2017). The default parameters are modified
#' to align with the Poisson distribution scenarios as described in the paper.
#' Adjusting the parameters enables the extension of these results.
#'
#' @param rdist Distribution type, with the default set to 'rpois' (Poisson).
#'        Other options include 'rlnorm' (lognormal), 'rchisq' (Chi-squared),
#'        and 'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 5 for Poisson's lambda.
#' @param par2_1 Second parameter for the first group's distribution,
#'        typically NULL for Poisson.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is 5 for Poisson's lambda.
#' @param par2_2 Second parameter for the second group's distribution,
#'        typically NULL for Poisson.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same
#'        length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds,
#'        default is 0.95.
#'
#' @note When using rlnorm (lognormal distribution), 'par1' represents 'meanlog'
#'       (the mean of the logarithms) and 'par2' represents 'sdlog' (the standard
#'       deviation of the logarithms). For rpois (Poisson distribution), 'par1' is
#'       'lambda' (the rate parameter). In the case of rchisq (Chi-squared distribution),
#'       'par1' is 'df' (degrees of freedom) and 'par2' is 'ncp' (non-centrality parameter).
#'       Lastly, for rcauchy (Cauchy distribution), 'par1' is the 'location' parameter
#'       and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_ts1_c2.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_ts1_c2.1 <- function(rdist = "rpois", par1_1 = 5, par2_1 = NULL,
                            par1_2 = 5, par2_2 = NULL,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table S1 Cell 3.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1' intended
#' to replicate and extend the type I error simulation results from Table S1
#' cell 3.1 of the paper by Dwivedi et al. (2017). The default parameters are
#' configured to match the Chi-squared distribution scenarios as detailed in the
#' paper. Adjusting these parameters allows users to extend these results further.
#'
#' @param rdist Distribution type, with the default set to 'rchisq' (Chi-squared).
#'        Other options include 'rlnorm' (lognormal), 'rpois' (Poisson), and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 3 for Chi-squared's degrees of freedom (df).
#' @param par2_1 Second parameter for the first group's distribution,
#'        typically 0 for Chi-squared.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is  3 for Chi-squared's degrees of freedom (df).
#' @param par2_2 Second parameter for the second group's distribution,
#'        typically 0 for Chi-squared.
#' @param n1 Vector of sample sizes for the first group.
#' @param n2 Vector of sample sizes for the second group, must be the same
#'        length as n1.
#' @param n_simulations Number of simulations to run, default is 10,000.
#' @param nboot Number of bootstrap samples, default is 1000.
#' @param conf.level Confidence level for calculating p-value thresholds,
#'        default is 0.95.
#'
#' @note When using rlnorm (lognormal distribution), 'par1' represents 'meanlog'
#'       (the mean of the logarithms) and 'par2' represents 'sdlog' (the standard
#'       deviation of the logarithms). For rpois (Poisson distribution), 'par1' is
#'       'lambda' (the rate parameter). In the case of rchisq (Chi-squared distribution),
#'       'par1' is 'df' (degrees of freedom) and 'par2' is 'ncp'
#'       (non-centrality parameter). Lastly, for rcauchy (Cauchy distribution),
#'       'par1' is the 'location' parameter and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_ts1_c3.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_ts1_c3.1 <- function(rdist = "rchisq", par1_1 = 3, par2_1 = 0,
                            par1_2 = 3, par2_2 = 0,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}
