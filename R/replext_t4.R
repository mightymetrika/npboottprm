#' Replicate and Extend Simulation Results from Table 4 Cell 1.1
#'
#' This function aims to replicate and extend the simulation results from
#' Table 4 cell 1.1 of the paper by Dwivedi et al. (2017). The default
#' parameters are set to replicate the results for the lognormal distribution
#' scenarios as presented in the paper, while modifying the parameter values
#' allows for an extension of these results.
#'
#' @param rdist Distribution type, default is 'rlnorm' (lognormal).
#'        Other options are 'rpois' (Poisson), 'rchisq' (Chi-squared), and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 1.
#' @param par2_1 Second parameter for the first group's distribution,
#'        default is 0.6.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is 2.
#' @param par2_2 Second parameter for the second group's distribution,
#'        default is 1.
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
#' replext_t4_c1.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c1.1 <- function(rdist = "rlnorm", par1_1 = 1, par2_1 = 0.6,
                            par1_2 = 2, par2_2 = 1,
                            n1 = c(5,5,10),
                            n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  # Ensure n1 and n2 are of equal length
  if (length(n1) != length(n2)) {
    stop("n1 and n2 must be of equal length")
  }

  # Generate data based on distribution type
  generate_data <- function(n, dist, par1, par2) {
    if (dist == "rlnorm") {
      stats::rlnorm(n, par1, par2)
    } else if (dist == "rpois"){
      stats::rpois(n, par1)
    } else if (dist == "rchisq"){
      stats::rchisq(n, par1, par2)
    } else if (dist == "rcauchy"){
      stats::rcauchy(n, par1, par2)
    } else {
      stop("rdist must be one of 'rlnorm', 'rpois', 'rchisq', or 'rcauchy'")
    }
  }

  # Handle rdist as either a vector or a single string
  get_dist_type <- function(index) {
    if (length(rdist) == 1) {
      return(rdist)
    } else if (length(rdist) == 2) {
      return(rdist[index])
    } else {
      stop("rdist must be a single value or a vector of length 2")
    }
  }

  # Get results for one iteration of the simulation or one pair of sample sizes
  get_result <- function(n1, n2) {

      F1 <- generate_data(n1, get_dist_type(1), par1_1, par2_1)
      F2 <- generate_data(n2, get_dist_type(2), par1_2, par2_2)

    df <- data.frame(x = c(F1, F2), grp = rep(c("F1", "F2"), c(n1, n2)))

    # Test p-value for each method
    npbtt <- tryCatch({
      nonparboot(df, x = "x", grp = "grp", nboot = nboot, test = "t", conf.level = conf.level)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    st <- tryCatch({
      stats::t.test(x ~ grp, df, var.equal = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    wt <- tryCatch({
      stats::t.test(x ~ grp, df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    wrst <- tryCatch({
      stats::wilcox.test(x ~ grp, df, exact = TRUE)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    ptt_res <- tryCatch({
       MKinfer::perm.t.test(x ~ grp, df, paired = FALSE)
    }, error = function(e) NA)

    ptta <- tryCatch({
      ptt_res$p.value <= 1 - conf.level
    }, error = function(e) NA)

    ptte <- tryCatch({
      ptt_res$perm.p.value <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named vector
    return(c(NPBTT = npbtt, ST = st, WT = wt, WRST = wrst, PTTa = ptta, PTTe = ptte))
  }

  # Run n_simulations at each sample size pair
  results <- lapply(seq_along(n1), function(i) {
    replicate(n_simulations, get_result(n1[i], n2[i]))
  })

  # Use lapply to calculate proportions and create a data frame for each sample size combination
  proportions_list <- lapply(seq_along(results), function(i) {
    proportions <- rowMeans(results[[i]], na.rm = TRUE)
    data.frame(
      n1 = n1[i],
      n2 = n2[i],
      ST = proportions["ST"],
      WT = proportions["WT"],
      NPBTT = proportions["NPBTT"],
      WRST = proportions["WRST"],
      PTTa = proportions["PTTa"],
      PTTe = proportions["PTTe"]
    )
  })

  # Combine the list of data frames into a single data frame
  proportions_df <- do.call(rbind, proportions_list)

  return(proportions_df)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 2.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1' designed
#' to replicate and extend the simulation results from Table 4 cell 2.1 of
#' the paper by Dwivedi et al. (2017). The default parameters are modified
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
#'        default is 10 for Poisson's lambda.
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
#' replext_t4_c2.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c2.1 <- function(rdist = "rpois", par1_1 = 5, par2_1 = NULL,
                            par1_2 = 10, par2_2 = NULL,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 3.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1' intended
#' to replicate and extend the simulation results from Table 4 cell 3.1 of
#' the paper by Dwivedi et al. (2017). The default parameters are configured
#' to match the Chi-squared distribution scenarios as detailed in the paper.
#' Adjusting these parameters allows users to extend these results further.
#'
#' @param rdist Distribution type, with the default set to 'rchisq' (Chi-squared).
#'        Other options include 'rlnorm' (lognormal), 'rpois' (Poisson), and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 3 for Chi-squared's degrees of freedom (df).
#' @param par2_1 Second parameter for the first group's distribution,
#'        typically 0 for Chi-squared.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is 6 for Chi-squared's degrees of freedom (df).
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
#' replext_t4_c3.1(n1 = c(10), n2 = c(10),n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c3.1 <- function(rdist = "rchisq", par1_1 = 3, par2_1 = 0,
                            par1_2 = 6, par2_2 = 0,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 4.1
#'
#' This function is a specialized wrapper around 'replext_t4_c1.1', tailored
#' to replicate and extend the simulation results from Table 4 cell 4.1 of
#' the paper by Dwivedi et al. (2017). It sets the default parameters to
#' correspond with the lognormal distribution scenarios for this specific cell,
#' allowing for both replication and extension of the results.
#'
#' @param rdist Distribution type, with the default set to 'rlnorm' (lognormal).
#'        Other options include 'rpois' (Poisson), 'rchisq' (Chi-squared),
#'        and 'rcauchy' (Cauchy).
#' @param par1_1 First parameter (meanlog) for the first group's distribution,
#'        default is 1.
#' @param par2_1 Second parameter (sdlog) for the first group's distribution,
#'        default is 0.6.
#' @param par1_2 First parameter (meanlog) for the second group's distribution,
#'        default is 3.
#' @param par2_2 Second parameter (sdlog) for the second group's distribution,
#'        default is 4.
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
#' replext_t4_c4.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c4.1 <- function(rdist = "rlnorm", par1_1 = 1, par2_1 = 0.6,
                            par1_2 = 3, par2_2 = 4,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 5.1
#'
#' This function is a specialized version of 'replext_t4_c1.1', designed
#' to replicate and extend the simulation results from Table 4 cell 5.1 of
#' the paper by Dwivedi et al. (2017). It adjusts the default parameters to
#' match the Cauchy distribution scenarios as described in this particular cell,
#' facilitating both replication and extension of these results.
#'
#' @param rdist Distribution type, with the default set to 'rcauchy' (Cauchy).
#'        Other options include 'rlnorm' (lognormal), 'rpois' (Poisson), and
#'        'rchisq' (Chi-squared).
#' @param par1_1 First parameter (location) for the first group's distribution,
#'        default is 5.
#' @param par2_1 Second parameter (scale) for the first group's distribution,
#'        default is 2.
#' @param par1_2 First parameter (location) for the second group's distribution,
#'        default is 10.
#' @param par2_2 Second parameter (scale) for the second group's distribution,
#'        default is 4.
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
#'       'par1' is 'df' (degrees of freedom) and 'par2' 'ncp' (non-centrality
#'       parameter). Lastly, for rcauchy (Cauchy distribution), 'par1' is the
#'       'location' parameter and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_t4_c5.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c5.1 <- function(rdist = "rcauchy", par1_1 = 5, par2_1 = 2,
                            par1_2 = 10, par2_2 = 4,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 6.1
#'
#' This function, a specialized variant of 'replext_t4_c1.1', is designed
#' to replicate and extend the simulation results from Table 4 cell 6.1 of
#' the paper by Dwivedi et al. (2017). It employs different distributions for
#' the two groups, using Chi-squared and Poisson distributions respectively,
#' in line with the specific cell conditions.
#'
#' @param rdist Vector of distribution types, with the defaults set to
#'        'rchisq' (Chi-squared) for the first group and 'rpois' (Poisson) for
#'        the second group. Other options include 'rlnorm' (lognormal) and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter for the first group's distribution,
#'        default is 6 for Chi-squared's degrees of freedom.
#' @param par2_1 Second parameter for the first group's distribution,
#'        typically 0 for Chi-squared.
#' @param par1_2 First parameter for the second group's distribution,
#'        default is 10 for Poisson's lambda.
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
#'       'par1' is 'df' (degrees of freedom) and 'par2' is typically 0 as 'ncp'
#'       (non-centrality parameter) is not often used. Lastly, for rcauchy (Cauchy distribution),
#'       'par1' is the 'location' parameter and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_t4_c6.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c6.1 <- function(rdist = c("rchisq", "rpois"), par1_1 = 6, par2_1 = 0,
                            par1_2 = 10, par2_2 = NULL,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

#' Replicate and Extend Simulation Results from Table 4 Cell 7.1
#'
#' This function is a customized version of 'replext_t4_c1.1', created to
#' replicate and extend the simulation results from Table 4 cell 7.1 of
#' the paper by Dwivedi et al. (2017). It is configured to use different
#' distributions for each group, specifically a lognormal distribution for
#' the first group and a Chi-squared distribution for the second group,
#' aligning with the specific cell conditions.
#'
#' @param rdist Vector of distribution types, with the defaults set to
#'        'rlnorm' (lognormal) for the first group and 'rchisq' (Chi-squared) for
#'        the second group. Other options include 'rpois' (Poisson) and
#'        'rcauchy' (Cauchy).
#' @param par1_1 First parameter (meanlog) for the first group's lognormal distribution,
#'        default is 1.
#' @param par2_1 Second parameter (sdlog) for the first group's lognormal distribution,
#'        default is 0.6.
#' @param par1_2 First parameter (df) for the second group's Chi-squared distribution,
#'        default is 6.
#' @param par2_2 Second parameter for the second group's Chi-squared distribution,
#'        typically 0.
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
#'       'par1' is 'df' (degrees of freedom) and 'par2' is typically 0 as 'ncp'
#'       (non-centrality parameter) is not often used. Lastly, for rcauchy (Cauchy distribution),
#'       'par1' is the 'location' parameter and 'par2' is the 'scale' parameter.
#'
#' @return A data frame with columns for each sample size pair (n1, n2) and
#'         the proportions of significant p-values for each test (ST, WT, NPBTT,
#'         WRST, PTTa, PTTe).
#'
#' @examples
#' replext_t4_c7.1(n1 = c(10), n2 = c(10), n_simulations = 1)
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size
#' studies using nonparametric bootstrap test with pooled resampling method.
#' Stat Med. 2017 Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @export
replext_t4_c7.1 <- function(rdist = c("rlnorm", "rchisq"), par1_1 = 1, par2_1 = 0.6,
                            par1_2 = 6, par2_2 = 0,
                            n1 = c(5,5,10), n2 = c(5,10,10),
                            n_simulations = 10000, nboot = 1000,
                            conf.level = 0.95){

  replext_t4_c1.1(rdist, par1_1, par2_1, par1_2, par2_2, n1, n2, n_simulations,
                  nboot, conf.level)
}

