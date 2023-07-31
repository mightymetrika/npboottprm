#' Simulated Data for Independent T-test
#'
#' A simulated data set to experiment with nonparboot() with test = "t"
#' @format ## `data_t`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{x}{A numeric variable}
#'   \item{grp}{A factor variable with group labels}
#' }
#' @source Simulated data
"data_t"

#' Simulated Data for Paired T-test
#'
#' A simulated data set to experiment with nonparboot() with test = "pt"
#' @format ## `data_pt`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{x}{A numeric variable}
#'   \item{y}{A numeric variable}
#' }
#' @source Simulated data
"data_pt"

#' Simulated Data for F-test
#'
#' A simulated data set to experiment with nonparboot() with test = "F"
#' @format ## `data_f`
#' A data frame with 15 rows and 2 columns:
#' \describe{
#'   \item{x}{A numeric variable}
#'   \item{grp}{A factor variable with group labels}
#' }
#' @source Simulated data
"data_f"

#' Simulated Data for Paired T-test Using Identically Distributed Data
#'
#' A simulated data set to experiment with nonparboot() with test = "pt" when
#' both variables are drawn from identical distributions
#' @format ## `data_pt_id`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{x}{A numeric variable}
#'   \item{y}{A numeric variable}
#' }
#' @source Simulated data
"data_pt_id"

#' Simulated Data for F-test Using Identically Distributed Data
#'
#' A simulated data set to experiment with nonparboot() with test = "F" when all
#' groups are drawn from identical distributions
#' @format ## `data_f_id`
#' A data frame with 15 rows and 2 columns:
#' \describe{
#'   \item{x}{A numeric variable}
#'   \item{grp}{A factor variable with group labels}
#' }
#' @source Simulated data
"data_f_id"
