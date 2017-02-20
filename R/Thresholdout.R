#' @title Thresholdout
#'
#' @description
#' Create Thresholdout object for adaptive data analysis and holdout reuse.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new(Threshold, Sigma, Budget)}}{Creates a new \code{Thresholdout} object.}
#'   \item{\code{$query(train_val, holdout_val)}}{Sends a query to the holdout dataset.}
#' }
#'
#' @section Actives:
#' \describe{
#'   \item{\code{$Threshold}}{gets the Threshold value.}
#'   \item{\code{$Sigma}}{gets the Sigma value.}
#'   \item{\code{$Budget}}{gets the remaining Budget value.}
#'   \item{\code{$Record}}{gets the Record of previous queries.}
#' }
#'
#' @examples
#' library(rThresholdout)
#' Thresholdout_Obj <- Thresholdout$new(Threshold = 0.04, Sigma = 0.01, Budget = 1000)
#' Thresholdout_Obj$query(train_val = c(0.06, 0.07), holdout_val = c(0.05, 0.08))
#'
#' @importFrom stats runif
#' @importFrom R6 R6Class
#' @export

Thresholdout <- R6Class(
  classname = "Thresholdout",
  public = list(
    initialize = function(Threshold, Sigma, Budget) {
      private$threshold <- Threshold
      private$sigma <- Sigma
      private$budget <- Budget
    },
    query = function(train_val, holdout_val) {
      if (length(train_val) != length(holdout_val)) {
        stop("The inputs of training and holdout should be compatible")
      } else {
        N <- length(train_val)
      }
      if (private$budget < 1) {
        message("Budget exhausted")
      } else {
        private$noise_update(n = N)
        threshold_ind <- abs(train_val - holdout_val) > private$threshold + private$gamma + private$eta
        if (private$budget - sum(threshold_ind) < 1) {
          message("Budget is not enough for current query")
        }
        private$budget <- private$budget - sum(threshold_ind)
        result <- holdout_val + private$xi
        result[!threshold_ind] <- train_val[!threshold_ind]
        private$record <- append(private$record, list(data.table::data.table(matrix(result, nrow = 1))))
        return(result)
      }
    }
  ),
  private = list(
    threshold = NULL,
    sigma = NULL,
    budget = NULL,
    xi = NULL,
    gamma = NULL,
    eta = NULL,
    record = list(),
    noise_update = function(n) {
      private$xi <- rLaplace(n = n, s = 1 * private$sigma)
      private$gamma <- rLaplace(n = n, s = 2 * private$sigma)
      private$eta <- rLaplace(n = n, s = 4 * private$sigma)
    }
  ),
  active = list(
    Threshold = function() {
      private$threshold
    },
    Sigma = function() {
      private$sigma
    },
    Budget = function() {
      private$budget
    },
    Record = function() {
      data.table::rbindlist(private$record)
    }
  ),
  inherit = NULL,
  lock_objects = TRUE,
  class = TRUE,
  portable = TRUE,
  lock_class = TRUE,
  cloneable = FALSE
)


rLaplace <- function (n = 1, m = 0, s = 1) {
  q <- runif(n)
  ifelse(q < 0.5, s * log(2 * q) + m, -s * log(2 * (1 - q)) + m)
}

