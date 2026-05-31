#' Discount values in time. 
#' 
#' This method is a vectorized function to discount at 
#' time points (if only t_A is specified).
#' Alternatively computes the area under a discounted curve between
#' two time points (if both t_A and t_B are specified). 
#' 
#' The method utilized has greater numerical stability than using a power '^'
#' operator. 
#' 
#' @param value       The value to discount
#' @param t_A         The first time point to discount from
#' @param t_B         (optional) the last time point of the discount curve
#' @param annual_rate The discount rate which defaults to 0.03
#' @param scale       The time scale of t_A and t_B relative to annual. E.g., if
#'                    t_A is in units of days, then the scaling factor would be
#'                    365.2425.
#' @return            Discounted value(s)
#' @examples
#'   discount_value(1, 1)
#'   discount_value(1, 0, 1)
#'   discount_value(1, c(0, 125), c(365, 490), 0.04, 365)
discount_value <- function(value, t_A, t_B=NULL, annual_rate=0.03, scale=1)
{
  # Scale to time step
  if(scale != 1)
  {
    t_A <- t_A / scale
    if(!is.null(t_B)) t_B <- t_B / scale
  }
  
  # Convert discount to continuous exponential rate
  r <- -log(1+annual_rate) 

  if(is.null(t_B))
  { # Point estimate at t_B
    value*exp(r*t_A)
  } else 
  { # Analytic integral of curve
    (value/r)*(exp(r*t_B)-exp(r*t_A))
  }
}