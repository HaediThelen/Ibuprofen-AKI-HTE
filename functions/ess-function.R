# Efective Sample Size Function
# inputs: data frame, treatment column in "", and weights column in ""

ess <- function(data, trt, wts) {
  data.trt <- data %>% filter(.data[[trt]] == 1)
  n.trt <-(sum(data.trt[[wts]])^2)/ (sum(data.trt[[wts]]^2))
  data.ctrl <- data %>% filter(.data[[trt]] == 0)
  n.ctrl <- (sum(data.ctrl[[wts]])^2)/ (sum(data.ctrl[[wts]]^2))
  n.total <- n.trt + n.ctrl
  
  result <- data.frame(
    "N treated" = n.trt,
    "N control" = n.ctrl,
    "N total" = n.total
  )
  
  return(result)
}


ess_mult <- function(data, trt, wts) {
  # for each value of trt, calculate the effective sample size
  # There coudl be 1 or more treatment groups, so we will loop through each unique value of trt
  trt_values <- unique(data[[trt]])
  ess_list <- list()
  for (i in trt_values) {
    data.trt <- data %>% filter(.data[[trt]] == i)
    n.trt <-(sum(data.trt[[wts]])^2)/ (sum(data.trt[[wts]]^2))
    ess_list[[as.character(i)]] <- n.trt
  }
  #make result dataframe
  result <- data.frame(
    "Treatment" = names(ess_list),
    "Effective Sample Size" = unlist(ess_list)
  )
  # Add a total row
  result <- rbind(result, data.frame("Treatment" = "Total", "Effective Sample Size" = sum(unlist(ess_list)))
  )
  rownames(result) <- NULL
  return(result)
}


