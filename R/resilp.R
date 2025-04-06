#' resilp
#' @description
#' An extended function from `silp`, applying the bootstrap method to obtain standard error estimation. 
#' Note: When using `silp` with the nearest positive definite matrix (npd = TRUE), this function should be used to obtain 
#' reliable inference.
#' 
#' @param fit A result object from `silp`.
#' @param R Integer. The number of bootstrap samples. Default is 2000.
#' @param progress Logical. Whether to display a progress bar. Default is `FALSE`.
#' @param max_try Maximum resampling attempts per bootstrap sample.
#' @return
#' An object of class "Silp".
#' @export
#'
#' @examples
#' n_obs = 100
#' corr = 0.1
#' effect = 0.12
#' ld = c(1,1,1,1)
#' alp = 0.9
#' data = generate_data(n_obs, corr, effect, ld, alp)
#' model = "
#'   fy =~ y1 + y2 + y3 + y4
#'   fx =~ x1 + x2 + x3 + x4
#'   fz =~ z1 + z2 + z3 + z4
#'   fy ~  fx + fz + fx:fz
#' "
#' fit = silp(model, data)
#' resilp(fit, R = 10)





resilp = function(fit, R = 2000, progress = T, max_try = 100){

  sta = Sys.time()
  ind_boot = replicate(R, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = T))
  ind_boot = as.list(as.data.frame(ind_boot))


  #test
  bt_silp <- function(ind_boot, fit, max_try = max_try) {
    success <- FALSE
    try_count <- 1
    res <- NULL
    has_warning <- FALSE
    has_resample <- FALSE  
    
    while (!success && try_count <= max_try) {
      boot_data <- fit@raw_data[as.numeric(ind_boot), ]
      has_warning <- FALSE  # 每輪都重設
      
      result <- tryCatch({
        
        withCallingHandlers({
          silp(fit@raw_model, boot_data, npd = fit@npd)
          
          
        }, warning = function(w) {
          # message("[silp warning] try ", try_count, " | ", conditionMessage(w))
          has_warning <<- TRUE
          invokeRestart("muffleWarning")
        })
      }, error = function(e) {
        message("[silp error] try ", try_count, " | ", e$message)
        return(NULL)
      })
      
      # 如果有 warning，也視為失敗
      if (has_warning) {
        result <- NULL
      }
      
      if (!is.null(result)) {
        success <- TRUE
        res <- lavaan::partable(result@pa)$est
      } else {
        if (!has_resample) {
          # message("[bootstrap warning] resampling was needed in this bootstrap sample.")
          has_resample <- TRUE  # 記住這一輪有重抽
        }
        ind_boot <- sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = TRUE)
      }
      
      try_count <- try_count + 1
    }
    
    if (success) {
      return(list(
        lav = res,
        resampled = has_resample,
        try_count = try_count - 1  # 減掉最後一次失敗也加的那次
      ))
    } else {
      message("[FINAL FAIL] silp could not converge after ", max_try, " tries of resample")
      return(NULL)
    }
  }
  
  b_silp = purrr::map(ind_boot, ~bt_silp(.x, fit,max_try), .progress = TRUE)  


  #remove NA
  original_n <- length(b_silp)
  valid_b <- purrr::compact(b_silp)
  cleaned_n <- length(valid_b)
  n_fail <- original_n - cleaned_n
  if (n_fail > 0) {
    message("There ", ifelse(n_fail == 1, "is", "are"), " ", 
            n_fail, " failed bootstrap result", 
            ifelse(n_fail == 1, "", "s"), ".")
  }
  
  
  #resampled count
  n_resample <- sum(sapply(valid_b, function(x) x$resampled))
  
  #total number of attempts
  n_attempt <- sum(sapply(valid_b, function(x) x$try_count))
  
  #lavaan output
  lav_list <- lapply(valid_b, function(x) x$lav)
  b_est <- as.data.frame(t(do.call(rbind, lav_list)))
  colnames(b_est) <- paste0("boot", seq_len(ncol(b_est)))
  b_est = cbind(lavaan::partable(fit@pa)[,2:12], b_est)


  fin = Sys.time() - sta 
  units(fin) = "secs"
  
  fit@boot = data.frame(b_est)
  fit@origine = as.data.frame(c(lavaan::parTable(fit@pa)$est))
  fit@time_resilp = as.numeric(fin)
  
  fit@tech = append(fit@tech, list("R" = R, "resample count" = n_attempt))
  return(fit)
}








