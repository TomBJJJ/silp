#' resilp
#' @description
#' An extended function from `silp`, applying the bootstrap method to obtain standard error estimation. 
#' Note: When using `silp` with the nearest positive definite matrix (npd = TRUE), this function should be used to obtain 
#' reliable inference.
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' 
#' @param fit A result object from `silp`.
#' @param R Integer. The number of bootstrap samples. Default is 2000.
#' @param progress progress bar
#' @param max_try Maximum resampling attempts per bootstrap sample.
#' @return
#' An object of class "Silp".
#' @export
#'
#' @examples
#' if (requireNamespace("future", quietly = TRUE)) {
#'   future::plan(future::multisession, workers = 2)
#' }
#' 
#' n_obs = 100
#' corr = 0.1
#' effect = 0.12
#' ld = c(1, 1, 1, 1)
#' alp = 0.9
#' data = generate_data(n_obs, corr, effect, ld, alp)
#' model = "
#'   fy =~ y1 + y2 + y3 + y4
#'   fx =~ x1 + x2 + x3 + x4
#'   fz =~ z1 + z2 + z3 + z4
#'   fy ~  fx + fz + fx:fz
#' "
#' fit = silp(model, data)
#' resilp(fit, R = 100)


resilp = function(fit, R = 2000, progress = TRUE, max_try = 100) {
  
  # 提示目前是否是平行環境
  if (requireNamespace("future", quietly = TRUE)) {
    if (inherits(future::plan(), "sequential")) {
      message("[resilp] Currently running sequentially. Consider using plan(multisession) for faster bootstrapping.")
    }
  } else {
    message("[resilp] 'future' not detected. Will use single-core mode.")
  }
  
  sta <- Sys.time()
  ind_boot <- replicate(R, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = TRUE))
  ind_boot <- as.list(as.data.frame(ind_boot))
  
  bt_silp <- function(ind_boot, fit, max_try = max_try) {
    success <- FALSE
    try_count <- 1
    res <- NULL
    has_warning <- FALSE
    has_resample <- FALSE  
    
    while (!success && try_count <= max_try) {
      boot_data <- fit@raw_data[as.numeric(ind_boot), ]
      has_warning <- FALSE
      
      result <- tryCatch({
        withCallingHandlers({
          silp(fit@raw_model, boot_data, npd = fit@npd)
        }, warning = function(w) {
          has_warning <<- TRUE
          invokeRestart("muffleWarning")
        })
      }, error = function(e) {
        message("[silp error] try ", try_count, " | ", e$message)
        return(NULL)
      })
      
      if (has_warning) result <- NULL
      
      if (!is.null(result)) {
        success <- TRUE
        res <- lavaan::partable(result@pa)$est
      } else {
        if (!has_resample) has_resample <- TRUE
        ind_boot <- sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = TRUE)
      }
      
      try_count <- try_count + 1
    }
    
    if (success) {
      return(list(
        lav = res,
        resampled = has_resample,
        try_count = try_count - 1
      ))
    } else {
      message("[FINAL FAIL] silp could not converge after ", max_try, " tries of resample")
      return(NULL)
    }
  }
  
  # ✅ 判斷是否使用 progressr 進度條
  if (requireNamespace("progressr", quietly = TRUE) && progress) {
    progressr::handlers(global = TRUE)
    b_silp <- progressr::with_progress({
      p <- progressr::progressor(along = ind_boot)
      future.apply::future_lapply(
        ind_boot,
        function(x) {
          res <- bt_silp(x, fit, max_try)
          p()
          res
        },
        future.seed = TRUE,
        future.packages = "lavaan"
      )
    })
  } else {
    if (progress) message("[resilp] progressr not installed. No progress bar will be shown.")
    b_silp <- future.apply::future_lapply(
      ind_boot,
      function(x) bt_silp(x, fit, max_try),
      future.seed = TRUE,
      future.packages = "lavaan"
    )
  }
  
  # remove NULL
  original_n <- length(b_silp)
  valid_b <- purrr::compact(b_silp)
  cleaned_n <- length(valid_b)
  n_fail <- original_n - cleaned_n
  if (n_fail > 0) {
    message("There ", ifelse(n_fail == 1, "is", "are"), " ", 
            n_fail, " failed bootstrap result", 
            ifelse(n_fail == 1, "", "s"), ".")
  }
  
  n_resample <- sum(sapply(valid_b, function(x) x$resampled))
  n_attempt <- sum(sapply(valid_b, function(x) x$try_count))
  
  lav_list <- lapply(valid_b, function(x) x$lav)
  b_est <- as.data.frame(t(do.call(rbind, lav_list)))
  colnames(b_est) <- paste0("boot", seq_len(ncol(b_est)))
  b_est <- cbind(lavaan::partable(fit@pa)[, 2:12], b_est)
  
  fin <- Sys.time() - sta 
  units(fin) <- "secs"
  
  fit@boot <- data.frame(b_est)
  fit@origine <- as.data.frame(c(lavaan::parTable(fit@pa)$est))
  fit@time_resilp <- as.numeric(fin)
  fit@tech <- append(fit@tech, list("R" = R, "resample count" = n_attempt))
  return(fit)
}




