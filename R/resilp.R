#' resilp
#'
#' @param fit result object from silp
#' @param R number of bootstrap. Default 300
#' @param progress whether there is a progress bar
#' @param sig_level significant level. Default 0.05
#'
#' @return parameter table of bootstrap outcome of silp
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

# 


resilp = function(fit, R = 300, progress = T, sig_level = 0.05){
  sta = Sys.time()
  ind_boot = replicate(R, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = T))
  ind_boot = as.list(as.data.frame(ind_boot))
  
  # ind_boot = ind_boot[[1]]
  bt_silp = function(ind_boot){
    result = silp(fit@raw_model, fit@raw_data[as.numeric(ind_boot),] , type = fit@type)
    res = result@pa
    
    if(res@optim$warn.txt == ""){
      return("lav" = lavaan::partable(res)$est )
      
    }else{
      while (res@optim$warn.txt != "") {
        ind = replicate(1, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = T))
        res = silp(fit@raw_model, fit@raw_data[as.numeric(ind),] , type = fit@type)$pa
      }
      return("lav" = lavaan::partable(res)$est )
    }
  }  
  
  b_silp = purrr::map(ind_boot, \(ind_boot) (bt_silp(ind_boot))
                      ,.progress = progress)  
  
  # lavaan::partable(fit$pa)
  b_est = do.call(cbind, b_silp)
  #2:11
  
  
  result = lavaan::partable(fit@pa)[,2:12]
  result["estimated"] = rowMeans(b_est[,1:ncol(b_est)])
  result["se"] = apply(b_est[,1:ncol(b_est)], 1, sd)
  result["CI_lower"] = apply(b_est[,1:ncol(b_est)], 1, quantile, probs = sig_level/2)
  result["CI_upper"] = apply(b_est[,1:ncol(b_est)], 1, quantile, probs = 1 - sig_level/2)
  b_est = cbind(lavaan::partable(fit@pa)[,2:12], b_est)
  fin = Sys.time() - sta 
  units(fin) = "secs"
  
  
  fit@boot = data.frame(b_est)
  fit@statistic = as.data.frame(result)
  fit@origine = as.data.frame(c(lavaan::parTable(fit@pa)$est))
  fit@time_resilp = as.numeric(fin)
  
  return(fit)
  # return(list("boot" = data.frame(b_est), "statistic" = result, 
  #             "origine" = c(lavaan::parTable(fit@pa)$est)
  #             ,"time" = fin))
  
}











