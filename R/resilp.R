#' resilp
#' @description
#' An extended function from `silp`, applying the bootstrap method to obtain standard error estimation. 
#' Note: When using `silp` with the nearest positive definite matrix (npd = TRUE), this function should be used to obtain 
#' reliable inference.
#' 
#' @param fit A result object from `silp`.
#' @param R Integer. The number of bootstrap samples. Default is 2000.
#' @param progress Logical. Whether to display a progress bar. Default is `FALSE`.
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

resilp = function(fit, R = 2000, progress = T){
  sta = Sys.time()
  ind_boot = replicate(R, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = T))
  ind_boot = as.list(as.data.frame(ind_boot))

  bt_silp = function(ind_boot){
    result = silp(fit@raw_model, fit@raw_data[as.numeric(ind_boot),] , npd = fit@npd)
    res = result@pa
    
    if(res@optim$warn.txt == ""){
      return("lav" = lavaan::partable(res)$est )
      
    }else{
      while (res@optim$warn.txt != "") {
        ind = replicate(1, sample(1:nrow(fit@raw_data), nrow(fit@raw_data), replace = T))
        res = silp(fit@raw_model, fit@raw_data[as.numeric(ind),] , npd = fit@npd)$pa
      }
      return("lav" = lavaan::partable(res)$est )
    }
  }  
  
  b_silp = purrr::map(ind_boot, \(ind_boot) (bt_silp(ind_boot))
                      ,.progress = progress)  
  
  b_est = do.call(cbind, b_silp)
  #2:11
  
  b_est = cbind(lavaan::partable(fit@pa)[,2:12], b_est)
  fin = Sys.time() - sta 
  units(fin) = "secs"
  
  fit@boot = data.frame(b_est)
  fit@origine = as.data.frame(c(lavaan::parTable(fit@pa)$est))
  fit@time_resilp = as.numeric(fin)
  
  return(fit)
}











