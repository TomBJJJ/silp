#' summary_table
#'
#' @param object Result form silp or resilp
#' @param method method of resampling choose Bootstrap or Biased corrected Bootstrap (BC_b).Default is Bootstrap.
#' @param sig_level significant level. Default 0.05. This should be the same as the sig_level in resilp.
#' @return summary table
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
#' summary_table(fit)
#'


summary_table = function(object, method = "Bootstrap", sig_level = 0.05){
  if(is.null(object$pa) == FALSE){
    #silp result
    cat("CFA model result \n")
    print(lavaan::summary(object$fa))
    cat("PA model result \n")
    print(lavaan::summary(object$pa))
    cat("Estimated reliability \n")
    print(object$reliability)

  }else if(is.null(object$pa) == TRUE){
    #resilp result
    if(method == "Bootstrap"){
      return(object$statistic)
    
    }else if(method == "BC_b"){
      #smaple estimated
      org = object$origine
      #bootstrap sample
      res = (object$boot)
      res = res[!res[,2] == "==", ]
      res = res[!(str_detect(res[,3], "pool") == T & res[,2] == "=~") ,]
      
      indx = as.numeric(rownames(res))
      # res = res[,12:ncol(res)]
      
      #original sample estimation
      org = org[as.numeric(rownames(res))]
      od = t(apply(res, 1, order))
      
      #order data
      z_adj = c()
      for (i in 1:nrow(res)) {
        res[i,] = res[i,][od[i,]]
        z_adj = append(z_adj, qnorm(sum(res[i,] < org[i])/ncol(res)))
      }
      p_l = pnorm(qnorm(sig_level/2) + 2*z_adj)
      p_u = pnorm(qnorm(1-sig_level/2) + 2*z_adj)
      CI_l = as.numeric(mapply(quantile, probs = p_l, as.list(as.data.frame(t(as.matrix(res))))))
      CI_u = as.numeric(mapply(quantile, probs = p_u, as.list(as.data.frame(t(as.matrix(res))))))
      
      res = object$statistic[indx,]
      res["CI_lower"] = CI_l
      res["CI_upper"] = CI_u
      return(res)

    }
  }
}


