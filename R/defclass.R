#' Define silp class
#' @import methods
#' @importClassesFrom lavaan lavaan
#' @slot raw_model The user-specified `lavaan` syntax model.
#' @slot rapi_model The revised model with the RAPI method.
#' @slot time The operation time for `silp` (in seconds).
#' @slot alp type of reliability used.
#' @slot npd Logical. Whether the nearest positive definite matrix is used.
#' @slot raw_data The input data.
#' @slot fa An object of class `lavaan` representing the CFA result.
#' @slot reliability The reliability index.
#' @slot composite_data The composite data for RAPI.
#' @slot pa The result of `silp`.
#' @slot boot The results of `resilp` from R bootstrap samples.
#' @slot origine The original `silp` estimation.
#' @slot time_resilp The operation time for `resilp` (in seconds).
#' @exportClass Silp
#' @exportMethod summary


setClass("Silp", slots = list(raw_model = "character", rapi_model = "character", 
                              time = "numeric",alp = "logical", npd = "logical", raw_data = "data.frame", fa = "lavaan", 
                              reliability = "data.frame", composite_data = "data.frame", pa = "lavaan",
                              boot = "data.frame", origine = "data.frame",
                              time_resilp = "numeric"))

setMethod("summary", signature("Silp"),function(object, method = "Bootstrap", sig_level = 0.05){ 
  print(object@pa) 

  if(length(object@time_resilp) != 0){
    b_est = object@boot[,-c(1:11)]
    result = lavaan::partable(object@pa)[,2:12]
    result["estimated"] = rowMeans(b_est[,1:ncol(b_est)])
    result["se"] = apply(b_est[,1:ncol(b_est)], 1, sd)
    result["CI_lower"] = apply(b_est[,1:ncol(b_est)], 1, quantile, probs = sig_level/2)
    result["CI_upper"] = apply(b_est[,1:ncol(b_est)], 1, quantile, probs = 1 - sig_level/2)
    if(method == "Bootstrap"){
      cat("\n")
      cat("\n")
      cat("Partable")
      cat("\n")
      print(result)  
      
    }else if(method == "BC_b"){
      org = object@origine
      #bootstrap sample
      res = (object@boot)
      res = res[!res[,2] == "==", ]
      res = res[!(str_detect(res[,3], "pool") == T & res[,2] == "=~") ,]
      indx = as.numeric(rownames(res))
      res = res[,12:ncol(res)]
      
      #original sample estimation
      
      org = as.numeric(unlist(org))
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
      res = result[indx,]
      res["CI_lower"] = CI_l
      res["CI_upper"] = CI_u
      
      cat("\n")
      cat("\n")
      cat("Partable")
      cat("\n")
      print(res)  
      
    }
  }})

