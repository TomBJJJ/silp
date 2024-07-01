#' Define silp class
#' @import methods
#' @importClassesFrom lavaan lavaan
#' @slot raw_model character. 
#' @slot rapi_model character. 
#' @slot time numeric. 
#' @slot type character. 
#' @slot raw_data data.frame. 
#' @slot fa lavaan. 
#' @slot reliability data.frame. 
#' @slot composite_data data.frame. 
#' @slot pa lavaan. 
#' @slot boot data.frame. 
#' @slot statistic data.frame. 
#' @slot origine data.frame. 
#' @slot time_resilp numeric. 
#' @exportClass Silp
#' @exportMethod summary

setClass("Silp", slots = list(raw_model = "character", rapi_model = "character", 
                              time = "numeric", type = "character", raw_data = "data.frame", fa = "lavaan", 
                              reliability = "data.frame", composite_data = "data.frame", pa = "lavaan",
                              boot = "data.frame", statistic = "data.frame", origine = "data.frame",
                              time_resilp = "numeric"))

setMethod("summary", signature("Silp"),function(object, method = "Bootstrap", sig_level = 0.05){ 
  print(object@pa) 
  
  if(dim(object@statistic)[1] != 0){
    if(method == "Bootstrap"){
      cat("\n")
      cat("\n")
      cat("Partable")
      cat("\n")
      print(object@statistic)  
      
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
      res = object@statistic[indx,]
      res["CI_lower"] = CI_l
      res["CI_upper"] = CI_u
      
      cat("\n")
      cat("\n")
      cat("Partable")
      cat("\n")
      print(res)  
      
    }
  }})

# setMethod("summary", signature("Silp"),function(object, method = "Bootstrap", sig_level = 0.05){ 
#   print(object@pa) 
#   
#   if(dim(object@statistic)[1] != 0){
#     if(method == "Bootstrap"){
#       cat("\n")
#       cat("\n")
#       cat("Partable")
#       cat("\n")
#       print(object@statistic)  
#       
#     }else if(method == "BC_b"){
#       org = object@origine
#       #bootstrap sample
#       res = (object@boot)
#       res = res[!res[,2] == "==", ]
#       res = res[!(str_detect(res[,3], "pool") == T & res[,2] == "=~") ,]
#       indx = as.numeric(rownames(res))
#       res = res[,12:ncol(res)]
#       
#       #original sample estimation
#       
#       org = as.numeric(unlist(org))
#       org = org[as.numeric(rownames(res))]
#       od = t(apply(res, 1, order))
#       
#       #order data
#       z_adj = c()
#       for (i in 1:nrow(res)) {
#         res[i,] = res[i,][od[i,]]
#         z_adj = append(z_adj, qnorm(sum(res[i,] < org[i])/ncol(res)))
#       }
#       p_l = pnorm(qnorm(sig_level/2) + 2*z_adj)
#       p_u = pnorm(qnorm(1-sig_level/2) + 2*z_adj)
#       
#       CI_l = as.numeric(mapply(quantile, probs = p_l, as.list(as.data.frame(t(as.matrix(res))))))
#       CI_u = as.numeric(mapply(quantile, probs = p_u, as.list(as.data.frame(t(as.matrix(res))))))
#       res = object@statistic[indx,]
#       res["CI_lower"] = CI_l
#       res["CI_upper"] = CI_u
#       
#       cat("\n")
#       cat("\n")
#       cat("Partable")
#       cat("\n")
#       print(res)  
#       
#     }
#   }})



