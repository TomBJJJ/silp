#' silp
#'
#' @description
#' This function extends the `lavaan` function, allowing users to define moderation effects using the symbol ":". 
#' The RAPI method is used to estimate moderation effects.
#' 
#' @param model A `lavaan` syntax model with extension. The notation ":" implies interaction between two variables (see Example).
#' @param data The dataset for `lavaan` SEM.
#' @param center Character. Whether single or double mean centering is used for the product indicator. Default is "double".
#' @param tau.eq Logical. Specifies the type of reliability used to estimate error variance. If `TRUE`, Cronbach's alpha reliability is used. 
#' If `FALSE`, omega reliability is used. Default is `FALSE`.
#' @param npd Logical. Specifies the type of input used in `lavaan` SEM. Default is `FALSE` for raw data or `TRUE` for a covariance matrix. 
#' Applying a covariance matrix can resolve problems of a non-positive definite covariance matrix. 
#' If `TRUE`, `resilp` should be used to obtain reliable inference.
#' @param ... Other parameters passed to the `lavaan` SEM function.#' 
#' @return
#' An "Silp" class object.
#'
#' @export
#' @import stringr
#' @import stats
#'
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
#' silp(model, data)
# 
# n_obs = 100
# corr = 0.1
# effect = 0.12
# ld = c(1,1,1,1)
# alp = 0.9
# data = generate_data()
# model = "
#   fy =~ y1 + y2 + y3 + y4
#   fx =~ x1 + x2 + x3 + x4
#   fz =~ z1 + z2 + z3 + z4
#   fy ~  fx + fz + fx:fz
# "
# fit = silp(model, data)
# refit = resilp(fit)

silp = function(model, data, center = "double", tau.eq = F, npd = F ,... ){
  t0 = Sys.time()
  #model preprocess
  model. = parsing_model(model)
  usr_d = model.[str_detect(model., ":=") == T]
  eq = model.[str_detect(model., ":=") == F]
  #moderator eq
  mod_eq= eq[str_detect(eq, ":") == TRUE]
  
  for (l in 1:length(mod_eq)) {
    tempt = str_split_1(mod_eq[l], "~")
    if(str_detect(tempt[1],":") == T & str_detect(tempt[2],"1") == T){
      warning("current function don't support intercept of moderation efffect")
      break()
    }
  }
  
  #ov eq
  o_eq = eq[str_detect(eq, "=~") == TRUE]

  #CFA model
  MD = lavaan::cfa(str_c(o_eq, sep = "/n"), data,  bounds =  "pos.var")
  Rel = semTools::compRelSEM(MD, tau.eq = tau.eq, return.df = T)

  #lv regression
  l_eq = eq[str_detect(eq, pattern = "~~") == FALSE &
              str_detect(eq, pattern = "=~") == FALSE]
  
  l_eq = l_eq[str_detect(l_eq, pattern = "~") == TRUE]

    
  if(length(mod_eq) == 0){
    fit = lavaan::sem(model, data)
    est = lavaan::parTable(fit)[str_detect(lavaan::parTable(fit)$op, "~") == TRUE & str_detect(lavaan::parTable(fit)$op, "~~") == FALSE
                                & str_detect(lavaan::parTable(fit)$op, "=") == FALSE, ]
    
    est = est[c("lhs","op","rhs", "est")]
    attributes(fit)$raw_model = model
    
    t1 = Sys.time() - t0
    attributes(fit)$silp_time = t1
    
    return(list("raw_data" = data
                , "fa" = MD
                , "reliability" = NA
                , "composite_data" = NA
                , "pa" = fit ))
  }
  
  data_material = exo_moderator(l_eq, o_eq, mod_eq, Rel = Rel, model, data, center = center)
  # data_material$ps = data_material$ps + 3
  
  #update measurement part
  u_model = indicator_update(eq, data_material, o_eq)
  
  #update variance part
  u_model = variance_update(u_model, data_material,o_eq)
  
  
  rel_cor = rel_correction(data_material, o_eq)
  colnames(rel_cor) = str_replace_all(colnames(rel_cor), pattern = ":", replacement = "_")
  colnames(data_material$ps) = str_replace_all(colnames(data_material$ps), pattern = ":", replacement = "_")
  
  sample_cov_r = cov(data_material$ps)
  diag(sample_cov_r[,colnames(rel_cor)]) = as.numeric(diag(sample_cov_r[,colnames(rel_cor)]) - rel_cor )
  
  
  
  if(npd == T){

    if(min(eigen(sample_cov_r)$value) < 0){
      sample_cov_r_ap = sample_cov_r
      sample_cov_r_ap = as.matrix(Matrix::nearPD(sample_cov_r_ap, posd.tol =1e-01)$mat)
      u_model = eq[str_detect(eq, "=~") == F]
      # for (cons in 1:ncol(data_material$ps)) {
      #   u_model = u_model[str_detect(u_model, colnames(data_material$ps)[cons]) == FALSE ]
      # }
      
      colnames(sample_cov_r_ap) = str_remove_all(colnames(sample_cov_r), "pool_")
      rownames(sample_cov_r_ap) = str_remove_all(colnames(sample_cov_r), "pool_")
      u_model = str_replace_all(u_model, pattern = ":", replacement = "_")
      u_model = paste(c(u_model, usr_d), collapse = "\n")

      
      mean_vec = colMeans(data_material$ps)
      names(mean_vec) = str_remove_all(colnames(data_material$ps), "pool_")
      names(mean_vec) = str_replace_all(names(mean_vec), pattern = ":", replacement = "_")

      
      fit = lavaan::sem(u_model, sample.cov = sample_cov_r_ap, sample.nobs = nrow(data_material$ps), fixed.x = F
                        , bounds =  "pos.var", sample.mean = mean_vec, ... )
      
    }else{
      #fit model
      sample_cov_r_ap = sample_cov_r
      u_model = eq[str_detect(eq, "=~") == F]

      colnames(sample_cov_r_ap) = str_remove_all(colnames(sample_cov_r), "pool_")
      rownames(sample_cov_r_ap) = str_remove_all(colnames(sample_cov_r), "pool_")
      u_model = str_replace_all(u_model, pattern = ":", replacement = "_")
      u_model = paste(c(u_model, usr_d), collapse = "\n")
      
      mean_vec = colMeans(data_material$ps)
      names(mean_vec) = str_remove_all(colnames(data_material$ps), "pool_")
      names(mean_vec) = str_replace_all(names(mean_vec), pattern = ":", replacement = "_")
      
      fit = lavaan::sem(u_model, sample.cov = sample_cov_r_ap, sample.nobs = nrow(data_material$ps), 
                        fixed.x = F, bounds =  "pos.var",  sample.mean = mean_vec, ...)
    
  }
    
  }else if(npd == F){
    
    u_model = str_replace_all(u_model, pattern = ":", replacement = "_")
    u_model = paste(c(u_model, usr_d), collapse = "\n")
    data_material$ps = cbind(data_material$ps, data)
    fit = lavaan::sem(u_model, data_material$ps, ...)
    
  }

  t1 = Sys.time() - t0
  units(t1) = "secs"
  
  result = new("Silp",raw_model = model,  rapi_model = u_model, time = as.numeric(t1), npd = npd,
      raw_data = data, fa = MD, reliability = data_material$reliability, composite_data = data_material$ps,
      pa = fit)
  
  return(result)
}

