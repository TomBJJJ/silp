parsing_model = function(model){
  eq = str_trim(model)
  eq = str_split(eq, "\n", simplify = T)
  eq = eq[eq != ""]
  eq = str_replace_all(eq, pattern = " ", replacement = "")
  return(eq)
}

exo_moderator = function(l_eq, o_eq, mod_eq, Rel, model, data, center ){
  #find moderator variables
  md_v = ""
  l_v = ""
  e_v = ""

  #store names of moderators
  ps = data.frame("nrow" = rep(0, nrow(data)))
  e_ps = data.frame("nrow" =rep(0, nrow(data)))

  reliability = data.frame("variable" = "reliability")
  variance = data.frame("variable" = "variance")

  # i = 1
  #Assume there are i moderator eq in the model
  for (i in 1:length(mod_eq)) {
    # split two moderator terms
    md_v = str_split(mod_eq[i], "~" ,simplify = T )[2]
    md_v = str_split(mod_eq[i], "\\+" ,simplify = T )
    md_v = md_v[str_detect(md_v, ":") == T ]
    md_v = str_split(md_v, ":", simplify = T)
    md_v = str_remove(md_v, '.*\\*')
    
    lv = str_split(o_eq, "=~", simplify = T)[,1]
    #mean, reliability of two terms
    #j: first & second variables of moderators
    
    # j = 1
    for (j in 1:2) {
      #measurement eq of variable
      p_ind = o_eq[str_detect(o_eq, str_trim(md_v[j])) == TRUE]
      # p_ind = str_remove(p_ind, '.*\\*')
      
      #split measurement eq to lv, ov
      #lv
      if(sum(str_detect(lv, md_v[j])) > 0){
        l_v[j] = str_split(p_ind,"=~", simplify = T)[1]
        
        p_ind = str_split(o_eq[str_detect(o_eq, md_v[j]) == TRUE],"=~", simplify = T)[2]
        #split ov by "+"
        p_ind = str_trim((str_split(p_ind, pattern = "\\+", simplify = T)))
        p_ind = str_remove(p_ind, '.*\\*')
        
        #generate pool mean score of exogenous variable
        ps[paste("pool_",l_v[j], sep = "")] = rowSums(data[p_ind])/length(p_ind)
        
        #calculate reliability
        reliability[paste("pool_", l_v[j], sep = "")] =
          Rel[str_split(o_eq[str_detect(o_eq, md_v[j]) == TRUE],"=~", simplify = T)[1]]
        # v1 = ps[colnames(ps) == paste("pool_", l_v[j], sep = "")]
        
      }else{
        l_v[j] = md_v[j]
        ps[l_v[j]] = data[l_v[j]]
        reliability[l_v[j]] = 1

      }
    }

    #pool-single indicator of moderator variable j
    v1 = ps[colnames(ps) == paste("pool_", l_v[1], sep = "") | colnames(ps) == l_v[1]]
    v2 = ps[colnames(ps) == paste("pool_", l_v[2], sep = "") | colnames(ps) == l_v[2]]

    
    #generate product of moderator variables
    if (center == "double"){
      ps[paste("pool_", l_v[1],":",l_v[2], sep = "")] = (v1 - colMeans(v1))*(v2 - colMeans(v2))
      - colMeans((v1 - colMeans(v1))*(v2 - colMeans(v2)))

    }else if(center == "single"){
      ps[paste("pool_", l_v[1],":",l_v[2], sep = "")] = (v1 - colMeans(v1))*(v2 - colMeans(v2))
    }

    #variance of moderator, product term
    var_data = diag(cov(ps))
    
    for (i in 1:length(l_v)) {
      if(sum(str_detect(lv, l_v[i])) > 0){
        variance[paste("pool_",l_v[i], sep = "")] =  var_data[paste("pool_", l_v[i], sep = "")]
        
      }else {
        variance[l_v[i]] = var_data[l_v[i]]
      }
    }
    

    #catch en-variable regressed by moderator
    en_eq = l_eq[str_detect(l_eq, pattern = l_v[1]) == TRUE]
    en_eq = en_eq[str_detect(en_eq, pattern = l_v[2]) == TRUE]

    
    #l number of eq of lv regressed contain moderation effect
    for (l in 1:length(en_eq)) {
      #name of en-variable
      e_v = str_split(en_eq[l], "~", simplify = T)[1]
      #ov of lv
      lv = str_split(o_eq, "=~", simplify = T)[,1]
      pool_endo = lv[str_detect(lv, e_v) == TRUE]
      
      if(sum(str_detect(lv, e_v)) > 0){
        pool_endo = str_split(o_eq[str_detect(o_eq, e_v) == TRUE],"=~" ,simplify = T) [2]
        pool_endo = str_trim((str_split(pool_endo, pattern = "\\+", simplify = T)))
        pool_endo = str_remove(pool_endo, '.*\\*')
        
        #en-variable pool score
        ps[paste("pool_",e_v, sep = "")] = rowSums(data[pool_endo])/length(pool_endo)
  
        #reliability
        reliability[paste("pool_" ,e_v, sep = "")] = Rel[e_v]
  
        #variance of en-var
        var_data = diag(cov(ps))
        variance[paste("pool_", e_v, sep = "")] = var_data[paste("pool_", e_v,sep = "")]
        
      }else{
        ps[e_v] = data[e_v]
        reliability[e_v] = 1
        var_data = diag(cov(ps))
        variance[e_v] = var_data[e_v]
      }
    }
    #end ith moderator eq
  }
  
  # l = 3
  #make all lv to ov by RAPI
  for (l in 1:length(l_eq)) {
    #name of en-variable
    e_v = str_split(l_eq[l], "~", simplify = T)[1]
    lv = str_split(o_eq, "=~", simplify = T)[,1]
    #ov of lv
    pool_endo = o_eq[str_detect(o_eq, e_v) == TRUE]

    if(sum(str_detect(lv, e_v)) > 0){
      pool_endo = str_split(o_eq[str_detect(o_eq, e_v) == TRUE],"=~" ,simplify = T) [2]
      pool_endo = str_trim((str_split(pool_endo, pattern = "\\+", simplify = T)))
      pool_endo = str_remove(pool_endo, '.*\\*')
      
      #en-variable pool score
      ps[paste("pool_",e_v, sep = "")] = rowSums(data[pool_endo])/length(pool_endo)
      
      #reliability
      reliability[paste("pool_" ,e_v, sep = "")] = Rel[e_v]
      
      #variance of en-var
      var_data = diag(cov(ps))
      variance[paste("pool_", e_v, sep = "")] = var_data[paste("pool_", e_v,sep = "")]
      
    }else{
      ps[e_v] = data[e_v]
      reliability[e_v] = 1
      var_data = diag(cov(ps))
      variance[e_v] = var_data[e_v]
    }
    
    #regressor
    e_v = str_split(l_eq[l], "~", simplify = T)[2]
    e_v = str_split(e_v ,"\\+", simplify = T)
    e_v = e_v[str_detect(e_v, ":") == F]
    e_v = str_remove(e_v, '.*\\*')
    
    #regression contain 1
    e_v = e_v[e_v != "1"]
    
    #regression contain only 1
    
    if(length(e_v) != 0){
      for (i in 1:length(e_v)) {
        if(sum(str_detect(lv, e_v[i])) > 0){
          pool_endo = str_split(o_eq[str_detect(o_eq, e_v[i]) == TRUE],"=~" ,simplify = T) [2]
          pool_endo = str_trim((str_split(pool_endo, pattern = "\\+", simplify = T)))
          pool_endo = str_remove(pool_endo, '.*\\*')
          #en-variable pool score
          ps[paste("pool_",e_v[i], sep = "")] = rowSums(data[pool_endo])/length(pool_endo)
          
          #reliability
          reliability[paste("pool_" ,e_v[i], sep = "")] = Rel[e_v[i]]
          
          #variance of en-var
          var_data = diag(cov(ps))
          variance[paste("pool_", e_v[i], sep = "")] = var_data[paste("pool_", e_v[i],sep = "")]
          
        }else{
          ps[e_v[i]] = data[,e_v[i]]
          reliability[e_v[i]] = 1
          var_data = diag(cov(ps))
          variance[e_v[i]] = var_data[e_v[i]]
        }
      }
    }
  }
  
  #delete 1th column
  ps = ps[,2:ncol(ps)]
  reliability = reliability[,2:ncol(reliability)]
  variance = variance[,2:ncol(variance)]

  return(list("ps" = ps, "reliability" = reliability, "variance" = variance))
}

indicator_update = function(eq , data_material, o_eq){

  u_model = eq
  lv = str_split(o_eq, "=~", simplify = T)[,1]
  reg = eq[str_detect(eq, "~") == T & str_detect(eq, "~~") == F & str_detect(eq, "=~") == F]
  
  # i = 1
  #deal  ov of moderator
  
  for (i in 1:ncol(data_material$ps)) {
    #indicator of LV
    v_name = str_remove(colnames(data_material$ps), "pool_")[i]
    
    #create product ob if : exists
    if (str_detect(v_name, ":") ==TRUE){
      if(sum(str_detect(lv, str_split_1(v_name, ":")[1])) > 0 | 
         sum(str_detect(lv, str_split_1(v_name, ":")[2])) > 0){
        u_model = c(u_model, paste(v_name, "=~ 1*", paste("pool_", v_name, sep = "")))
      }
    
      #product indicator
      # u_model = c(u_model, paste(v_name, "=~ 1*", paste("pool_", v_name, sep = "")))
    
      #non-product ov
    }else{
      #ensure v_name is LV
      if(sum(str_detect(lv, v_name)) > 0){
        u_model[str_detect(u_model, "=~") == TRUE & str_detect(u_model, v_name) == TRUE] =
        paste(v_name, "=~ 1*", paste("pool_", v_name, sep = ""))
      }else{
      }
    }
    
    for (j in 1:length(reg)) {
      tempt = str_split_1(str_split_1(reg[j], "~")[2], "\\+")
      tempt_2 = str_split_1(reg[j], "~")[2]
      if(v_name == str_split(reg[j], "~", simplify = T)[1] & sum(str_detect(tempt, "1")) > 0){
        u_model = c(u_model, paste(paste("pool_", v_name, sep = ""), "~ 1" ))
        
        if(sum(str_detect(tempt_2, "\\+")) == 0){
          u_model = u_model[-which(u_model == reg[j])]
        }else{
          u_model[which(u_model == reg[j])] = str_remove(u_model[which(u_model == reg[j])], "\\+1") 
          u_model[which(u_model == reg[j])] = str_remove(u_model[which(u_model == reg[j])], "1\\+")  
        }
      }
    }

    }
  
  return(u_model)
}

variance_update = function(u_model , data_material, o_eq){
  #all pool data
  for (i in 1:ncol(data_material$ps)) {
    
    v_name = colnames(data_material$ps)[i]
    lv = str_split(o_eq, "=~", simplify = T)[,1]
    #detect whether it's product or not
    if(str_detect(v_name, ":") == FALSE){
      u_model = append(u_model, paste(v_name, " ~~ ev_" ,v_name, " * ", v_name, sep = ""))
      u_model = append(u_model, paste("ev_" ,v_name, " == ",
                                      data_material$variance[v_name]*(1- data_material$reliability[v_name]),
                                      sep = ""))
      

    }else{
      u_model = append(u_model, paste(v_name, " ~~ ev_" ,v_name, " * ", v_name, sep = ""))
      v = c()
      # u_model[str_detect(u_model, "=~") == TRUE & str_detect(u_model, v_name) == TRUE] =
      #   paste(v_name, "=~ 1*", paste("pool_", v_name, sep = ""))
      
      
      tempt = str_remove(v_name, "pool_")
      v = str_split(tempt, ":", simplify = T)[1]
      if(sum(str_detect(lv, v)) > 0){
        v1 = paste("pool_", v, sep = "")
      }else{
        v1 = v
      }
      
      v = str_split(tempt, ":", simplify = T)[2]
      if(sum(str_detect(lv, v)) > 0){
        v2 = paste("pool_", v, sep = "")
      }else{
        v2 = v
      }
      

      u_model = append(u_model, paste("ev_" ,v_name, " == " ,
                                      (data_material$variance[v1]*data_material$reliability[v1]*
                                         data_material$variance[v2]*(1 - data_material$reliability[v2])) +
                                        (data_material$variance[v2]*data_material$reliability[v2]*
                                           data_material$variance[v1]*(1 - data_material$reliability[v1])) +
                                        (data_material$variance[v1]*(1 - data_material$reliability[v1])*
                                           data_material$variance[v2]*(1 - data_material$reliability[v2])),
                                        sep = ""))

    }
    #delete repeat element
    u_model = u_model[!duplicated(u_model)]
  }
  return(u_model)
}

c_res = function(ld, alp){
  #cov = factor loading 相乘
  #var = "" + var(residual)
  ld = as.matrix(ld)
  res = (sum(ld)**2 - (sum(ld)**2)*alp)/4/alp

  #check
  # sum(ld)**2 / (sum(ld)**2 + 4*res)

  return(sqrt(res))
}

rel_correction = function(data_material, o_eq){
  #all pool data
  rel_cor = data.frame("1" = 0)
  lv = str_split(o_eq, "=~", simplify = T)[,1]
  for (i in 1:ncol(data_material$ps)) {
    v_name = colnames(data_material$ps)[i]


    #detect whether it's product or not
    if(str_detect(v_name, ":") == FALSE){
      rel_cor[v_name] = data_material$variance[v_name]*(1 - data_material$reliability[v_name] )

    }else{
      tempt = str_remove(v_name, "pool_")
      v = str_split(tempt, ":", simplify = T)[1]
      if(sum(str_detect(lv, v)) > 0){
        v1 = paste("pool_", v, sep = "")
      }else{
        v1 = v
      }
      
      v = str_split(tempt, ":", simplify = T)[2]
      if(sum(str_detect(lv, v)) > 0){
        v2 = paste("pool_", v, sep = "")
      }else{
        v2 = v
      }
      
      
      
      # v1 = str_split(v_name, ":", simplify = T)[1]
      # v2 = str_split(v_name, ":", simplify = T)[2]

      rel_cor[v_name] = (data_material$variance[v1]*data_material$reliability[v1]*data_material$variance[v2]*
                        (1 - data_material$reliability[v2])) +
                    (data_material$variance[v2]*data_material$reliability[v2]*
                       data_material$variance[v1]*(1 - data_material$reliability[v1])) +
                    (data_material$variance[v1]*(1 - data_material$reliability[v1])*
                       data_material$variance[v2]*(1 - data_material$reliability[v2]))

    }
    #delete repeat element
    rel_cor = rel_cor[!duplicated(rel_cor)]
  }
  return((rel_cor[,-1]))
}






