#' generate_data
#'
#' @param n_obs number of observation
#' @param corr correlation of latent variable
#' @param effect effect of moderator
#' @param ld factor loading of one latent variable to its indicators
#' @param alp reliability of latent variable
#' @param effect_x direction effect of x
#' @param effect_z direction effect of z
#'
#' @return data stimulated from arguement setting
#' @description
#' generate data from Cheung et al.(2021). Note the reliability used here is omega
#'
#' @export
#'
#' @examples
#' n_obs = 100
#' corr = 0.1
#' effect = 0.12
#' ld = c(1,1,1,1)
#' alp = 0.9
#' generate_data(n_obs, corr, effect, ld, alp)




generate_data <- function(n_obs, corr, effect, ld, alp, effect_x = 0.4, effect_z = 0.2 ){
  iv <- MASS::mvrnorm(n_obs, mu = c(0, 0), Sigma = matrix(c(1, corr, corr, 1), 2, 2)) #nx2 mnormal variable
  iv <- cbind(iv, iv[, 1] * iv[, 2]) #a, b, ab, nx3 matrix

  cov_iv <- matrix(c(1, corr, 0, corr, 1, 0, 0, 0, 1 + corr^2), 3, 3)
  coef_iv <- matrix(c(effect_x, effect_z, effect))


  #s.t var(y) = 1
  sd_res <- sqrt(1 - t(coef_iv) %*% cov_iv %*% coef_iv)
  res <- rnorm(n_obs, 0, sd_res)
  dv <- iv %*% coef_iv + res
  lv <- cbind(dv, iv)


  #correct latent variable residual variance by alpha
  error_y <- rnorm(n_obs * 4 , 0, c_res(c(1,1,1,1), alp))
  error_x <- rnorm(n_obs * 4 , 0, c_res(c(1,1,1,1), alp))
  error_z <- rnorm(n_obs * 4 , 0, c_res(ld, alp))


  #y, x, z
  ov <- lv[,1:3] %*% matrix(c(rep(1,4),rep(0,8),
                              rep(0,4),rep(1,4),rep(0,4),
                              rep(0,8),ld ), nrow = 3, byrow = T)

  ov[,1] = ov[,1] + error_y[1:n_obs]
  ov[,2] = ov[,2] + error_y[(1*n_obs +1) : (2*n_obs)]
  ov[,3] = ov[,3] + error_y[(2*n_obs +1) : (3*n_obs)]
  ov[,4] = ov[,4] + error_y[(3*n_obs +1) : (4*n_obs)]

  ov[,5] = ov[,5] + error_x[1:n_obs]
  ov[,6] = ov[,6] + error_x[(1*n_obs +1) : (2*n_obs)]
  ov[,7] = ov[,7] + error_x[(2*n_obs +1) : (3*n_obs)]
  ov[,8] = ov[,8] + error_x[(3*n_obs +1) : (4*n_obs)]

  ov[,9]  = ov[,9]  + error_z[1:n_obs]
  ov[,10] = ov[,10] + error_z[(1*n_obs +1) : (2*n_obs)]
  ov[,11] = ov[,11] + error_z[(2*n_obs +1) : (3*n_obs)]
  ov[,12] = ov[,12] + error_z[(3*n_obs +1) : (4*n_obs)]

  ov <- data.frame(ov)

  colnames(ov) <- c("y1", "y2", "y3", "y4",
                    "x1", "x2", "x3", "x4",
                    "z1", "z2", "z3", "z4")
  return(ov)
  
}




