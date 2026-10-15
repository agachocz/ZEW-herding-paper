
# GENERAL SCHEME

# variables
# x - zew index
# xl - lagged zew index
# yl - lagged macro variable
# dx - zew increase (x_{t-1} - x_{t-T})/T
# dy - macro increase
# pxl - 1 + x_{t-1}
# mxl - 1 - x_{t-1}



# ******************* FUNCTIONS ****************************************

# INCLUDING RECESSION
herding_model_rec <- function(data, ny, params){
  
  base_y <- "+ fy1*y1l + fdy1*dy1l + fy1r*y1l*rec + fdy1r*dy1l*rec"
  base_x <- "+ fdx*dxl + fdxr*dxl*rec"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_yp <- str_flatten( lapply(1:ny, function(x) {str_replace_all(base_y, "1", as.character(x))}), " ")
  base_ym <- str_replace_all(base_yp, "\\+", "\\-")
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x, base_yp)
  f_alt_m <- paste(const, base_xm, base_ym)
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 1, fdxr = -1, rep(params, ny*4))
  nm <- c("fy1", "fdy1", "fy1r", "fdy1r")
  
  names(start) <- c("v", "fdx", "fdxr", 
                    c(outer(nm, 1:ny, function(x,y){str_replace(x, "1", as.character(y))})))
  
  model <- nls(formula_alt, data, start, trace = F, algorithm = "port")
  summary(model)

}


# NO EGZOGENOUS VARIABLES
herding_model_noy <- function(data){
  
  base_x <- "+ fdx*dxl"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x)
  f_alt_m <- paste(const, base_xm)
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 1)
  
  names(start) <- c("v", "fdx")
  
  model <- nls(formula_alt, data, start, trace = T, algorithm = "port")
  print(summary(model))
  
  p <- predict(model, data[,-1])
  res <- data$x-p
  RMSE <- sqrt(mean(res^2))
  print(paste("RMSE:", round(RMSE, 3)))
  
}

# NO EGZOGENOUS VARIABLES, BUT WITH RECESSION
herding_model_noy_rec <- function(data){
  
  base_x <- "+ fdx*dxl + fdxr*dxl*rec"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x, "+ r*rec")
  f_alt_m <- paste(const, base_xm, "- r*rec")
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 1, fdxr = -1, r = -1)
  
  names(start) <- c("v", "fdx", "fdxr", "r")
  
  model <- nls(formula_alt, data, start, trace = T, algorithm = "port")
  print(summary(model))
  
  p <- predict(model, data[,-1])
  res <- data$x-p
  RMSE <- sqrt(mean(res^2))
  print(paste("RMSE:", round(RMSE, 3)))
  
}



herding_model <- function(data, ny, params){
  
  base_y <- "+ fy1*y1l + fdy1*dy1l"
  base_x <- "+ fdx*dxl + fdxr*dxl*rec"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_yp <- str_flatten( lapply(1:ny, function(x) {str_replace_all(base_y, "1", as.character(x))}), " ")
  base_ym <- str_replace_all(base_yp, "\\+", "\\-")
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x, base_yp, "+ r*rec")
  f_alt_m <- paste(const, base_xm, base_ym, "- r*rec" )
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 3, fdxr = -5, rep(params, ny*2), -1)
  nm <- c("fy1", "fdy1")
  
  names(start) <- c("v", "fdx", "fdxr", 
                    c(outer(nm, 1:ny, function(x,y){str_replace(x, "1", as.character(y))})), "r")
  
  model <- nls(formula_alt, data, start, trace = T, algorithm = "port") #algorithm = "port"
  print(summary(model))
  
  p <- predict(model, data[,-1])
  res <- data$x-p
  RMSE <- sqrt(mean(res^2))
  print(paste("RMSE:", round(RMSE, 3)))
  
}



# WITH EGZOGENOUS VARIABLES, BUT NO RECESSION
herding_model_basic <- function(data, ny, params){
  
  base_y <- "+ fy1*y1l + fdy1*dy1l"
  base_x <- "+ fdx*dxl"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_yp <- str_flatten( lapply(1:ny, function(x) {str_replace_all(base_y, "1", as.character(x))}), " ")
  base_ym <- str_replace_all(base_yp, "\\+", "\\-")
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x, base_yp)
  f_alt_m <- paste(const, base_xm, base_ym)
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 1, rep(params, ny*2))
  nm <- c("fy1", "fdy1")
  
  names(start) <- c("v", "fdx", 
                    c(outer(nm, 1:ny, function(x,y){str_replace(x, "1", as.character(y))})))
  
  model <- nls(formula_alt, data, start, trace = F, algorithm = "port")
  print(summary(model))
  
  p <- predict(model, data[,-1])
  res <- data$x-p
  RMSE <- sqrt(mean(res^2))
  print(paste("RMSE:", round(RMSE, 3)))
  
}


# RETURN ADDITIONAL INFO FOR PARAMETERS TESTING

herding_model_return <- function(data, ny, params){
  
  base_y <- "+ fy1*y1l + fdy1*dy1l"
  base_x <- "+ fdx*dxl + fdxr*dxl*rec"
  const <- "1/2*log((1-xm)/(1+xm))"
  
  base_yp <- str_flatten( lapply(1:ny, function(x) {str_replace_all(base_y, "1", as.character(x))}), " ")
  base_ym <- str_replace_all(base_yp, "\\+", "\\-")
  base_xm <- str_replace_all(base_x, "\\+", "\\-")
  
  f_alt <- paste("-", const, base_x, base_yp, "+ r*rec")
  f_alt_m <- paste(const, base_xm, base_ym, "- r*rec" )
  formula_alt <- paste0("x ~ xl + v*( mxl*exp(",f_alt,") - pxl*exp(",f_alt_m,"))")
  
  start <- c(v = 0.05, fdx = 1, fdxr = -1, rep(params, ny*2), -1)
  nm <- c("fy1", "fdy1")
  
  names(start) <- c("v", "fdx", "fdxr", 
                    c(outer(nm, 1:ny, function(x,y){str_replace(x, "1", as.character(y))})), "r")
  
  model <- nls(formula_alt, data, start, trace = T, algorithm = "port")
  summary(model)
  return(model)
  
}

