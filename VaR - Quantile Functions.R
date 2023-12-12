########## Value at Risk and Quantil FUNCIONTS - WITHOUT BUILD-IN R FUNCTIONS  ###########



################# Solved by Summation of PDF ###################

xq_normal_finder = function(cuantil,mu,sigma){
  
  dominio = seq(mu - 5*sigma, mu + 5*sigma, 0.001)
  
  normal<-function(x, mu, sigma){
    
    (1/sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2)) #PDF
    
  }
  
  f = normal(dominio,mu,sigma)
  
  f = f/sum(f)
  
  aux<-cbind(dominio,cumsum(f)) 
  
  return(aux[which(round(aux[,2],5) >= cuantil),1][1])
  
}

# xq_normal_finder(0.95,10,10) - qnorm(0.95,10,10)


quantile_normal_finder = function(xquantile,mu,sigma){
  if (mu + 4*sigma < xquantile | mu - 4*sigma > xquantile) {
    
    return(1)
    
  }
  
  else {
    
    dominio = seq(mu - 5*sigma, mu + 5*sigma, 0.01)
    normal<-function(x, mu, sigma){
      
      (1/sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2)) #PDF
      
    }
    f = normal(dominio,mu,sigma)
    f = f/sum(f)
    aux<-cbind(dominio,cumsum(f)) 
    
    return(aux[which(aux == xquantile),][2])
    
  }
  
}

# quantile_normal_finder(100,100,100) - pnorm(100,100,100)


xq_exponencial_finder = function(cuantil, landa){
  
  dominio = seq(0,1000,0.0001)
  
  exponencial = function(x,landa) {
    
    landa*exp(-landa*x)

  }
  
  f = exponencial(dominio,landa)
  
  f = f/sum(f)
  
  aux<-cbind(dominio,cumsum(f)) 
  
  return(aux[which(round(aux[,2],5) >= cuantil),1][1])
  

  
}

# xq_exponencial_finder(0.95,0.2) - qexp(0.95,0.2)


quantile_exponencial_finder = function(xquantile, landa){
  
  dominio = seq(0,1000,0.0001)
  
  exponencial = function(x,landa) {
    
    landa*exp(-landa*x)
    
  }
  
  f = exponencial(dominio,landa)
  
  f = f/sum(f)
  
  aux<-cbind(dominio,cumsum(f)) 
  
  return(aux[which(aux == xquantile),][2])
  
  
}

# quantile_exponencial_finder(2,0.2) - pexp(2,0.2)


################## Solved by Gradient Descent ##############

quantile_normal_finder_GD = function(xq, mean_value, sd_value) {
  
    
    f = function(q) {  
      
      (sum((1/(sd_value*sqrt(2*pi)))*exp(-0.5*((seq(mean_value - 6*sd_value,xq,1/1000) - mean_value)/(sd_value))^2)/1000) - q)^2
      
    }
    
    derivada = function(x0){
      
      (f(x0 +0.00001) - f(x0))/0.00001
      
    }
    
    b = 0
    
    alpha = 1/(mean_value + 2*sd_value)
    
    for (i in 1:100000) {
      
      if (mean(f(b)) < 0.0001) {
        
        return(b)
        
        # break
        
      }
      
      b = b - alpha*(mean(derivada(b))) 
      
    }
    
  
}

# quantile_normal_finder_GD(0,0,1) - pnorm(0,0,1)


####### Solved by Integration  #######

xq2_normal_finder = function(cuantil, mu, sigma) {
  
  Rectangulos=1000
  base=1/Rectangulos
  
  dominio = seq(mu - 5*sigma, cuantil, base)
  
  normal<-function(x, mu, sigma){
    
    (1/sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2))
    
  }
  
  altura = normal(dominio,mu,sigma)
  
  
  return(sum(abs(altura)*base)/(2*pi))

  
}

# xq2_normal_finder(2332,824,8054) - pnorm(2332,824,8054)

xq2_exponencial_finder = function(xq,landa) {
  
  Rectangulos=10000
  base=1/Rectangulos
  
  dominio = seq(0, xq, base)
  
  
  exponencial = function(x,landa) {
    
    landa*exp(-landa*x)
    
  }
  
  altura = exponencial(dominio,landa)
  
  if (sum(abs(altura)*base) > 1) {
    
    dominio_total = seq(0, xq*exp(1), base)
    altura_total = exponencial(dominio_total,landa)
    integral_total = sum(abs(altura_total)*base)
    
    return(sum(abs(altura)*base)/integral_total)
    
  }
  
  else{
    
    return(sum(abs(altura)*base))
    
  }

}

# xq2_exponencial_finder(2,0.2) - pexp(2,0.2)




