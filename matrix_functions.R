xformMatrix <- function(m,v) {
  out = c(0,0,0,0)
  for (i in 1:4) {
    for (j in 1:4) {
      out[j] = out[j] + m[4*(i-1) + j] * v[i]
    }
  }
  return(out)
}


projectVertex <- function(v, model, view, projection, resolution) {
  p = xformMatrix(projection,
                  xformMatrix(view, 
                              xformMatrix(model, c(v[1],v[2],v[3],1.0))))
  for (i in 1:4) {
    p[i] = p[i]/p[4]
  }
  if ((p[3]+1)*0.5 > 1) {
    p[1] = -1
    p[2] = -1
  } else {
    p[1] = 0.5 * resolution[1] * (1.0+p[1])
    p[2] = 0.5 * resolution[2] * (1.0+p[2])
  }
  return(c(p[1], p[2]))
}

generate_random_sample_data <- function(num_points){
  n = floor(num_points / 3)
  x<-rnorm(n, mean = 10, sd = 2)
  y<-rnorm(n, mean = 100, sd = 25)
  z<-rnorm(n, mean = 50, sd = 50)
  ind <- c(rep("group1",n))
  df1 = data.frame(x,y,z, ind)
  x<-rnorm(n, mean = 20, sd = 3)
  y<-rnorm(n, mean = 50, sd = 25)
  z<-rnorm(n, mean = 100, sd = 10)
  ind <- c(rep("group2",n))
  df2 = data.frame(x,y,z, ind)
  df <-rbind(df1, df2)
  x<-rnorm(n, mean = 30, sd = 50)
  y<-rnorm(n, mean = 100, sd = 15)
  z<-rnorm(n, mean = 5, sd = 10)
  ind <- c(rep("group3",n))
  df3 = data.frame(x,y,z, ind)
  df <-rbind(df, df3)
  
  df$group1 <- sample(c(1, 2, 3,4,5), n*3, replace = TRUE)
  df$group2 <- sample(c('A', 'B', 'C', 'D','E','F','G', 'H', 'I','J','K','L','M','N','O','P','Q','R','S','T','U'), n*3, replace = TRUE)
  df$group3 <- sample(c('athff', 'rhe', 'erjgel', 'ejg','ejgle','epgt','bb', 'qljrqlr', 'mbndkbn', 'gag', 'ttpgi', 'egiep'), n*3, replace = TRUE)
  df$group4 <- sample(rnorm(1000, mean = 5, sd = 3), n*3, replace = TRUE)
  df$pk = c(1:(n*3))
  
  return(df)
}
