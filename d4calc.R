#d4calc.R - Symmetries of the square

D4.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABCD")
  DF[2,] <- c("r","BCDA")
  DF[3,] <- c("s","CDAB")
  DF[4,] <- c("t","DABC")
  DF[5,] <- c("w","ADCB")
  DF[6,] <- c("x","CBAD")
  DF[7,] <- c("y","BADC")
  DF[8,] <- c("z","DCBA")
  return(DF)
}

DF <- D4.makeDataFrame()
D4.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,25),ylim = c(-1,3), asp = 1, axes = FALSE)
  for (i in 0:7) {print(i)
    points(c(0,2,2,0, 0)+3*i,c(0,0,2,2, 0),type = "l")
    lbl <- strsplit(DF[i+1,2],"")[[1]]
    print(lbl)
    text(c(.3,1.75,1.75, .3)+3*i,c(sqrt(3),sqrt(3), 0.25,0.25),lbl)
    text(1+3*i,-0.5,DF[i+1,1])
    segments(c(14,15,19),c(-.1,0,-0.3),
             c(12,17,19),
             c(2,2,sqrt(3)/2+1), 1, lty = 2)
    segments(x0 = 21, y0 = 1, x1 = 23, y1 = 1, lty=2)
  }
}
D4.showConfigs(DF)



D4.apply <- function(a,cfg){
  v <-strsplit(cfg,"")[[1]]
  w <- switch(a,
              "i" = v,
              "r" = c(v[2],v[3],v[4],v[1]),
              "s" = c(v[3],v[4],v[1],v[2]),
              "t" = c(v[4],v[1],v[2],v[3]),
              "w" = c(v[1],v[4],v[3],v[2]),
              "x" = c(v[3],v[2],v[1],v[4]),
              "y" = c(v[2],v[1],v[4],v[3]),
              "z" = c(v[4],v[3],v[2],v[1])
  )
  s <- paste(w,sep="",collapse="") 
  return(s)
}



D4.showSquare <- function(cfg){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
  points(c(0,2,2,0, 0),c(0,0,2,2, 0),type = "l", lwd = 2)
  lbl <- strsplit(cfg,"")[[1]]
  text(c(.18,1.8, 1.8, .18),c(1.9,1.9,0.12, .12),lbl)
}
D4.showSquare("ABCD")




D4.multiply <- function(DF,a,b){
  #Look up the name
  idx <- which(DF$name==b)[1]
  #Find the corresponding configuration
  cfg <- DF$cfg[idx]
  #Apply the group operation to it
  newcfg <- D4.apply(a,cfg)
  # Look up the configuration
  idx <- which(DF$cfg==newcfg)[1]
  return (DF$name[idx])
}
vD4.multiply <- Vectorize(D4.multiply,c("a","b"))
