
    n <- 80 # number of signal series
    m <- 100 # bandwith resolution
    x <- seq(-15,15, len=m) # sequential 

    y <- matrix(NA, n, m) # creation of matrix filled with NA's
    for (i in 1:n) # loop for generation of randomly & logistically distributed data
    
    #You can whether use normal or logistic distribution or even couple them together for customization

    y[i,] <- (dnorm(x+runif(1,-1,1),mean = runif(1, -5,5), log = F)*runif(m, 1.8,2)/ runif(1, 0.9,3) + dlogis(x+runif(1,-1,1),location = runif(1,-5,5), scale = runif(1,1,1.5), log = FALSE)*runif(m, 1.9,2) / runif(1, 0.9,7) + dlogis(x+runif(1,-1,1),location = runif(1,-5,5), scale = runif(1,1,1.5), log = FALSE)*runif(m, 1.9,2) / runif(1, 0.9,7)) *runif(x, 0.85,1)/runif(1, 1,4)

    #y[i,] <- dlogis(x,location = runif(1,-5,5), scale = runif(1,1,7), log = FALSE)*runif(m, 1.9,2.5) / runif(1, 0.9,3)
   

    par(bg="black") # black color for background
    yrange <- range(c(y, y+n/20)) # range between signals in y direction

    plot(x, x, type="n", axes=F, bg="black", ylim=yrange)

    for (i in n:1) {
       y1 <- c(y[i,] + i/20, 0, 0)
       x1 <- c(x, x[m], x[1])
       polygon(x1,y1,col="black")
       #density(x1,y1,col="black")
       lines(x, y[i,] + i/20, col="white")

    }

