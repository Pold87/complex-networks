### Volker Strobel
### Fit power law distribution to data and return power exponent gamma
power_law_fit<- function(d, plot=F) {

    h <- table(d) # Get count of each degree
    degrees <- as.numeric(names(h)) # Extract degrees
    k = 1:max(degrees) # Create degree vector
    probs <- unname(h / sum(table(degree(g)))) # Extract degree distribution

    myfit <- lm(log(probs) ~ log(degrees)) # Fit linear model (with log-log transformation) 
    preds <- -myfit$coefficients[[1]] * k ** myfit$coefficients[[2]] # Make predictions

    ### Display coefficients
    cat(sprintf("Intercept %f\n", - myfit$coefficients[[1]])) 
    cat(sprintf("Slope %f\n", myfit$coefficients[[2]]))

    ### Save to pdf
    if (plot) {

        pdf("fitted_curve_normalized.pdf")
        plot(preds, pch=2, col=2, type="b", xlab="Degree k", ylab="Pr[D=k]")
        axis(side = 2, at = seq(0, 1, 0.1))
        lines(degrees, probs, pch=1, col=1, type="b")
        legend(15, 0.6,
               c("degree distribution of graph G", "fitting curve (power law distribution)"),
               pch=1:2, col=1:2, lty=1)
        dev.off()
    }

    ### Return power exponent gamma
    gamma <- myfit$coefficients[[2]]
    return(gamma)

}
