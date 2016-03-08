### Volker Strobel
### Calculate (and optionally plot) the degree distribution for multiple
### graphs
ers_degree_distribution <- function(graphs, plot=F) {

# Apply function 'degree' to every graph
ers_degrees <- sapply(graphs, degree)

# Calculate degree distribution
ers_degree_distribution <- table(ers_degrees) / length(ers_degrees)

# Theoretial degree distribution is binomial distribution
theoretical_distribution <- dbinom(1:16, gn_metrics$N - 1, gn_metrics$p)

if (plot) {

    pdf(file="er_degree_distribution.pdf")
    plot(ers_degree_distribution, xlab="Degree k", ylab="Pr[D=k]",
         pch=1, col=1, type="b")
    points(theoretical_distribution, pch=2, col=2, type="b")
    legend(6, 0.18,
           c("degree distribution of 100 ER graphs", "theoretical degree distribution"),
           pch=1:2,
           col=1:2,
           lty=1)
    dev.off()  
  }

    return(ers_degree_distribution)

}
