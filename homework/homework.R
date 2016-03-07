# Volker Strobel
# Student number: 4524187

require(igraph)
require(plyr)

### Load helper functions
source("compute_all_metrics.R")
source("metrics2latex.R")
source("ers_degree_distribution.R")
source("sirmodel.R") 

set.seed(42) # Set random seed for reproducability

### Read graph (G) from text file
g <- read.graph("7.txt", format="ncol", directed=FALSE)

### Read Net Science graph (G_N) from file 
gn <- read.graph("NetScience.txt", format="ncol", directed=FALSE)

### Compute metrics for G and G_N
g_metrics <- compute_all_metrics(g, latex=T)
gn_metrics <- compute_all_metrics(gn, latex=T)

### Find out power exponent gamma and plot fitting curve
pfit <- power.law.fit(degree(g), implementation="R.mle")
h <- table(degree(g))
hn <- h / sum(table(degree(g)))
k = 1:max(degree(g))
y = k ^ (- pfit@coef)
y <- y / sum(y)

pdf("fitted_curve_normalized.pdf")
plot(k, y, pch=2, col=2, type="b", xlim=c(1, 34), xlab="Degree k", ylab="Pr[D=k]")
lines(hn, pch=1, col=1, type="b",  ylim=0:1, xlim=c(1, 34))
legend(15, 1.00,
       c("degree distribution of graph G", "fitting curve (power law distribution)"),
       pch=1:2,
       col=1:2,
       lty=1)
dev.off()


### Generate 100 Erdos-Renyi (ER) graphs with the same number N of nodes
### and the same link density p as the network G
graphs <- replicate(100, sample_gnp(g_metrics["N"], g_metrics["p"]), simplify=F)

### Apply function 'compute_all_metrics' to each element of 'graphs'
er_metrics <- ldply(graphs, compute_all_metrics)
er_means <- colMeans(er_metrics)
metrics2latex(er_means)

### Create SIR model
## An SIR model consists of
## - a graph
## - a set of infected states
## - a set of susceptible states
## - a set of recovered states
initial_infections <- sample(1:vcount(g), 20, replace=F)
beta = 0.3 # infection probability

sir_model.g <- init_sirmodel(g, initial_infections, beta)
sir_model.gn <- init_sirmodel(gn, initial_infections, beta)
sir_model.er <- init_sirmodel(gn, initial_infections, beta)


T = 2  # Time steps
for (t in 1:T) {
    # Update model for G
    sir_model.g <- update_sirmodel(sir_model.g, beta)

    # Update model for G_N
    #sir_model.gn <- update_sirmodel(sir_model.gn, beta)

    # Create new ER graph and update ER model
    #er <- sample_gnp(g_metrics["N"], g_metrics["p"])
    #sir_model.er$graph <- er
    #sir_model.er <- update_sirmodel(sir_model.er, beta)
}

## # Plot models
## plot_sirmodel(sir_model.g, T)
## plot_sirmodel(sir_model.gn, T)
## plot_sirmodel(sir_model.er, T)

