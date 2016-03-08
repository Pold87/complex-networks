# Volker Strobel
# Student number: 4524187

require(igraph)
require(plyr)

### Load helper functions
source("compute_all_metrics.R")
source("metrics2latex.R")
source("ers_degree_distribution.R")
source("sirmodel.R")
source("power_law_fit.R") 

set.seed(42) # Set random seed for reproducability

### Read graph (G) from text file
g <- read.graph("7.txt", format="ncol", directed=FALSE)

### Read Net Science graph (G_N) from file 
gn <- read.graph("NetScience.txt", format="ncol", directed=FALSE)

### Compute metrics for G and G_N
g_metrics <- compute_all_metrics(g, latex=T, plot=F)
gn_metrics <- compute_all_metrics(gn, latex=T, plot=F)

### Find out power exponent gamma and, optionally plot fitting curve
gamma <- power_law_fit(degree(g), plot=F)

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
## - a set of infected nodes
## - a set of susceptible nodes
## - a set of resistant nodes

R = 1 # Repetitions
T = 100  # Time steps

g.resistants <- NULL
gn.resistants <- NULL
er.resistants <- NULL

for (r in 1:R) {

    initial_infections <- sample(1:vcount(g), 20, replace=F)
    beta = 0.3 # infection probability
    
    sir_model.g <- init_sirmodel(g, initial_infections, beta)
    sir_model.gn <- init_sirmodel(gn, initial_infections, beta)
    sir_model.er <- init_sirmodel(sample_gnp(g_metrics["N"], g_metrics["p"]),
                                  initial_infections, beta)

    for (t in 1:T) {
        ## Update model for G
        sir_model.g <- update_sirmodel(sir_model.g, beta)
        ## Update model for G_N
        sir_model.gn <- update_sirmodel(sir_model.gn, beta)
        ## Create new ER graph and update ER model
        sir_model.er$graph <- sample_gnp(g_metrics["N"], g_metrics["p"])
        sir_model.er <- update_sirmodel(sir_model.er, beta)
    }

    ## Store number of resistants
    g.resistants <- c(g.resistants, tail(sir_model.g$num_resistant, 1))
    gn.resistants <- c(gn.resistants, tail(sir_model.gn$num_resistant, 1))
    er.resistants <- c(er.resistants, tail(sir_model.er$num_resistant, 1))

}

cat(sprintf("Graph G E[n_R_inf] = %f\n", mean(g.resistants)))
cat(sprintf("Graph G_N E[n_R_inf] = %f\n", mean(gn.resistants)))
cat(sprintf("Graph ER E[n_R_inf] = %f\n", mean(er.resistants)))

## # Plot models
## pdf("G_SIR.pdf")
## plot_sirmodel(sir_model.g, T)
## dev.off()

## pdf("GN_SIR.pdf")
## plot_sirmodel(sir_model.gn, T)
## dev.off()

## pdf("ER_SIR.pdf")
## plot_sirmodel(sir_model.er, T)
## dev.off()

