require(igraph)
require(plyr)

### Load helper functions
source("compute_all_metrics.R")
source("metrics2latex.R")
source("ers_degree_distribution.R")
source("sismodel.R") # TODO rename to sirmodel.R

set.seed(42)

### Read graph (G) from text file
g <- read.graph("7.txt", format="ncol", directed=FALSE)

### Read Net Science graph (G_N) from file 
gn <- read.graph("NetScience.txt", format="ncol", directed=FALSE)

### Compute metrics for G and G_N
g_metrics <- compute_all_metrics(g, latex=T)
gn_metrics <- compute_all_metrics(gn, latex=T)

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
initial_infections <- 1:20
beta = 0.3
sir_model <- init_sirmodel(gn, initial_infections, beta)

T = 30
for (t in 1:T) {
  sir_model <- update_sirmodel(sir_model, beta)
}

plot_sirmodel(sir_model, T)

