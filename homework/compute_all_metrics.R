### Volker Strobel
## Compute all desired metrics for a given graph and, optionally,
## generate LaTeX output or plot the degree distribution
compute_all_metrics <- function(graph, latex=F, plot=F) {

# Number of nodes
N = vcount(graph)

# Number of links
L = ecount(graph)

# Link density
p = graph.density(graph)

# Mean degree
m_degree = mean(degree(graph))

# Degree variance
var_degree = var(degree(graph))

# Get adjaceny matrix and calculate spectral radius
graph.adj <- get.adjacency(graph, sparse=F)
eigenvalues_adj <- eigen(graph.adj)$values
n_eig_adj <- length(eigenvalues_adj)
eigen_max <- sort(eigenvalues_adj)[n_eig_adj]

# Assortativity (degree correlation)
ass = assortativity.degree(graph)

# Clustering coefficient
c = transitivity(graph, "localaverageundirected")

# Average hopcount
avg_h = average.path.length(graph, direct=FALSE)

# Diameter
diam <- diameter(graph)

# Laplacian matrix
laplacian <- graph.laplacian(graph)
eigenvalues <- eigen(laplacian)$values
n_e <- length(eigenvalues)
mu_second<- sort(eigenvalues)[2]

metrics <- c(N=N, L=L, p=p,
             m_degree=m_degree,
             var_degree=var_degree,
             c=c, ass=ass, avg_h=avg_h,
             eigen_max=eigen_max,
             mu_second=mu_second,
             diameter=diam)

# Format output for LaTex
if (latex) {
    metrics2latex(metrics)
}

### Plot degree distribution
if (plot) {
    dd <- degree_distribution(graph)
  pdf(file="degree_distribution_1.pdf")
  plot(1:(length(dd)-1), dd[2:length(dd)],
      xlab="Degree k", ylab="Pr[D=k]",
      pch=1, col=1, type="b")
  dev.off()
}
    return(metrics)
}
