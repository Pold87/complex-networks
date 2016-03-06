# Compute all desired metrics for a given graph and, optionally write
# to LaTeX or plot the degree distribution
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


if (latex) {
    metrics2latex(metrics)
}

if (plot) {
  # Plot
  pdf(file="degree_distribution.pdf")
  plot(degree_distribution(graph),
       xlab="Degree k", ylab="Pr[D=k]",
      pch=1, col=1, type="b")
  dev.off()
}
    return(metrics)
}
