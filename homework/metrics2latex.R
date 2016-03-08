### Volker Strobel
### Output metrics values for LaTeX
metrics2latex <- function(metrics) {

   cat("\\begin{itemize}\n")
   cat(sprintf("\\item Number of nodes $N$: %d\n", metrics["N"]))
   cat(sprintf("\\item Number of links $L$: %f\n", metrics["L"]))
   cat(sprintf("\\item Link density $p$: %f\n", metrics["p"]))
   cat(sprintf("\\item Average degree $E[D]$: %f\n", metrics["m_degree"]))
   cat(sprintf("\\item Degree variance $Var[D]$: %f\n", metrics["var_degree"]))
   cat(sprintf("\\item Clustering coefficient: %f\n", metrics["c"]))
   cat(sprintf("\\item Assortativity: %f\n", metrics["ass"]))
   cat(sprintf("\\item Average hopcount $E[H]$: %f\n", metrics["avg_h"]))
   cat(sprintf("\\item Spectral radius $lambda_1$: %f\n", metrics["eigen_max"]))
   cat(sprintf("\\item Algebraic connectivity $mu_{N-1}$: %f\n", metrics["mu_second"]))
   cat(sprintf("\\item Diameter $H_{max}$: %f\n", metrics["diameter"]))
   cat("\\end{itemize}\n")

}
