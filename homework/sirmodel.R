set.seed(42)

### Initialize a SIR model using
### - a graph
### - a list of initial infections 
### - an infection probability beta
### returns: the constructed SIR model

init_sirmodel <- function(graph, initial_infections, beta) {

    sir_model <- list(graph=graph,
                      infected=initial_infections,
                      susceptible=setdiff(1:vcount(g), initial_infections),
                      recovered=NULL,
                      beta=beta,
                      ## for keeping track the developments over time:
                      num_infected=length(initial_infections),
                      num_susceptible=(vcount(g)-length(initial_infections)),
                      num_recovered=0) 

    return(sir_model)

}

### Update an existing SIR model, that is, spread the disease and let
### infected states recover
update_sirmodel <- function(sir_model, beta) {

    newly_infected <- NULL
    
    ## Spread the disease
    for(node in sir_model$infected) {

        ## Get neighbors
            neighs <- neighbors(sir_model$graph, node)

        ## Iterate over neighbors
            for(ne in neighs) {

                ## Infect with probability beta
                if (beta > runif(1) & ne %in% sir_model$susceptible) {
                    newly_infected <- union(newly_infected, ne)
                }
            }
    }

    ## Recover all infected nodes ...
    recovered <- union(sir_model$recovered, sir_model$infected)

    ## ... And infect new nodes
    infected <- newly_infected

    ## Calculate new susceptibles
    susceptible <- setdiff(sir_model$susceptible, recovered)
    susceptible <- setdiff(susceptible, infected)

    num_infected <- c(sir_model$num_infected, length(infected))
    num_susceptible <- c(sir_model$num_susceptible, length(susceptible))
    num_recovered <- c(sir_model$num_recovered, length(recovered))

    return(list(graph=sir_model$graph,
                susceptible=susceptible,
                infected=infected,
                recovered=recovered,
                num_infected=num_infected,
                num_susceptible=num_susceptible,
                num_recovered=num_recovered))
}

plot_sirmodel <- function(sir_model, T){

    plot(0:T, sir_model$num_infected, pch=1, col=1, type="b",
         ylim=range(0, vcount(sir_model$graph)))
    par(new=T)
    points(0:T, sir_model$num_susceptible, pch=2, col=2, type="b",
           ylim=range(0, vcount(sir_model$graph)))
    par(new=T)
    points(0:T, sir_model$num_recovered, pch=3, col=3, type="b",
           ylim=range(vcount(sir_model$graph)))
}
