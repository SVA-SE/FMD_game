##' Generate a model from mparse
##'
##' @importFrom SimInf mparse
##' @importFrom utils read.csv
##' @noRd
mparse_model <- function() {
    ## Get ldata from package csv file
    path <- system.file("extdata/ldata.csv", package = "game.FMD")
    ldata <- read.csv(path)

    ## Set the compartments in the model
    compartments <- c("S", "I", "R")

    ## Set the transitions in the model
    transitions <- c("S -> S * I * beta / (S + I + R) -> I",
                     "I -> I * gamma -> S")

    ## Create the model
    mparse(transitions = transitions,
           compartments = compartments,
           ldata = ldata,
           u0 = create_u0(),
           tspan = 1)
}
