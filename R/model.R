##'  Create events for the model
##'
##' @template db-param
##' @importFrom SimInf events_SIR
##' @noRd
create_events <- function(db = NULL, index) {
    if (!is.null(db)) {
        sql <- "SELECT * FROM events WHERE time=(SELECT MAX(time) + 1 FROM U);"
        return(dbGetQuery(db, sql))
    }

    events <- events_SIR()
    if (length(index) < 1600)
        events <- events[0, ]
    events
}

##' Create local node data for the model.
##'
##' @template db-param
##' @importFrom SimInf distance_matrix
##' @importFrom stats uniroot
##' @noRd
create_ldata <- function(db = NULL, beta, gamma, index) {
    if (!is.null(db)) {
        return(dbGetQuery(db, "SELECT * FROM ldata ORDER BY node;"))
    }

    ldata <- matrix(c(
        rep(beta, length(index)),  ## beta
        rep(gamma, length(index)), ## gamma
        seq_len(length(index)),    ## node
        SimInf::nodes$x[index],    ## x
        SimInf::nodes$y[index]),   ## y
        nrow = 5,
        byrow = TRUE,
        dimnames = list(c("beta", "gamma", "node", "x", "y"), NULL))

    ## Determine the cutoff for the distance (in meters) for when to
    ## inlude the interaction from neighbours.
    k <- 1.76
    cutoff <- uniroot(f = function(d, k) {
        K_d_ij(d, k) - 0.01
    }, interval = c(0, 1e5), k = k)$root

    distance <- distance_matrix(ldata["x", ], ldata["y", ], cutoff = cutoff)

    ## Now we need to re-structure the distance matrix to local data
    ## available to each node. The reason for this is that every node
    ## needs to know the index (node identifier) and the spatial
    ## coupling to all its neigboring nodes. We keep the information
    ## for each node in a vector with two values for every neighbour:
    ## (index, distance), (index, distance), ..., (-1, 0). Each vector
    ## is stored as one column in the ldata matrix. For this
    ## re-structuring, we will use an internal function in SimInf.
    rownames_ldata <- rownames(ldata)
    ldata <- .Call(SimInf:::SimInf_ldata_sp, ldata, distance, 1L)
    n_neighbours <- (nrow(ldata) - length(rownames_ldata)) / 2
    rownames(ldata) <- c(
        rownames_ldata,
        paste0(c("index", "coupling"), rep(seq_len(n_neighbours), each = 2)))

    ## Recalculate the distance to a coupling.
    for (j in seq_len(ncol(ldata))) {
        i <- length(rownames_ldata) + 1
        repeat {
            if (ldata[i, j] < 0)
                break
            ldata[i + 1, j] <- K_d_ij(ldata[i + 1, j], k)
            i <- i + 2
        }
    }

    ldata
}

##' Create a SimInf_model object.
##'
##' @template db-param
##' @importFrom SimInf SimInf_model
##' @noRd
create_model <- function(db = NULL, beta, gamma, index) {
    transitions <- c("S -> beta*S*I/(S+I+R) -> I",
                     "I -> gamma*I -> R")
    compartments <- c("S", "I", "R")

    E <- matrix(c(
        1, 0, 0, 1,  ## S
        0, 1, 0, 1,  ## I
        0, 0, 1, 1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    G <- matrix(c(
        1, 1,  ## S -> beta*S*I/(S+I+R) -> I
        1, 1), ## I -> gamma*I -> R
        nrow = length(transitions),
        byrow = TRUE,
        dimnames = list(transitions, NULL))

    S <- matrix(c(
        -1,  0,  ## S
         1, -1,  ## I
         0,  1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    SimInf_model(
        G      = G,
        S      = S,
        E      = E,
        tspan  = create_tspan(db),
        events = create_events(db, index),
        ldata  = create_ldata(db, beta, gamma, index),
        u0     = create_u0(db, index),
        v0     = create_v0(db, index))
}

##' Create tspan for the model
##'
##' @template db-param
##' @noRd
create_tspan <- function(db = NULL) {
    if (!is.null(db)) {
        sql <- "SELECT MAX(time) + 1 FROM U;"
        return(as.numeric(dbGetQuery(db, sql)))
    }

    1
}

##' Create the initial population
##'
##' @template db-param
##' @importFrom SimInf u0_SIR
##' @noRd
create_u0 <- function(db = NULL, index) {
    if (!is.null(db)) {
        sql <- "SELECT * FROM U WHERE time=(SELECT max(time) FROM U) ORDER BY node;"
        u0 <- dbGetQuery(db, sql)
        u0 <- u0[, -match(c("node", "time", "I_coupling"), colnames(u0)), drop = FALSE]
        return(u0)
    }

    ## Load the test-population from SimInf.
    u0 <- u0_SIR()[index, ]

    ## Add between 1--10 infected individuals to a random node.
    u0$I[sample(seq_len(nrow(u0)), 1)] <- sample(1:10, 1)

    u0
}

create_v0 <- function(db = NULL, index) {
    if (!is.null(db)) {
        sql <- "SELECT I_coupling FROM U WHERE time=(SELECT max(time) FROM U) ORDER BY node;"
        v0 <- dbGetQuery(db, sql)
        return(v0)
    }

    data.frame(I_coupling = rep(0, length(index)))
}

##' Power-law kernel for the interaction between populations
##'
##' @param d distance in meters between populations
##' @param k spatial kernel scaling parameter.
##' @noRd
K_d_ij <- function(d, k) {
    1 / (1 + (d/1000)^k)
}

##' Initialize an FMD model
##'
##' @template dbname-param
##' @param beta the transmission rate parameter.
##' @param gamma the recovery rate parameter.
##' @param n the number of nodes to include in the population.
##' @export
init <- function(dbname = "./model.sqlite", beta = 0.005, gamma = 0.077, n = 1600) {
    index <- sample(seq_len(1600), n)
    model <- create_model(NULL, beta, gamma, index)
    dbWriteModel(model = model, dbname = dbname)
    invisible(NULL)
}

##' Simulate one time-step of the disease spread model
##'
##' @template dbname-param
##' @importFrom methods validObject
##' @export
##' @useDynLib game.FMD, .registration=TRUE
run <- function(dbname = "./model.sqlite") {
    ## Load model
    model <- dbReadModel(dbname)

    ## Run one time-step and save the outcome
    validObject(model)
    result <- .Call(game_FMD_run, model, "ssm")

    dbWriteModel(result, dbname = dbname)

    invisible(NULL)
}

##' Load the disease spread model from the database
##'
##' @template dbname-param
##' @importFrom RSQLite dbConnect
##' @importFrom RSQLite dbDisconnect
##' @importFrom RSQLite SQLite
##' @importFrom RSQLite dbGetQuery
##' @export
dbReadModel <- function(dbname = "./model.sqlite") {
    ## Open the database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)
    create_model(con)
}

##' Append or overwrite data in model.sqlite
##'
##' @template dbname-param
##' @importFrom RSQLite dbConnect
##' @importFrom RSQLite dbDisconnect
##' @importFrom RSQLite dbWriteTable
##' @importFrom RSQLite SQLite
##' @importFrom SimInf trajectory
##' @importFrom SimInf events
##' @noRd
dbWriteModel <- function(model, dbname) {
    ## Open a database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)

    ## Determine if the model is empty, then overwrite the data in the
    ## database, else append the data.
    empty <- identical(dim(model@U), c(0L, 0L))

    ## Save the U state
    if (isTRUE(empty)) {
        U <- as.data.frame(t(model@u0))
        U <- cbind(U, as.data.frame(t(model@v0)))
        U <- cbind(node = seq_len(nrow(U)), time = 0L, U)
        dbWriteTable(con, "U", U, overwrite = TRUE)
    } else {
        U <- trajectory(model)
        dbWriteTable(con, "U", U, append = TRUE)
    }

    ## Save ldata
    if (isTRUE(empty)) {
        ldata <- as.data.frame(t(model@ldata))
        ldata$node <- as.integer(ldata$node)
        dbWriteTable(con, "ldata", ldata, overwrite = TRUE)
    }

    ## Save events
    if (isTRUE(empty)) {
        dbWriteTable(con, "events", as.data.frame(events(model)), overwrite = TRUE)
    }

    invisible(NULL)
}
