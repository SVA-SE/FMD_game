##'  Create events for the model
##'
##' @template db-param
##' @noRd
create_events <- function(db = NULL) {
    if (!is.null(db)) {
        sql <- "SELECT * FROM events WHERE time=(SELECT MAX(time) + 1 FROM U);"
        return(dbGetQuery(db, sql))
    }

    events_SIR()
}

##' Create local node data for the model.
##'
##' @template db-param
##' @importFrom SimInf distance_matrix
##' @importFrom stats uniroot
##' @noRd
create_ldata <- function(db = NULL) {
    if (!is.null(db)) {
        ldata <- dbGetQuery(db, "SELECT * FROM ldata ORDER BY node;")
        ldata$node <- as.numeric(ldata$node)
        ldata <- t(as.matrix(ldata))
        return(ldata)
    }

    ldata <- matrix(c(
        rep(0.16, nrow(SimInf::nodes)), ## beta
        rep(0.77, nrow(SimInf::nodes)), ## gamma
        seq_len(nrow(SimInf::nodes)),   ## node
        SimInf::nodes$x,                ## x
        SimInf::nodes$y),               ## y
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
##' @importFrom SimInf SimInf_model
##' @noRd
create_model <- function() {
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
        -1, 1,  ## S
         0, 0,  ## I
        -1, 1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    tspan <- 1

    SimInf_model(
        G      = G,
        S      = S,
        E      = E,
        tspan  = tspan,
        events = events_SIR(),
        ldata  = create_ldata(),
        u0     = create_u0())
}

##' Create the initial population.
##' @importFrom SimInf u0_SIR
##' @noRd
create_u0 <- function() {
    ## Load the test-population from SimInf.
    u0 <- u0_SIR()

    ## Add between 1--10 infected individuals to a random node.
    u0$I[sample(seq_len(nrow(u0)), 1)] <- sample(1:10, 1)

    u0
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
##' @importFrom SimInf events_SIR
##' @export
init <- function(dbname = "./model.sqlite") {
    model <- create_model()
    save(model = model, dbname = dbname)
    invisible(NULL)
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
save <- function(model, dbname) {
    ## Open a database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)

    ## Determine if the model is empty, then overwrite the data in the
    ## database, else append the data.
    empty <- identical(dim(model@U), c(0L, 0L))

    ## Save the U state
    if (isTRUE(empty)) {
        U <- as.data.frame(t(model@u0))
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

##' Simulate one time-step of the disease spread model
##'
##' @template dbname-param
##' @importFrom methods validObject
##' @export
##' @useDynLib game.FMD, .registration=TRUE
run <- function(dbname = "./model.sqlite") {
    ## Load model
    model <- load(dbname)

    ## Run one time-step and save the outcome
    validObject(model)
    result <- .Call(game_FMD_run, model, "ssm")

    save(result, dbname = dbname)

    invisible(NULL)
}

##' Load the disease spread model from the database
##'
##' @template dbname-param
##' @importFrom RSQLite dbConnect
##' @importFrom RSQLite dbDisconnect
##' @importFrom RSQLite SQLite
##' @importFrom RSQLite dbGetQuery
##' @importFrom SimInf SIR
##' @noRd
load <- function(dbname) {
    ## Open the database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)

    sql <- "SELECT time FROM U WHERE time=(SELECT max(time) FROM U) LIMIT 1"
    time <- as.numeric(dbGetQuery(con, sql))

    sql <- "SELECT * FROM U WHERE time=:time ORDER BY node;"
    u0 <- dbGetQuery(con, sql, params = c(time = time))

    sql <- "SELECT * FROM events WHERE time=:time;"
    events <- dbGetQuery(con, sql, params = c(time = time + 1))

    ## Create an SIR model. Note that beta and gamma will be replaced
    ## when ldata is injected.
    model <- SIR(u0     = u0,
                 tspan  = time + 1,
                 events = events,
                 beta   = 0,
                 gamma  = 0)

    model@ldata <- create_ldata(con)

    model
}
