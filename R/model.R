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

##' Initialize an FMD model
##'
##' @template dbname-param
##' @importFrom SimInf events_SIR
##' @importFrom SimInf n_nodes
##' @importFrom SimInf SIR
##' @export
init <- function(dbname = "./model.sqlite") {
    ## Create an SIR model.
    model <- SIR(u0     = create_u0(),
                 tspan  = 1,
                 events = events_SIR(),
                 beta   = 0.16,
                 gamma  = 0.077)

    ## Add coordinates for the nodes to ldata.
    model@ldata <- rbind(model@ldata, node = seq_len(n_nodes(model)))
    model@ldata <- rbind(model@ldata, x = SimInf::nodes$x)
    model@ldata <- rbind(model@ldata, y = SimInf::nodes$y)

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
##' @export
run <- function(dbname = "./model.sqlite") {
    ## Load model
    model <- load(dbname)

    ## Run one time-step and save the outcome
    save(SimInf::run(model), dbname = dbname)

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

    sql <- "SELECT * FROM ldata ORDER BY node;"
    ldata <- dbGetQuery(con, sql)
    ldata$node <- as.numeric(ldata$node)

    sql <- "SELECT * FROM events WHERE time=:time;"
    events <- dbGetQuery(con, sql, params = c(time = time + 1))

    ## Create an SIR model. Note that beta and gamma will be replaced
    ## when ldata is injected.
    model <- SIR(u0     = u0,
                 tspan  = time + 1,
                 events = events,
                 beta   = 0,
                 gamma  = 0)

    model@ldata <- t(as.matrix(ldata))

    model
}
