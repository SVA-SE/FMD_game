##' Initialize an FMD model
##'
##' @template dbname-param
##' @importFrom SimInf events_SIR
##' @importFrom SimInf u0_SIR
##' @importFrom SimInf SIR
##' @export
init <- function(dbname = "./model.sqlite") {
    ## Create the initial population.
    u0 <- u0_SIR()

    ## Add one infected individual to the first node.
    u0$I[1] <- 1

    ## Create an SIR model.
    model <- SIR(u0     = u0,
                 tspan  = 1,
                 events = events_SIR(),
                 beta   = 0.16,
                 gamma  = 0.01)

    ## Add coordinates for the nodes to ldata.
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
##' @importFrom SimInf events
##' @noRd
save <- function(model, dbname) {
    ## Open a database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)

    ## Determine if the model is empty, then overwrite the data in the
    ## database, else append the data.
    empty <- SimInf:::is_trajectory_empty(model)

    ## Save the U state
    if (isTRUE(empty)) {
        U <- as.data.frame(t(model@u0))
        U <- cbind(node = seq_len(nrow(U)), time = 0L, U)
        dbWriteTable(con, "U", U, overwrite = TRUE)
    } else {
        stop("Not implemented")
    }

    ## Save ldata
    ldata <- as.data.frame(t(model@ldata))
    if (isTRUE(empty)) {
        dbWriteTable(con, "ldata", ldata, overwrite = TRUE)
    } else {
        dbWriteTable(con, "ldata", ldata, append = TRUE)
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
step <- function(dbname = "./model.sqlite") {
    ## Load model
    model <- load()

    ## Run one time-step

    ## Save the outcome

    invisible(NULL)
}

##' Load the disease spread model from the database
##'
##' @template dbname-param
##' @importFrom RSQLite dbConnect
##' @importFrom RSQLite dbDisconnect
##' @importFrom RSQLite dbReadTable
##' @importFrom RSQLite dbListTables
##' @importFrom RSQLite SQLite
##' @noRd
load <- function(dbname) {
    ## Open the database connection
    con <- dbConnect(SQLite(), dbname = dbname)
    on.exit(expr = dbDisconnect(con), add = TRUE)
    tables <- dbListTables(con)
    lapply(tables, function(x) {
        dbReadTable(con, x)
    })
}
