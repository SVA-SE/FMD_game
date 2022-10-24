##' Initialize an FMD model
##'
##' @importFrom SimInf u0_SIR
##' @importFrom SimInf SIR
##' @export
init <- function() {
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
    model@ldata <- rbind(model@ldata, x = nodes$x)
    model@ldata <- rbind(model@ldata, y = nodes$y)

    save(model, FALSE)

    invisible(NULL)
}

##' Append or overwrite data in model.sqlite
##'
##' @param append if TRUE, append data to the database, else create
##'     (or overwrite) data in the database.
##' @param path the path to the sqlite database.
##' @noRd
save <- function(model, append, path) {
    ## Open a database connection
    con <- dbConnect(SQLite(), path)
    on.exit(expr = dbDisconnect(con))

    ## Save the U state
    if (isTRUE(append)) {
        stop("Not implemented")
    } else {
        U <- as.data.frame(t(model@u0))
        U <- cbind(node = seq_len(nrow(U)), time = 0L, U)
        dbWriteTable(con, "U", U, overwrite = TRUE)
    }

    ## TODO: We need to consider that to check if each is empty before
    ## writing and then overwrite if true and append if false.

    ## Save ldata

    gdata <- model@gdata
    dbWriteTable(con, "gdata", gdata, overwrite = TRUE)

    ## TODO: If the gdata slot in the mode is empty what do we do?

    ## Save v

    ## Save events

    ## Save shift

    ## Save select

}

step <- function(path) {
    ## Step
    con <- dbConnect(SQLite(), path)
    dbWriteTable(con, "U", u0_SIR(), append = TRUE)
    dbDisconnect(con)

    con <- dbConnect(SQLite(), path)
    dbReadTable(con, "U")
    dbDisconnect(con)
}

load <- function() {

}

model_step <- function(con) {
    ## Step
    con <- dbConnect(SQLite(), "model.sqlite")
    dbWriteTable(con, "U", u0_SIR(), append = TRUE)
    dbDisconnect(con)

    con <- dbConnect(SQLite(), "model.sqlite")
    dbReadTable(con, "U")
    dbDisconnect(con)
}
