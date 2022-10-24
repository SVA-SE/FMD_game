library(RSQLite)
library(SimInf)

model_init <- function() {
    u0 <- u0_SIR()
    u0$I[1] <- 1
    tspan <- seq(from = 1, to = 4*365, by = 1)
    SIR(u0     = u0,
        tspan  = tspan,
        events = events_SIR(),
        beta   = 0.16,
        gamma  = 0.01)
}

model_save <- function(model, path) {
    ## Open a database connection
    con <- dbConnect(SQLite(), path)
    on.exit(expr = dbDisconnect(con))

    ## Save the U state
    U <- as.data.frame(t(model@u0))
    U <- cbind(node = seq_len(nrow(U)), time = 0L, U)
    dbWriteTable(con, "U", U, overwrite = TRUE)

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

model_step <- function(path) {
    ## Step
    con <- dbConnect(SQLite(), path)
    dbWriteTable(con, "U", u0_SIR(), append = TRUE)
    dbDisconnect(con)

    con <- dbConnect(SQLite(), path)
    dbReadTable(con, "U")
    dbDisconnect(con)
}

model_load <- function() {

}





model <- model_init()
model_save(model, "model.sqlite")
