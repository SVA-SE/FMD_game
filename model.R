library(RSQLite)
ilibrary(SimInf)

## Create model
u0 <- u0_SIR()
u0$I[1] <- 1
tspan <- seq(from = 1, to = 4*365, by = 1)
model <- SIR(u0     = u0,
             tspan  = tspan,
             events = events_SIR(),
             beta   = 0.16,
             gamma  = 0.01)

## Open a database connection
con <- dbConnect(SQLite(), "model.sqlite")

## Init U
U <- as.data.frame(t(model@u0))
U <- cbind(node = seq_len(nrow(U)), time = 0L, U)
dbWriteTable(con, "U", U, overwrite = TRUE)

gdata <- c(beta = 2, gamma = 3)
dbWriteTable(con, "gdata", gdata, overwrite = TRUE)


dbDisconnect(con)

## Step
con <- dbConnect(SQLite(), "model.sqlite")
dbWriteTable(con, "U", u0_SIR(), append = TRUE)
dbDisconnect(con)

con <- dbConnect(SQLite(), "model.sqlite")
dbReadTable(con, "U")
dbDisconnect(con)
