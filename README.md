# An FMD model that could be used for a game

The design is centred on a database file that can be queried or
modified from the game engine or from the disease spread model
side.

## The database

An SQlite database in a file that contains the following tables:

- U, the current and historical state of all nodes in the model.
- ldata, the covariates for each node in the model including
  geographical location of each node and perhaps the disease spread
  parameters if they are unique to each node.
- events, the current and historical events table.

## Possible interactions

### Initialize model (game)

First install the package. You can do that with remotes:

```sh
Rscript -e "install.packages('remotes', repos = 'https://cloud.r-project.org/')"
Rscript -e "remotes::install_github('SVA-SE/game.FMD')"
```

We need to be able to ask for a clean starting state of the model when
the game is started or restarted. The following will initialize a
model and save its relevant data to a database file "model.mysql":

```sh
Rscript -e "game.FMD::init('model.mysql')"
```

### Query the model

Query the database by whatever means you have to do that.

### Affect the model

Modify the states of the tables in the database. This will most likely
involve injecting events into the events table or modifying the last
entry in the U table.

### Step the model 1 day

Here we need the possibility to ask R to run the model one day from
what is in the database file. You should run the following to step the
model by one day:

```sh
Rscript -e "game.FMD::run('model.mysql')"
```
