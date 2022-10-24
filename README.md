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
- gdata, the fixed disease spread parameters.
- v, the continuous state(s) for each node and historical information
  about this.
- events, the current and historical events table.
- shift, the shift matrix that defines valid events.
- select, the select matrix. (These two matrices perhaps
  should not be stored here, since it would be weird to change this
  during the simulation?)

## Possible interactions

### Initialize model (game)

We need to be able to ask for a clean starting state of the model when
the game is started or restarted.

### Query the model

This will be by simply querying the database

### Affect the model

Modify the states of the tables in the database. This will most likely
involve injecting events into the events table or modifying the last
entry in the U table.

### Step the model 1 day

Here we need the possibility to ask R to run the model one day from
what is in the database file.

## Modal architecture.

The model in R needs to be able to preform the tasks required to
initialize the model and to step the model one day.

### Initialize the model {model\_init()}

This will consist of two steps

1. Create a SimInf model object in R {model\_init()}
2. Save the state of this model object in a database file {model\_save()}

### Step the model {model\_step()}

1. {model\_init()}
2. Read the tables from the database and inject them into the
   appropriate slots in the model object {model\_load()}
3. Run the model forward one day {model\_step\_internal()}
4. {model\_save()}

An R package would be a good way to implement this as we can specify
the dependencies on SimInf and RSQlite etc. We should have 2 exported
functions: `model\_init` and `model\_step`.
