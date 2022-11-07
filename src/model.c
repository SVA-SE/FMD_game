#include <R_ext/Visibility.h>
#include "SimInf.h"

#define SIMINF_MODEL_RUN game_FMD_run
#define SIMINF_R_INIT R_init_game_FMD
#define SIMINF_FORCE_SYMBOLS TRUE

/* Offset in the integer compartment state vector. */
enum {S, I, R, N_COMPARTMENTS};

/* Offset in the real-valued continuous state vector. */
enum {I_COUPLING};

/* Offsets in the local data (ldata) vector to parameters in the
 * model */
enum {BETA, GAMMA, NODE, X, Y, NEIGHBOUR};

/**
 * susceptible to infected: S -> I
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SIR_S_to_I(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    const double S_n = u[S];
    const double I_n = u[I];
    const double n = S_n + I_n + u[R];

    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    if (n > 0.0)
        return (ldata[BETA] * S_n * (I_n + v[I_COUPLING])) / n;
    return 0.0;
}

/**
 *  infected to recovered: I -> R
 *
 * @param u The compartment state vector in node.
 * @param v The continuous state vector in node.
 * @param ldata The local data vector for node.
 * @param gdata The global data vector.
 * @param t Current time.
 * @return propensity.
 */
static double SIR_I_to_R(
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    double t)
{
    SIMINF_UNUSED(v);
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

    return ldata[GAMMA] * u[I];
}

/**
 * SIR post time step
 *
 * @param v_new The continuous state vector in the node after the post
 * time step
 * @param u The compartment state vector in the node.
 * @param v The current continuous state vector in the node.
 * @param ldata The local data vector for the node.
 * @param gdata The global data vector.
 * @param node The node.
 * @param t The current time.
 * @return error code (<0), or 1 if node needs to update the
 * transition rates, or 0 when it doesn't need to update the
 * transition rates.
 */
static int SIR_post_time_step(
    double *v_new,
    const int *u,
    const double *v,
    const double *ldata,
    const double *gdata,
    int node,
    double t)
{
    SIMINF_UNUSED(gdata);
    SIMINF_UNUSED(t);

   /* Clear the force of infection from neighbouring nodes. Then
    * iterate over all neighbors and update the value. */
    v_new[I_COUPLING] = 0.0;

    /* Deterimine the pointer to the compartment state vector in the
     * first node. Use this to find the number of individuals at
     * neighbours to the current node. */
    const int *u_0 = &u[-N_COMPARTMENTS * node];

    /* Variable to keep track of where to look in the neigbours data:
     * (index, coupling), (index, * coupling), ..., (-1, 0). Start at
     * the position for the first index. */
    int i = NEIGHBOUR;

    /* Iterate over all neighbours and add the contributions from
     * infected individuals. */
    while ((int)ldata[i] >= 0) {
        /* Index and coupling to neighbor. */
        int j = (int)ldata[i];
        double coupling = ldata[i + 1];

        /* Add the contribution from the infected in node j */
        v_new[I_COUPLING] += coupling * u_0[j * N_COMPARTMENTS + I];

        /* Move to the next neighbour pair (index, distance) ,i.e.,
         * move 'i' two steps forward in the ldata vector. */
        i = i + 2;
    }

    /* Error check the new I_coupling value. */
    if (!R_FINITE(v_new[I_COUPLING]))
        return SIMINF_ERR_V_IS_NOT_FINITE;
    if (v_new[I_COUPLING] < 0.0)
        return SIMINF_ERR_V_IS_NEGATIVE;

    /*  Finally, if I_coupling has changed compared to the previous
     *  value, return 1 to indicate to the numerical solver that the
     *  transition rates must be updated. */
    return v[I_COUPLING] != v_new[I_COUPLING];
}

/**
 * Run a trajectory of the model.
 *
 * @param model The model.
 * @param solver The name of the numerical solver.
 * @return A model with a trajectory attached to it.
 */
static SEXP SIMINF_MODEL_RUN(SEXP model, SEXP solver)
{
    static SEXP(*SimInf_run)(SEXP, SEXP, TRFun*, PTSFun) = NULL;
    TRFun tr_fun[] = {&SIR_S_to_I, &SIR_I_to_R};

    if (!SimInf_run) {
        SimInf_run = (SEXP(*)(SEXP, SEXP, TRFun*, PTSFun))
            R_GetCCallable("SimInf", "SimInf_run");

        if (!SimInf_run) {
            Rf_error("Cannot find function 'SimInf_run'.");
        }
    }

    return SimInf_run(model, solver, tr_fun, &SIR_post_time_step);
}

/**
 * A NULL-terminated array of routines to register for the .Call
 * interface, see section '5.4 Registering native routines' in
 * the 'Writing R Extensions' manual.
 */
static const R_CallMethodDef callMethods[] =
{
    SIMINF_CALLDEF(SIMINF_MODEL_RUN, 2),
    {NULL, NULL, 0}
};

/**
 * This routine will be invoked when R loads the shared object/DLL,
 * see section '5.4 Registering native routines' in the
 * 'Writing R Extensions' manual.
 */
void SIMINF_R_INIT(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, SIMINF_FORCE_SYMBOLS);
}
