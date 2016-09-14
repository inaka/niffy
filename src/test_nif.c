#include "erl_nif.h"
#include <math.h>

static ERL_NIF_TERM sum_c_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int a;
  int b;
  if (!enif_get_int(env, argv[1], &a))
    return enif_make_badarg(env);
  if (!enif_get_int(env, argv[0], &b))
    return enif_make_badarg(env);
  return enif_make_int(a + b);
}

static ERL_NIF_TERM nat_log_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  double a;
  if (!enif_get_double(env, argv[0], &a))
    return enif_make_badarg(env);
  double logA = log(a);
  return enif_make_double(logA);
}

static ERL_NIF_TERM square_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int a;
  if (!enif_get_int(env, argv[0], &a))
    return enif_make_badarg(env);
  return enif_make_int(a * a);
}

static ErlNifFunc niffuncs[] = {{"sum_c", 2, sum_c_nif, 0},
                                {"nat_log", 1, nat_log_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
                                {"square", 1, square_nif, 0}};

ERL_NIF_INIT(test, niffuncs, NULL, NULL, NULL, NULL)