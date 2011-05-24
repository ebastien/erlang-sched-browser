#include <math.h>
#include <erl_nif.h>

#define EARTH_RADIUS 6367.0

static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
  return (0);
}

static int
reload(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
  return (0);
}

static ERL_NIF_TERM
orthodromic_distance(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  double LonA, LatA, LonB, LatB;

  if (!enif_get_double(env, argv[0], &LonA))
    return enif_make_badarg(env);
  if (!enif_get_double(env, argv[1], &LatA))
    return enif_make_badarg(env);
  if (!enif_get_double(env, argv[2], &LonB))
    return enif_make_badarg(env);
  if (!enif_get_double(env, argv[3], &LatB))
    return enif_make_badarg(env);

  double SinLat = sin((LatB-LatA)/2);
  double SinLon = sin((LonB-LonA)/2);
  double Distance = 2*EARTH_RADIUS*asin(
    sqrt(SinLat*SinLat + cos(LatA)*cos(LatB)*SinLon*SinLon)
  );
  return enif_make_double(env, Distance);
}

static ErlNifFunc nif_funcs[] = {
  {"orthodromic_distance", 4, orthodromic_distance}
};

ERL_NIF_INIT(sched_nif, nif_funcs, load, reload, NULL, NULL)
