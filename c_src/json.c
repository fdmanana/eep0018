#include "erl_nif_compat.h"

ERL_NIF_TERM encode_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM reverse_tokens(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"encode_string", 1, encode_string},
    {"encode_double", 1, encode_double},
    {"reverse_tokens", 1, reverse_tokens}
};

ERL_NIF_INIT(json, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL);
