#include "erl_nif_compat.h"

#ifndef OTP_R13B03
ERL_NIF_TERM encode_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encode_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM reverse_tokens(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#else
ERL_NIF_TERM encode_string(ErlNifEnv* env, ERL_NIF_TERM ioList);
ERL_NIF_TERM reverse_tokens(ErlNifEnv* env, ERL_NIF_TERM ioList);
#endif

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
#ifndef OTP_R13B03
    {"encode_double", 1, encode_double},
#endif
    {"reverse_tokens", 1, reverse_tokens}
};

ERL_NIF_INIT(json, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL);
