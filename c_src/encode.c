#include <stdio.h>
#include <string.h>
#include <math.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_encode.h"


ERL_NIF_TERM
encode_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    ErlNifBinary bin;
    ErlNifBinary retBin;
    yajl_alloc_funcs allocFuns;
    yajl_buf buf;
    
    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        ret = enif_make_badarg(env);
        goto done;
    }

    yajl_set_default_alloc_funcs(&allocFuns);
    buf = yajl_buf_alloc(&allocFuns);

    yajl_string_encode(buf, bin.data, bin.size);

    if (!yajl_buf_data(buf)) {
        ret = enif_make_tuple(env, 2,
                              enif_make_atom(env, "error"),
                              enif_make_atom(env, "insufficient_memory"));
        goto done;
    } else {
        if (!enif_alloc_binary(yajl_buf_len(buf), &retBin)) {
            ret = enif_make_tuple(env, 2,
                                  enif_make_atom(env, "error"),
                                  enif_make_atom(env, "insufficient_memory"));
            goto done;
        }

        memcpy(retBin.data, yajl_buf_data(buf), yajl_buf_len(buf));
        ret = enif_make_binary(env, &retBin);
    }
done:
    return ret;
}


ERL_NIF_TERM
encode_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    double number;
    char buffer[32];
    
    if(argc != 1 || !enif_get_double(env, argv[0], &number))
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    if (isnan(number) || isinf(number)) {
        ret =  enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "invalid_number")
        );
    } else {
        snprintf(buffer, sizeof(buffer), "%.16g", number);
        ret = enif_make_string(env, buffer, ERL_NIF_LATIN1);
    }
done:
    return ret;
}
