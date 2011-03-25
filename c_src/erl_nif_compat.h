#ifndef ERL_NIF_COMPAT_H_
#define ERL_NIF_COMPAT_H_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "erl_nif.h"

#if ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION == 0
#define OTP_R14
#elif ERL_NIF_MAJOR_VERSION == 1 && ERL_NIF_MINOR_VERSION == 0
#define OTP_R13B04
#elif ERL_NIF_MAJOR_VERSION == 0 && ERL_NIF_MINOR_VERSION == 1
#define OTP_R13B03
#endif


#ifdef OTP_R13B03

#define enif_open_resource_type_compat enif_open_resource_type
#define enif_alloc_resource_compat enif_alloc_resource
#define enif_release_resource_compat enif_release_resource
#define enif_alloc_binary_compat enif_alloc_binary
#define enif_alloc_compat enif_alloc
#define enif_free_compat enif_free
#define enif_get_atom_compat enif_get_atom
#define enif_priv_data_compat enif_get_data
#define enif_make_uint_compat enif_make_ulong

#define enif_make_string_compat(E, B, Enc) \
    enif_make_string(E, B)

#endif /* R13B03 */


#ifdef OTP_R13B04

#define enif_open_resource_type_compat enif_open_resource_type
#define enif_alloc_resource_compat enif_alloc_resource
#define enif_release_resource_compat enif_release_resource
#define enif_alloc_binary_compat enif_alloc_binary
#define enif_alloc_compat enif_alloc
#define enif_free_compat enif_free
#define enif_get_atom_compat enif_get_atom
#define enif_priv_data_compat enif_priv_data
#define enif_make_string_compat enif_make_string
#define enif_make_uint_compat enif_make_uint

#endif /* R13B04 */


#if !defined(OTP_R13B03) && !defined(OTP_R13B04)

#define enif_open_resource_type_compat(E, N, D, F, T) \
    enif_open_resource_type(E, NULL, N, D, F, T)

#define enif_alloc_resource_compat(E, T, S) \
    enif_alloc_resource(T, S)

#define enif_release_resource_compat(E, H) \
    enif_release_resource(H)

#define enif_alloc_binary_compat(E, S, B) \
    enif_alloc_binary(S, B)

#define enif_alloc_compat(E, S) \
    enif_alloc(S)

#define enif_free_compat(E, P) \
    enif_free(P)

#define enif_get_atom_compat(E, T, B, S) \
    enif_get_atom(E, T, B, S, ERL_NIF_LATIN1)

#define enif_priv_data_compat enif_priv_data
#define enif_make_string_compat enif_make_string
#define enif_make_uint_compat enif_make_uint

#endif /* R14 and future releases */



#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ERL_NIF_COMPAT_H_ */
