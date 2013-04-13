/*% 'antonius' chess engine
%%% Copyright (C) 2013 Rabbe Fogelholm
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%*/

#include <stdbool.h>

#include "erl_nif.h"

/**
 * 
 *
static ERL_NIF_TERM hello(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	return enif_make_string(env, "hi from c", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM createTuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	return enif_make_tuple2(env, argv[0], argv[1]);
}

static ERL_NIF_TERM createTupleFromArray(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

	ERL_NIF_TERM arr[3];

	arr[0] = enif_make_string(env, "tic", ERL_NIF_LATIN1);
	arr[1] = enif_make_string(env, "tac", ERL_NIF_LATIN1);
	arr[2] = enif_make_string(env, "toe", ERL_NIF_LATIN1);

	return enif_make_tuple_from_array(env, arr, 3);
}
 *
 */

#define MATERIAL_WHITE 0
#define MATERIAL_BLACK 1
#define ATOM_BOARDMAP 2
#define ATOM_NULL 3
#define ELEMENT_PIECE_SQUARE 3
#define ELEMENT_SQUARE_TUPLEINDEX 7

static ERL_NIF_TERM createPiecesMap(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

	ERL_NIF_TERM arr[64];
	for (int j = 0; j < 64; j++) {
		arr[j] = argv[ATOM_NULL];
	}

	for (int material = MATERIAL_WHITE;
			material == MATERIAL_WHITE || material == MATERIAL_BLACK;
			material++) {
		for (ERL_NIF_TERM m = argv[material]; true; ) {
			ERL_NIF_TERM head;
			ERL_NIF_TERM tail;
			if (enif_get_list_cell(env, m, &head, &tail) == 0) {
				// list exhausted, fine
				break;
			}
			else {
				const ERL_NIF_TERM *pieceTupleArray;
				int pieceArity;
				if (enif_get_tuple(env, head, &pieceArity, &pieceTupleArray) == false) {
					return enif_make_badarg(env);
				}
				else {
					const ERL_NIF_TERM square = pieceTupleArray[ELEMENT_PIECE_SQUARE];
					const ERL_NIF_TERM *squareTupleArray;
					int squareArity;
					if (enif_get_tuple(env, square, &squareArity, &squareTupleArray) == false) {
						return enif_make_badarg(env);
					}
					else {
						const ERL_NIF_TERM tupleIndex = squareTupleArray[ELEMENT_SQUARE_TUPLEINDEX];
						int tupleIndexInt;
						if (enif_get_int(env, tupleIndex, &tupleIndexInt) == false) {
							return enif_make_badarg(env);
						}
						else {
							arr[tupleIndexInt-1] = head;
						}
					}
				}
				m = tail;
			}
		}
	}
	const ERL_NIF_TERM tuple = enif_make_tuple_from_array(env, arr, 64);
	return enif_make_tuple2(env, argv[ATOM_BOARDMAP], tuple);
}


///**
// * Returns an Erlang atom with the given name. The atom is created
// * if it does not already exist.
// */
//static ERL_NIF_TERM intern(ErlNifEnv *env, const char *atomName, ERL_NIF_LATIN1) {
//	ERL_NIF_TERM * const atom;
//	if (enif_make_existing_atom(env, atomName, &atom, ERL_NIF_LATIN1)) {
//		return *atom;
//	}
//	else {
//		return enif_make_atom(env, atomName);
//	}
//}

static ErlNifFunc nif_funcs[] = {
	{"createPiecesMap", 4, createPiecesMap}
};

ERL_NIF_INIT(core_nif, nif_funcs, NULL, NULL, NULL, NULL)
