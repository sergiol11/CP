%%%-------------------------------------------------------------------
%%% @author sergio
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mar. 2022 13:37
%%%-------------------------------------------------------------------
-module(store).
-author("sergio").


-export([start/0, store/2, get/2]).
-export([loop/1]).

%% API

start() -> spawn(?MODULE, init, []).

store(S, P) ->
  S ! {store, P}  %Que o garde o servidor, nn necesitamos resposta
.

get(S, F) ->
  S ! {get, F, self()},  %F é a funcion
  receive
    {get_reply, R} -> R
  end
.

init() ->
  loop([])
.

loop(Elements) ->
  receive
    {store, P} -> loop([P | Elements]);  %Gardamos

    {get, F, FROM} ->
      case find(F, Elements) of   %Se nn queremos facer o find poñemos case lists:search(F, Elements) of
        {value, E} -> FROM ! {get_reply, {ok, E}},  %R = {ok, E} !!! arriba
          loop(lists:delete(E, Elements));   %Borramos o elemento!

      %O elemento nn existe
        false -> {get_reply, {error, no_product}},
          loop(Elements)
      end
  end
.

find(_, []) -> false;
find(F, [H | T]) -> case F(H) of
                      true -> {value, H};
                      false -> find(F, T)
                    end.
