%%%-------------------------------------------------------------------
%%% @author sergio
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mar. 2022 13:56
%%%-------------------------------------------------------------------
-module(tree).
-author("sergio").

-export([start_node/0, add_child/2, height/1]).

%% API
start_node() ->
  spawn(?MODULE, init_node, []).


add_child(Tree, Child_Tree) ->
  Tree ! {add_child, Child_Tree}.

%Preguntámoslle a altura a todos os fillos de primeiro nivel e sumámoslle 1 para a altura da raiz e via!
height(Tree) ->
    Tree ! {height, self()},   %Preguntámoslle por altura ao árbol
    receive
      {height_reply, H} -> H  %Cando contesta damos a súa altura
    end
.


%% Internal functions
init_node() ->
  node_loop([]).

node_loop(Children) ->
  receive
    {add_child, Child_Tree} ->
      node_loop(Children ++ [Child_Tree]);

    %Queremos preguntarlles a todos os fillos a altura súa e obter unha lista de alturas
    {height, FROM} -> HEIGHTS = lists:map(fun height/1, Children),
      %Collemos o valor máximo!!!!!!
       MAX_HEIGHT = maximo(HEIGHTS),
       FROM ! {height_reply, MAX_HEIGHT + 1},   %Respondemos ao que pregunta a altura!
       node_loop(Children)
end.

maximo([]) -> -1;
maximo([H | T]) ->  max(maximo(T), H).   %Collemos o máximo entre o max de todos e do primeiro que é H

maximo_con_fold(L) ->
  lists:foldl(fun(ACC, H) -> max(ACC, H) end, -1, L).  %A fun fai que dados 2 valores nos de o máximo entre eles
%-1 É o valor inicial de ACC
%L é o valor inicial da lista

maximo_again(L) ->
 lists:foldl(fun max/2, -1, L).