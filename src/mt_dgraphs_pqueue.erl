%% ----------------------------------
%%
%% @author Matthew Tolman
%% @copyright 2024 Matthew Tolman
%% @doc Simple priority queue.
%% @version 0.1.0
%% @end
%%
%% ----------------------------------
-module(mt_dgraphs_pqueue).

-type queue_entry() :: {any(), any()}.
-type pqueue() :: [{any(), any()}] | [].

-define(DEFAULT_PRIORITY, 10).

-export_type([pqueue/0, queue_entry/0]).

-export([new/0, is_empty/1, get/1, get/2, get_r/1, get_r/2, drop/1, is_pqueue/1, filter/2,
         is_queue/1, in/2, from_list/1, join/2, len/1, member/2, out/1, out_r/1, reverse/1,
         split/2, to_list/1, to_reverse_list/1, in_r/2, drop_r/1, in_all/2, peek_r/1, peek/1]).

%% @doc Creates a new priority queue. Lower priority values come first in the list.
-spec new() -> mt_dgraphs_pqueue:pqueue().
new() ->
    [].

%% ----------------------------------
%%
%% @doc
%% Filters items from the priority queue.
%%
%% The filter function recieves both the item and the priority in the tuple {Item, Priority}.
%% @end
%% @param FilterFun Filter function.
%% @param Queue Priority queue to filter.
%% @returns Filtered queue.
%% @end
%%
%% ----------------------------------
-spec filter(fun(({any(), any()}) -> boolean()), pqueue()) -> pqueue().
filter(FilterFun, Queue) ->
    lists:filter(FilterFun, Queue).

%% ----------------------------------
%%
%% @doc
%% Creates a priority queue from a list of either items or {Item, Priority} tuples. Items without a priority will get the default priority.
%%
%% Note: If you are wanting to push a 2 element tuple as an item, it will need to be in the form {{Your, Tuple}, Priorit]}
%% @end
%% @returns A new queue.
%% @end
%%
%% ----------------------------------
-spec from_list([queue_entry() | any()]) -> pqueue().
from_list(Items) when is_list(Items) ->
    push_all(mt_dgraphs_pqueue:new(), Items).

%% ----------------------------------
%%
%% @doc Adds an item to a priority queue. It can either be in the form {Item, Priority} or Item (with default priority)
%% @end
%%
%% ----------------------------------
-spec in(pqueue(), queue_entry() | any()) -> pqueue().
in(Queue, Node) ->
    InsertNode = default_priority(Node),
    {_, Priority} = InsertNode,
    [{E, P} || {E, P} <- Queue, P =< Priority]
    ++ [InsertNode | [{E, P} || {E, P} <- Queue, P > Priority]].

%% ----------------------------------
%%
%% @doc Adds all elements to the priority queue.
%% @end
%% @param Queue Priority queue to add to.
%% @param List List of either {Item, Priority} or Item to add to the queue. If priority is omitted, then it will be given the default priority.
%% @returns Return.
%% @end
%%
%% ----------------------------------
-spec in_all(pqueue(), [queue_entry() | any()]) -> pqueue().
in_all(Queue, List) ->
    push_all(Queue, List).

%% @equiv mt_dgraphs_pqueue:in(Queue, Item)
-spec in_r(pqueue(), queue_entry()) -> pqueue().
in_r(Queue, Item) ->
    in(Queue, Item).

%% ----------------------------------
%%
%% @doc Checks if a queue is empty.
%% @end
%% @returns Returns true if empty, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_empty(pqueue()) -> boolean().
is_empty(_Queue = []) ->
    true;
is_empty(_) ->
    false.

%% @equiv mt_dgraphs_pqueue:is_pqueue(Queue)
-spec is_queue(any()) -> boolean().
is_queue(Queue) ->
    is_pqueue(Queue).

%% ----------------------------------
%%
%% @doc Checks if a term is a valid priority queue.
%% @end
%%
%% ----------------------------------
-spec is_pqueue(any()) -> boolean().
is_pqueue(Queue) when is_list(Queue) ->
    is_sorted(Queue);
is_pqueue(_) ->
    false.

%% ----------------------------------
%%
%% @doc Joins two priority queues together.
%% @end
%%
%% ----------------------------------
-spec join(pqueue(), pqueue()) -> pqueue().
join(Queue1, Queue2) when length(Queue1) < length(Queue2) ->
    Unsorted = Queue1 ++ Queue2,
    lists:sort(fun({_, L}, {_, R}) -> L =< R end, Unsorted);
join(Queue1, Queue2) ->
    Unsorted = Queue2 ++ Queue1,
    lists:sort(fun({_, L}, {_, R}) -> L =< R end, Unsorted).

%% ----------------------------------
%%
%% @doc Returns the length of the priority queue.
%% @end
%%
%% ----------------------------------
-spec len(pqueue()) -> integer().
len(Queue) ->
    length(Queue).

%% ----------------------------------
%%
%% @doc
%% Checks if an item is part of the queue.
%%
%% The item to search for may have a priority specified with it.
%%
%% If the priority is the atom 'any' then it will search to see if the value is present without checking the priority.
%% If a priority is specified, it will search for both a matching value and priority.
%% Otherwise, it will only check for a matching value.
%% @end
%% @param Queue Queue to search.
%% @param Item Item to search for. Can be {Value, Priority} or Value.
%% @returns Return.
%% @end
%%
%% ----------------------------------
-spec member(pqueue(), queue_entry() | any() | {any(), any}) -> boolean().
member(Queue, _Item = {V, any}) ->
    case lists:filter(fun({VL, _}) -> V =:= VL end, Queue) of
        [] ->
            false;
        _ ->
            true
    end;
member(Queue, _Item = {V, P}) ->
    case lists:filter(fun({VL, PL}) -> (V =:= VL) and (PL =:= P) end, Queue) of
        [] ->
            false;
        _ ->
            true
    end;
member(Queue, V) ->
    member(Queue, {V, any}).

%% ----------------------------------
%%
%% @doc
%% Removes the lowest priority item from the queue and returns the item removed.
%% When a value is returned, it will return the tuple {{value, Value}, NewQueue}.
%% When the queue is empty, it will return {empty, NewQueue}.
%% @end
%%
%% ----------------------------------
-spec out(pqueue()) -> {empty, pqueue()} | {{value, any()}, pqueue()}.
out([{H, _} | R]) ->
    {{value, H}, R};
out([]) ->
    {empty, []}.

%% ----------------------------------
%%
%% @doc
%% Removes the highest priority item from the queue and returns the item removed.
%% When a value is returned, it will return the tuple {{value, Value}, NewQueue}.
%% When the queue is empty, it will return {empty, NewQueue}.
%% @end
%%
%% ----------------------------------
-spec out_r(pqueue()) -> {empty, pqueue()} | {{value, any()}, pqueue()}.
out_r(Queue) ->
    case lists:reverse(Queue) of
        [{H, _} | R] ->
            {{value, H}, lists:reverse(R)};
        _ ->
            {empty, []}
    end.

%% ----------------------------------
%%
%% @doc Reverses a priority queue to be highest items first.
%% @end
%%
%% ----------------------------------
-spec reverse(pqueue()) -> [queue_entry()].
reverse(Queue) ->
    lists:reverse(Queue).

%% ----------------------------------
%%
%% @doc Splits a queue at the Nth position. Returns both halves in a tuple.
%% @param Queue Queue to split.
%% @param N Number of elements to keep in the first queue.
%% @end
%%
%% ----------------------------------
-spec split(pqueue(), integer()) -> {pqueue(), pqueue()}.
split(Queue, N) ->
    lists:split(N, Queue).

%% ----------------------------------
%%
%% @doc Converts a priority queue to a list of items.
%% The resulting list will only be the items, no priorities will be included.
%% @end
%%
%% ----------------------------------
-spec to_list(pqueue()) -> [any()] | [].
to_list(Queue) ->
    lists:map(fun({V, _}) -> V end, Queue).

%% ----------------------------------
%%
%% @doc Converts a priority queue to a list of items in reverse order.
%% The resulting list will only be the items, no priorities will be included.
%% @end
%%
%% ----------------------------------
-spec to_reverse_list(pqueue()) -> [any()] | [].
to_reverse_list(Queue) ->
    lists:map(fun({V, _}) -> V end, reverse(Queue)).

%% ----------------------------------
%%
%% @doc Returns the queue with the lowest priority item removed.
%% @end
%%
%% ----------------------------------
-spec drop(pqueue()) -> pqueue().
drop(_Queue = [_ | R]) ->
    R;
drop(_) ->
    [].

%% ----------------------------------
%%
%% @doc Returns the queue with the highest priority item removed.
%% @end
%%
%% ----------------------------------
-spec drop_r(pqueue()) -> pqueue().
drop_r(Queue) when is_list(Queue) ->
    lists:reverse(drop(lists:reverse(Queue)));
drop_r(_) ->
    [].

%% ----------------------------------
%%
%% @doc Gets the lowest priority item from the queue or raises an error if the queue is empty.
%% @end
%%
%% ----------------------------------
-spec get(pqueue()) -> any();
         ([]) -> no_return().
get(_Queue = [{H, _} | _]) ->
    H;
get(_) ->
    error(empty).

%% ----------------------------------
%%
%% @doc Gets the highest priority item from the queue or raises an error if the queue is empty.
%% @end
%%
%% ----------------------------------
-spec get_r(pqueue()) -> any().
get_r(Queue) ->
    mt_dgraphs_pqueue:get(
        lists:reverse(Queue)).

%% ----------------------------------
%%
%% @doc Gets the highest priority item from the queue or default value if queue is empty.
%% @end
%%
%% ----------------------------------
-spec get_r(pqueue(), any()) -> any().
get_r(Queue, Default) ->
    get(lists:reverse(Queue), Default).

%% ----------------------------------
%%
%% @doc Gets the lowest priority item from the queue or returns the default value if empty.
%% @end
%%
%% ----------------------------------
-spec get(pqueue(), any()) -> any().
get(_Queue = [{H, _} | _], _Default) ->
    H;
get(_, D) ->
    D.

%% ----------------------------------
%%
%% @doc Peeks at the lowest priority item in the queue.
%% If the queue has a value, it will return the tuple {value, Value}.
%% Otherwise, it will return the atom 'empty'.
%% @end
%%
%% ----------------------------------
-spec peek(pqueue()) -> empty | {value, any()}.
peek(_Queue = [{H, _} | _]) ->
    {value, H};
peek([]) ->
    empty.

%% ----------------------------------
%%
%% @doc Peeks at the highest priority item in the queue.
%% If the queue has a value, it will return the tuple {value, Value}.
%% Otherwise, it will return the atom 'empty'.
%% @end
%%
%% ----------------------------------
-spec peek_r(pqueue()) -> empty | {value, any()}.
peek_r(Queue) ->
    peek(lists:reverse(Queue)).

%% Implementation

push_all(Queue, New) ->
    Unsorted = join_short(Queue, lists:map(fun(E) -> default_priority(E) end, New)),
    lists:sort(fun({_, L}, {_, R}) -> L =< R end, Unsorted).

join_short(L1, L2) when length(L1) < length(L2) ->
    L2 ++ L1;
join_short(L1, L2) ->
    L1 ++ L2.

default_priority(Node = {_V, _P}) ->
    Node;
default_priority(Value) ->
    {Value, ?DEFAULT_PRIORITY}.

is_sorted([{_, P1}, E2 = {_, P2} | R]) when P1 =< P2 ->
    is_sorted([E2 | R]);
is_sorted([{_, _}]) ->
    true;
is_sorted([]) ->
    true;
is_sorted(_) ->
    false.
