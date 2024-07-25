-module(mt_dgraphs_pqueue_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
        Queue = mt_dgraphs_pqueue:new(),
        true = mt_dgraphs_pqueue:is_queue(Queue).

from_list_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        4 = mt_dgraphs_pqueue:len(Queue),
        {{value, c}, Q1} = mt_dgraphs_pqueue:out(Queue),
        {{value, a}, Q2} = mt_dgraphs_pqueue:out(Q1),
        {{value, b}, Q3} = mt_dgraphs_pqueue:out(Q2),
        1 = mt_dgraphs_pqueue:len(Q3),
        {{value, d}, Q4} = mt_dgraphs_pqueue:out(Q3),
        {empty, _} = mt_dgraphs_pqueue:out(Q4).

filter_test() ->
        Queue = mt_dgraphs_pqueue:from_list([1, 2, {3, 5}, {10, 1}]),
        4 = mt_dgraphs_pqueue:len(Queue),
        Q1 = mt_dgraphs_pqueue:filter(fun({V, _}) -> V =< 5 end, Queue),
        3 = mt_dgraphs_pqueue:len(Q1),
        {{value, 3}, Q2} = mt_dgraphs_pqueue:out(Q1),
        {{value, 1}, Q3} = mt_dgraphs_pqueue:out(Q2),
        {{value, 2}, Q4} = mt_dgraphs_pqueue:out(Q3),
        {empty, _} = mt_dgraphs_pqueue:out(Q4).

in_test() ->
        Queue = mt_dgraphs_pqueue:new(),
        empty = mt_dgraphs_pqueue:peek(Queue),
        Q1 = mt_dgraphs_pqueue:in(Queue, a),
        {value, a} = mt_dgraphs_pqueue:peek(Q1),
        Q2 = mt_dgraphs_pqueue:in(Q1, b),
        {value, a} = mt_dgraphs_pqueue:peek(Q2),
        Q3 = mt_dgraphs_pqueue:in_r(Q2, {c, 2}),
        {value, c} = mt_dgraphs_pqueue:peek(Q3),
        {value, b} = mt_dgraphs_pqueue:peek_r(Q3),
        Q4 = mt_dgraphs_pqueue:in(Q3, {d, 20}),
        {value, c} = mt_dgraphs_pqueue:peek(Q4),
        {value, d} = mt_dgraphs_pqueue:peek_r(Q4).

is_empty_test() ->
        true =
                mt_dgraphs_pqueue:is_empty(
                        mt_dgraphs_pqueue:new()),
        false =
                mt_dgraphs_pqueue:is_empty(
                        mt_dgraphs_pqueue:from_list([a, b, c])),
        true =
                mt_dgraphs_pqueue:is_empty(
                        mt_dgraphs_pqueue:drop(
                                mt_dgraphs_pqueue:from_list([a]))).

is_queue_test() ->
        true =
                mt_dgraphs_pqueue:is_queue(
                        mt_dgraphs_pqueue:new()),
        true =
                mt_dgraphs_pqueue:is_queue(
                        mt_dgraphs_pqueue:from_list([a, b, c])),
        true =
                mt_dgraphs_pqueue:is_queue(
                        mt_dgraphs_pqueue:from_list([{a, 1}, {b, 20}, {c, 30}])),
        false =
                mt_dgraphs_pqueue:is_queue(
                        mt_dgraphs_pqueue:reverse(
                                mt_dgraphs_pqueue:from_list([{a, 1}, {b, 2}]))),
        false = mt_dgraphs_pqueue:is_queue(3),
        false = mt_dgraphs_pqueue:is_queue([1, 2, 3]),
        false = mt_dgraphs_pqueue:is_queue([{a, 2}, b]).

join_test() ->
        Q = mt_dgraphs_pqueue:join(
                    mt_dgraphs_pqueue:from_list([{a, 20}, {b, 5}]),
                    mt_dgraphs_pqueue:from_list([{c, 15}, {d, 8}])),
        4 = mt_dgraphs_pqueue:len(Q),
        {{value, b}, Q1} = mt_dgraphs_pqueue:out(Q),
        {{value, d}, Q2} = mt_dgraphs_pqueue:out(Q1),
        {{value, c}, Q3} = mt_dgraphs_pqueue:out(Q2),
        {{value, a}, Q4} = mt_dgraphs_pqueue:out(Q3),
        {empty, _} = mt_dgraphs_pqueue:out(Q4).

member_test() ->
        Q = mt_dgraphs_pqueue:from_list([{a, 4}, {a, 9}, {c, 3}, {e, 6}]),
        true = mt_dgraphs_pqueue:member(Q, a),
        false = mt_dgraphs_pqueue:member(Q, d),
        true = mt_dgraphs_pqueue:member(Q, {c, 3}),
        true = mt_dgraphs_pqueue:member(Q, {a, 4}),
        false = mt_dgraphs_pqueue:member(Q, {a, 5}).

out_r_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        4 = mt_dgraphs_pqueue:len(Queue),
        {{value, d}, Q1} = mt_dgraphs_pqueue:out_r(Queue),
        {{value, b}, Q2} = mt_dgraphs_pqueue:out_r(Q1),
        {{value, a}, Q3} = mt_dgraphs_pqueue:out_r(Q2),
        1 = mt_dgraphs_pqueue:len(Q3),
        {{value, c}, Q4} = mt_dgraphs_pqueue:out_r(Q3),
        {empty, _} = mt_dgraphs_pqueue:out(Q4).

reverse_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        4 = mt_dgraphs_pqueue:len(Queue),
        [{d, 100}, {b, 10}, {a, 10}, {c, 0.1}] = mt_dgraphs_pqueue:reverse(Queue).

to_list_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        [c, a, b, d] = mt_dgraphs_pqueue:to_list(Queue),
        [d, b, a, c] = mt_dgraphs_pqueue:to_reverse_list(Queue).

drop_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        [a, b, d] =
                mt_dgraphs_pqueue:to_list(
                        mt_dgraphs_pqueue:drop(Queue)),
        [c, a, b] =
                mt_dgraphs_pqueue:to_list(
                        mt_dgraphs_pqueue:drop_r(Queue)).

get_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        c = mt_dgraphs_pqueue:get(Queue),
        d = mt_dgraphs_pqueue:get_r(Queue),
        c = mt_dgraphs_pqueue:get(Queue, nil),
        d = mt_dgraphs_pqueue:get_r(Queue, nil),
        nil =
                mt_dgraphs_pqueue:get(
                        mt_dgraphs_pqueue:new(), nil),
        nil =
                mt_dgraphs_pqueue:get_r(
                        mt_dgraphs_pqueue:new(), nil).

peek_test() ->
        Queue = mt_dgraphs_pqueue:from_list([a, b, {c, 0.1}, {d, 100}]),
        {value, c} = mt_dgraphs_pqueue:peek(Queue),
        {value, d} = mt_dgraphs_pqueue:peek_r(Queue),
        empty =
                mt_dgraphs_pqueue:peek(
                        mt_dgraphs_pqueue:new()),
        empty =
                mt_dgraphs_pqueue:peek_r(
                        mt_dgraphs_pqueue:new()).
