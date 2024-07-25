-module(mt_dgraphs_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("mt_dgraphs.hrl").

-export([sleep_and_change/3]).

basic_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        0 = mt_dgraphs_server:version(Graph),
        [mt_dgraphs_server:add_node(Graph, N) || N <- [a, b, c, d]],
        4 = mt_dgraphs_server:version(Graph),
        4 = mt_dgraphs_server:num_nodes(Graph).

new_type_test() ->
        {ok, Graph1} = mt_dgraphs_server:new(directed),
        0 = mt_dgraphs_server:version(Graph1),
        T1 = mt_dgraphs_server:type(Graph1),
        ?assert(T1 =:= directed),
        {ok, Graph2} = mt_dgraphs_server:new(undirected),
        T2 = mt_dgraphs_server:type(Graph2),
        ?assert(T2 =:= undirected).

new_nodes_edges_test() ->
        {ok, Graph1} = mt_dgraphs_server:new([a, b, c], [{b, c}]),
        0 = mt_dgraphs_server:version(Graph1),
        NE1 = mt_dgraphs_server:num_edges(Graph1),
        NN1 = mt_dgraphs_server:num_nodes(Graph1),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 3).

new_edges_test() ->
        {ok, Graph1} = mt_dgraphs_server:new(directed, [{a, b}]),
        {ok, Graph2} = mt_dgraphs_server:new(undirected, [{a, b}, {d, e}]),
        0 = mt_dgraphs_server:version(Graph1),
        NE1 = mt_dgraphs_server:num_edges(Graph1),
        NE2 = mt_dgraphs_server:num_edges(Graph2),
        NN1 = mt_dgraphs_server:num_nodes(Graph1),
        NN2 = mt_dgraphs_server:num_nodes(Graph2),
        ?assert(NE1 =:= 1),
        ?assert(NE2 =:= 2),
        ?assert(NN1 =:= 2),
        ?assert(NN2 =:= 4).

new_all_test() ->
        {ok, Graph1} = mt_dgraphs_server:new(directed, [a, b, c], [{d, e}]),
        0 = mt_dgraphs_server:version(Graph1),
        NE1 = mt_dgraphs_server:num_edges(Graph1),
        NN1 = mt_dgraphs_server:num_nodes(Graph1),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 5).

add_edges_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        0 = mt_dgraphs_server:version(Graph),
        NE1 = mt_dgraphs_server:num_edges(Graph),
        NN1 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE1 =:= 0),
        ?assert(NN1 =:= 0),
        2 = mt_dgraphs_server:add_edges(Graph, [{a, b}, {b, c}]),
        1 = mt_dgraphs_server:version(Graph),
        NE2 = mt_dgraphs_server:num_edges(Graph),
        NN2 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        {ok, Graph2} = mt_dgraphs_server:new(undirected),
        2 = mt_dgraphs_server:add_edges(Graph2, [{a, b}, {b, c}]),
        NE3 = mt_dgraphs_server:num_edges(Graph2),
        NN3 = mt_dgraphs_server:num_nodes(Graph2),
        ?assert(NE3 =:= 2),
        ?assert(NN3 =:= 3),
        already_exists = mt_dgraphs_server:add_edge(Graph2, a, b, [{label, "Test"}]),
        {ok, nil} = mt_dgraphs_server:get_edge_label(Graph2, a, b).

add_edge_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        0 = mt_dgraphs_server:version(Graph),
        ok = mt_dgraphs_server:add_edge(Graph, a, b),
        NE1 = mt_dgraphs_server:num_edges(Graph),
        1 = mt_dgraphs_server:version(Graph),
        NN1 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 2),
        ok = mt_dgraphs_server:add_edge(Graph, c, a, [{label, "C->A"}]),
        2 = mt_dgraphs_server:version(Graph),
        NE2 = mt_dgraphs_server:num_edges(Graph),
        NN2 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        already_exists = mt_dgraphs_server:add_edge(Graph, c, a, []),
        2 = mt_dgraphs_server:version(Graph).

set_edge_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        0 = mt_dgraphs_server:version(Graph),
        ok = mt_dgraphs_server:set_edge(Graph, a, b),
        1 = mt_dgraphs_server:version(Graph),
        NE1 = mt_dgraphs_server:num_edges(Graph),
        NN1 = mt_dgraphs_server:num_nodes(Graph),
        {ok, nil} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 2),
        ok = mt_dgraphs_server:set_edge(Graph, a, b, [{label, "A->B"}]),
        NE2 = mt_dgraphs_server:num_edges(Graph),
        NN2 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE2 =:= 1),
        ?assert(NN2 =:= 2),
        {ok, "A->B"} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        ok = mt_dgraphs_server:set_edge(Graph, a, b),
        3 = mt_dgraphs_server:version(Graph).

set_edges_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        NE1 = mt_dgraphs_server:num_edges(Graph),
        NN1 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE1 =:= 0),
        ?assert(NN1 =:= 0),
        ok = mt_dgraphs_server:set_edges(Graph, [{a, b}, {b, c}]),
        NE2 = mt_dgraphs_server:num_edges(Graph),
        NN2 = mt_dgraphs_server:num_nodes(Graph),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        {ok, Graph2} = mt_dgraphs_server:new(undirected),
        ok = mt_dgraphs_server:set_edges(Graph2, [{a, b}, {b, c}]),
        NE3 = mt_dgraphs_server:num_edges(Graph2),
        NN3 = mt_dgraphs_server:num_nodes(Graph2),
        ?assert(NE3 =:= 2),
        ?assert(NN3 =:= 3),
        ok = mt_dgraphs_server:set_edges(Graph2, [{a, b, [{label, "Test"}]}]),
        {ok, "Test"} = mt_dgraphs_server:get_edge_label(Graph2, a, b).

add_node_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok = mt_dgraphs_server:add_node(Graph, a, [{label, "A"}]),
        1 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        {ok, "A"} = mt_dgraphs_server:get_node_label(Graph, a),
        ok = mt_dgraphs_server:add_node(Graph, b),
        2 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        already_exists = mt_dgraphs_server:add_node(Graph, a).

by_id_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([{a, [{label, "A"}]}, b], [{a, b, [{label, "A->B"}]}]),
        {ok, #mt_dgraphs_node{id = a, label = "A"}} = mt_dgraphs_server:node_by_id(Graph, a),
        {ok,
         #mt_dgraphs_edge{v1 = a,
                          v2 = b,
                          weight = 1,
                          label = "A->B"}} =
                mt_dgraphs_server:edge_by(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:node_by_id(Graph, c),
        {error, not_found} = mt_dgraphs_server:edge_by(Graph, b, a).

node_exists_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        2 = mt_dgraphs_server:add_nodes(Graph, [{a, [{label, "A"}]}, b]),
        true = mt_dgraphs_server:node_exists(Graph, a),
        true = mt_dgraphs_server:node_exists(Graph, b),
        false = mt_dgraphs_server:node_exists(Graph, c),
        false = mt_dgraphs_server:node_exists(Graph, d),
        1 = mt_dgraphs_server:add_nodes(Graph, [c]),
        true = mt_dgraphs_server:node_exists(Graph, a),
        true = mt_dgraphs_server:node_exists(Graph, b),
        true = mt_dgraphs_server:node_exists(Graph, c),
        false = mt_dgraphs_server:node_exists(Graph, d).

add_nodes_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        2 = mt_dgraphs_server:add_nodes(Graph, [{a, [{label, "A"}]}, b]),
        0 = mt_dgraphs_server:add_nodes(Graph, [{a, [{label, "A"}]}, b]),
        2 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        {ok, "A"} = mt_dgraphs_server:get_node_label(Graph, a),
        {ok, nil} = mt_dgraphs_server:get_node_label(Graph, b),
        1 = mt_dgraphs_server:add_nodes(Graph, [c]),
        3 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        0 = mt_dgraphs_server:add_nodes(Graph, [a]).

set_nodes_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok = mt_dgraphs_server:set_nodes(Graph, [{a, [{label, "A"}]}, b]),
        2 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        {ok, "A"} = mt_dgraphs_server:get_node_label(Graph, a),
        {ok, nil} = mt_dgraphs_server:get_node_label(Graph, b),
        ok = mt_dgraphs_server:set_nodes(Graph, [c]),
        3 = mt_dgraphs_server:num_nodes(Graph),
        0 = mt_dgraphs_server:num_edges(Graph),
        ok = mt_dgraphs_server:set_nodes(Graph, [a]),
        {ok, nil} = mt_dgraphs_server:get_node_label(Graph, a).

nodes_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok = mt_dgraphs_server:set_nodes(Graph, [a, b, {c, [{label, "C"}]}]),
        [#mt_dgraphs_node{id = c, label = "C"},
         #mt_dgraphs_node{id = a, label = nil},
         #mt_dgraphs_node{id = b, label = nil}] =
                mt_dgraphs_server:nodes(Graph).

edges_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([a, d], [{b, c}, {c, d, [{label, "C->D"}, {weight, 4}]}]),
        [#mt_dgraphs_edge{v1 = c,
                          v2 = d,
                          weight = 4,
                          label = "C->D"},
         #mt_dgraphs_edge{v1 = b,
                          v2 = c,
                          weight = 1,
                          label = nil}] =
                mt_dgraphs_server:edges(Graph).

node_ids_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok = mt_dgraphs_server:set_nodes(Graph, [a, b, {c, [{label, "C"}]}]),
        [c, a, b] = mt_dgraphs_server:node_ids(Graph).

get_node_label_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok = mt_dgraphs_server:set_nodes(Graph, [a, b, {c, [{label, "C"}]}]),
        {ok, "C"} = mt_dgraphs_server:get_node_label(Graph, c),
        {ok, nil} = mt_dgraphs_server:get_node_label(Graph, a),
        {error, not_found} = mt_dgraphs_server:get_node_label(Graph, z).

get_edge_label_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok =
                mt_dgraphs_server:set_edges(Graph,
                                            [{a, b},
                                             {b, a, [{weight, 5}]},
                                             {c, d, [{label, "C->D"}]}]),
        {ok, "C->D"} = mt_dgraphs_server:get_edge_label(Graph, c, d),
        {ok, nil} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, a, c),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, c, b),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, z, a),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, a, z),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, z, f).

get_edge_weight_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok =
                mt_dgraphs_server:set_edges(Graph,
                                            [{a, b},
                                             {b, a, [{weight, 5}]},
                                             {c, d, [{label, "C->D"}]}]),
        {ok, 5} = mt_dgraphs_server:get_edge_weight(Graph, b, a),
        {ok, 1} = mt_dgraphs_server:get_edge_weight(Graph, c, d),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, a, c),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, c, b),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, z, a),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, a, z),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, z, f).

edges_from_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok =
                mt_dgraphs_server:set_edges(Graph,
                                            [{a, b},
                                             {b, a, [{weight, 5}]},
                                             {c, d, [{label, "C->D"}]},
                                             {a, c}]),
        ok = mt_dgraphs_server:set_node(Graph, x),
        {ok,
         [#mt_dgraphs_edge{v1 = b,
                           v2 = a,
                           weight = 5,
                           label = nil}]} =
                mt_dgraphs_server:edges_from(Graph, b),
        {ok,
         [#mt_dgraphs_edge{v1 = a,
                           v2 = c,
                           weight = 1,
                           label = nil},
          #mt_dgraphs_edge{v1 = a,
                           v2 = b,
                           weight = 1,
                           label = nil}]} =
                mt_dgraphs_server:edges_from(Graph, a),
        {ok, []} = mt_dgraphs_server:edges_from(Graph, x),
        {error, not_found} = mt_dgraphs_server:edges_from(Graph, z).

edges_to_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok =
                mt_dgraphs_server:set_edges(Graph,
                                            [{a, b, [{label, "A->B"}]},
                                             {b, a, [{weight, 5}]},
                                             {c, d},
                                             {d, b}]),
        ok = mt_dgraphs_server:set_node(Graph, x),
        {ok,
         [#mt_dgraphs_edge{v1 = a,
                           v2 = b,
                           weight = 1,
                           label = "A->B"},
          #mt_dgraphs_edge{v1 = d,
                           v2 = b,
                           weight = 1,
                           label = nil}]} =
                mt_dgraphs_server:edges_to(Graph, b),
        {ok,
         [#mt_dgraphs_edge{v1 = b,
                           v2 = a,
                           weight = 5,
                           label = nil}]} =
                mt_dgraphs_server:edges_to(Graph, a),
        {ok, []} = mt_dgraphs_server:edges_to(Graph, x),
        {error, not_found} = mt_dgraphs_server:edges_to(Graph, z).

path_exists_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([a, b, c, d, e, f, g],
                                      [{a, b}, {c, d}, {c, f}, {f, c}, {f, h}, {h, g}, {g, a}]),
        true = mt_dgraphs_server:path_exists(Graph, c, g),
        true = mt_dgraphs_server:path_exists(Graph, c, h),
        true = mt_dgraphs_server:path_exists(Graph, a, b),
        true = mt_dgraphs_server:path_exists(Graph, g, a),
        false = mt_dgraphs_server:path_exists(Graph, a, g),
        false = mt_dgraphs_server:path_exists(Graph, d, c),
        false = mt_dgraphs_server:path_exists(Graph, h, c),
        false = mt_dgraphs_server:path_exists(Graph, z, c),
        false = mt_dgraphs_server:path_exists(Graph, h, z).

path_to_unweighted_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([a, b, c, d, e, f, g],
                                      [{a, b},
                                       {c, d},
                                       {c, f},
                                       {f, c},
                                       {f, h},
                                       {f, g},
                                       {h, g},
                                       {g, a}]),
        {ok, [c, f, g]} = mt_dgraphs_server:path_to(Graph, c, g),
        {ok, [c, f, h]} = mt_dgraphs_server:path_to(Graph, c, h),
        {ok, [a, b]} = mt_dgraphs_server:path_to(Graph, a, b),
        {ok, [g, a]} = mt_dgraphs_server:path_to(Graph, g, a),
        {error, none} = mt_dgraphs_server:path_to(Graph, a, g),
        {error, none} = mt_dgraphs_server:path_to(Graph, d, c),
        {error, none} = mt_dgraphs_server:path_to(Graph, h, c),
        {error, not_found} = mt_dgraphs_server:path_to(Graph, z, c),
        {error, not_found} = mt_dgraphs_server:path_to(Graph, h, z).

path_to_weighted_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([a, b, c, d, e, f, g],
                                      [{a, b},
                                       {c, d},
                                       {c, f},
                                       {f, c},
                                       {f, h},
                                       {f, g, [{weight, 10}]},
                                       {h, g},
                                       {g, a}]),
        {ok, [c, f, h, g]} = mt_dgraphs_server:path_to(Graph, c, g),
        {ok, [c, f, h]} = mt_dgraphs_server:path_to(Graph, c, h),
        {ok, [a, b]} = mt_dgraphs_server:path_to(Graph, a, b),
        {ok, [g, a]} = mt_dgraphs_server:path_to(Graph, g, a),
        {error, none} = mt_dgraphs_server:path_to(Graph, a, g),
        {error, none} = mt_dgraphs_server:path_to(Graph, d, c),
        {error, none} = mt_dgraphs_server:path_to(Graph, h, c),
        {error, not_found} = mt_dgraphs_server:path_to(Graph, z, c),
        {error, not_found} = mt_dgraphs_server:path_to(Graph, h, z).

paths_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new([a, b, c, d, e, f, g],
                                      [{a, b},
                                       {c, d},
                                       {c, f},
                                       {f, c},
                                       {f, h},
                                       {f, g, [{weight, 10}]},
                                       {h, g},
                                       {g, a}]),
        {ok, [[c, f, h, g], [c, f, g]]} = mt_dgraphs_server:paths(Graph, c, g),
        {ok, [[c, f, h]]} = mt_dgraphs_server:paths(Graph, c, h),
        {ok, [[a, b]]} = mt_dgraphs_server:paths(Graph, a, b),
        {ok, [[g, a]]} = mt_dgraphs_server:paths(Graph, g, a),
        {ok, []} = mt_dgraphs_server:paths(Graph, a, g),
        {ok, []} = mt_dgraphs_server:paths(Graph, d, c),
        {ok, []} = mt_dgraphs_server:paths(Graph, h, c),
        {error, not_found} = mt_dgraphs_server:paths(Graph, z, c),
        {error, not_found} = mt_dgraphs_server:paths(Graph, h, z).

set_node_label_test() ->
        {ok, Graph} = mt_dgraphs_server:new([a, b, c, d], []),
        {ok, nil} = mt_dgraphs_server:get_node_label(Graph, a),
        {error, not_found} = mt_dgraphs_server:get_node_label(Graph, z),
        ok = mt_dgraphs_server:set_node_label(Graph, a, "Test"),
        {ok, "Test"} = mt_dgraphs_server:get_node_label(Graph, a),
        {error, not_found} = mt_dgraphs_server:get_node_label(Graph, z),
        ok = mt_dgraphs_server:set_node_label(Graph, z, "Zest"),
        {ok, "Test"} = mt_dgraphs_server:get_node_label(Graph, a),
        {ok, "Zest"} = mt_dgraphs_server:get_node_label(Graph, z).

set_edge_label_test() ->
        {ok, Graph} = mt_dgraphs_server:new([a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        {ok, nil} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, z, e),
        ok = mt_dgraphs_server:set_edge_label(Graph, {a, b}, "Test"),
        {ok, "Test"} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:get_edge_label(Graph, z, e),
        ok = mt_dgraphs_server:set_edge_label(Graph, {z, e}, "Zest"),
        {ok, "Test"} = mt_dgraphs_server:get_edge_label(Graph, a, b),
        {ok, "Zest"} = mt_dgraphs_server:get_edge_label(Graph, z, e).

set_edge_weight_test() ->
        {ok, Graph} = mt_dgraphs_server:new([a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        {ok, 1} = mt_dgraphs_server:get_edge_weight(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, z, e),
        ok = mt_dgraphs_server:set_edge_weight(Graph, {a, b}, 3),
        {ok, 3} = mt_dgraphs_server:get_edge_weight(Graph, a, b),
        {error, not_found} = mt_dgraphs_server:get_edge_weight(Graph, z, e),
        ok = mt_dgraphs_server:set_edge_weight(Graph, {z, e}, 4),
        {ok, 3} = mt_dgraphs_server:get_edge_weight(Graph, a, b),
        {ok, 4} = mt_dgraphs_server:get_edge_weight(Graph, z, e).

edge_exists_test() ->
        {ok, Graph} = mt_dgraphs_server:new(directed, [a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, z, e),
        false = mt_dgraphs_server:edge_exists(Graph, z, f),
        ok = mt_dgraphs_server:set_edge(Graph, z, e),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, z, e),
        false = mt_dgraphs_server:edge_exists(Graph, z, f).

delete_edge_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(directed,
                                      [],
                                      [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        true = mt_dgraphs_server:delete_edge(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, a, g).

delete_edges_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(directed,
                                      [],
                                      [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, c, d),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        2 = mt_dgraphs_server:delete_edges(Graph, [{a, b}, {c, d}]),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, c, d),
        true = mt_dgraphs_server:edge_exists(Graph, a, g).

delete_edge_undirected_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(undirected,
                                      [],
                                      [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, b, a),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        true = mt_dgraphs_server:delete_edge(Graph, a, b),
        false = mt_dgraphs_server:delete_edge(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, b, a),
        true = mt_dgraphs_server:edge_exists(Graph, a, g).

delete_edges_undirected_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(undirected,
                                      [],
                                      [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, b, a),
        true = mt_dgraphs_server:edge_exists(Graph, c, d),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        2 = mt_dgraphs_server:delete_edges(Graph, [{a, b}, {c, d}]),
        0 = mt_dgraphs_server:delete_edges(Graph, [{a, b}, {c, d}]),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, b, a),
        false = mt_dgraphs_server:edge_exists(Graph, c, d),
        true = mt_dgraphs_server:edge_exists(Graph, a, g).

delete_node_test() ->
        {ok, Graph} = mt_dgraphs_server:new(directed, [], [{a, b}, {c, d}, {e, f}, {a, g}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        true = mt_dgraphs_server:node_exists(Graph, a),
        true = mt_dgraphs_server:delete_node(Graph, a),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, a, g),
        false = mt_dgraphs_server:node_exists(Graph, a),
        true = mt_dgraphs_server:node_exists(Graph, g),
        {ok, []} = mt_dgraphs_server:edges_to(Graph, g),
        {ok, []} = mt_dgraphs_server:edges_from(Graph, g),
        true = mt_dgraphs_server:delete_node(Graph, g),
        false = mt_dgraphs_server:delete_node(Graph, g),
        false = mt_dgraphs_server:node_exists(Graph, g).

delete_nodes_test() ->
        {ok, Graph} = mt_dgraphs_server:new(directed, [], [{a, b}, {c, d}, {e, f}, {a, g}]),
        true = mt_dgraphs_server:edge_exists(Graph, a, b),
        true = mt_dgraphs_server:edge_exists(Graph, a, g),
        true = mt_dgraphs_server:node_exists(Graph, a),
        true = mt_dgraphs_server:node_exists(Graph, g),
        2 = mt_dgraphs_server:delete_nodes(Graph, [a, g]),
        0 = mt_dgraphs_server:delete_nodes(Graph, [a, g]),
        false = mt_dgraphs_server:edge_exists(Graph, a, b),
        false = mt_dgraphs_server:edge_exists(Graph, a, g),
        false = mt_dgraphs_server:node_exists(Graph, a),
        false = mt_dgraphs_server:node_exists(Graph, g).

in_degree_test() ->
        {ok, Graph} = mt_dgraphs_server:new(directed, [], [{a, b}, {c, d}, {f, g}, {a, g}]),
        {ok, 2} = mt_dgraphs_server:in_degree(Graph, g),
        {ok, 1} = mt_dgraphs_server:in_degree(Graph, b),
        {ok, 0} = mt_dgraphs_server:in_degree(Graph, a).

out_degree_test() ->
        {ok, Graph} = mt_dgraphs_server:new(directed, [], [{a, b}, {c, d}, {f, g}, {a, g}]),
        {ok, 0} = mt_dgraphs_server:out_degree(Graph, g),
        {ok, 1} = mt_dgraphs_server:out_degree(Graph, c),
        {ok, 2} = mt_dgraphs_server:out_degree(Graph, a).

info_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(directed,
                                      [{a, [{label, "A"}]}],
                                      [{a, b}, {b, c}, {c, d, [{label, "C->D"}]}, {a, e}]),
        #{type := Type,
          edges := Edges,
          nodes := Nodes} =
                mt_dgraphs_server:info(Graph),
        directed = Type,
        [#mt_dgraphs_edge{v1 = c,
                          v2 = d,
                          weight = 1,
                          label = "C->D"},
         #mt_dgraphs_edge{v1 = a,
                          v2 = b,
                          weight = 1,
                          label = nil},
         #mt_dgraphs_edge{v1 = a,
                          v2 = e,
                          weight = 1,
                          label = nil},
         #mt_dgraphs_edge{v1 = b,
                          v2 = c,
                          weight = 1,
                          label = nil}] =
                Edges,
        [#mt_dgraphs_node{id = c, label = nil},
         #mt_dgraphs_node{id = a, label = "A"},
         #mt_dgraphs_node{id = d, label = nil},
         #mt_dgraphs_node{id = b, label = nil},
         #mt_dgraphs_node{id = e, label = nil}] =
                Nodes.

raw_test() ->
        {ok, Graph} =
                mt_dgraphs_server:new(directed,
                                      [{a, [{label, "A"}]}],
                                      [{a, b}, {b, c}, {c, d, [{label, "C->D"}]}, {a, e}]),
        #{type := Type,
          edges := Edges,
          nodes := Nodes} =
                mt_dgraphs:info(
                        mt_dgraphs_server:raw_graph(Graph)),
        directed = Type,
        [#mt_dgraphs_edge{v1 = c,
                          v2 = d,
                          weight = 1,
                          label = "C->D"},
         #mt_dgraphs_edge{v1 = a,
                          v2 = b,
                          weight = 1,
                          label = nil},
         #mt_dgraphs_edge{v1 = a,
                          v2 = e,
                          weight = 1,
                          label = nil},
         #mt_dgraphs_edge{v1 = b,
                          v2 = c,
                          weight = 1,
                          label = nil}] =
                Edges,
        [#mt_dgraphs_node{id = c, label = nil},
         #mt_dgraphs_node{id = a, label = "A"},
         #mt_dgraphs_node{id = d, label = nil},
         #mt_dgraphs_node{id = b, label = nil},
         #mt_dgraphs_node{id = e, label = nil}] =
                Nodes.

second({_, V}) ->
        V.

is_acyclic_test() ->
        true = mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(undirected, []))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(undirected,
                                                                          [a, b, c, d, e, f, g],
                                                                          []))),
        false = mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(undirected, [{a, b}]))),
        true = mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed, []))),
        true = mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed, [{a, b}]))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b},
                                                                           {b, d},
                                                                           {a, c},
                                                                           {c, d}]))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [g],
                                                                          [{a, b}, {d, e}]))),
        false =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b}, {b, a}]))),
        false = mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed, [{a, a}]))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b},
                                                                           {b, c},
                                                                           {c, d},
                                                                           {e, f},
                                                                           {f, g},
                                                                           {g, h},
                                                                           {h, i},
                                                                           {i, j},
                                                                           {j, k},
                                                                           {k, l},
                                                                           {l, m},
                                                                           {m, n},
                                                                           {n, o},
                                                                           {o, p},
                                                                           {p, q},
                                                                           {q, r},
                                                                           {r, s},
                                                                           {s, t},
                                                                           {t, u}]))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b},
                                                                           {b, c},
                                                                           {c, d},
                                                                           {d, e},
                                                                           {f, g},
                                                                           {g, h},
                                                                           {h, i},
                                                                           {i, j},
                                                                           {j, k},
                                                                           {k, l},
                                                                           {l, m},
                                                                           {m, n},
                                                                           {n, o},
                                                                           {o, p},
                                                                           {p, q},
                                                                           {q, r},
                                                                           {r, s},
                                                                           {s, t},
                                                                           {t, u},
                                                                           {e, f}]))),
        true =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b},
                                                                           {b, c},
                                                                           {c, d},
                                                                           {d, e},
                                                                           {f, g},
                                                                           {g, h},
                                                                           {h, i},
                                                                           {i, j},
                                                                           {j, k},
                                                                           {k, l},
                                                                           {l, m},
                                                                           {m, n},
                                                                           {n, o},
                                                                           {o, p},
                                                                           {p, q},
                                                                           {q, r},
                                                                           {r, s},
                                                                           {s, t},
                                                                           {t, u},
                                                                           {u, a}]))),
        false =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new(directed,
                                                                          [{a, b},
                                                                           {b, c},
                                                                           {c, d},
                                                                           {d, e},
                                                                           {f, g},
                                                                           {g, h},
                                                                           {h, i},
                                                                           {i, j},
                                                                           {j, k},
                                                                           {k, l},
                                                                           {l, m},
                                                                           {m, n},
                                                                           {n, o},
                                                                           {o, p},
                                                                           {p, q},
                                                                           {q, r},
                                                                           {r, s},
                                                                           {s, t},
                                                                           {t, u},
                                                                           {e, f},
                                                                           {u, a}]))),
        false =
                mt_dgraphs_server:is_acyclic(second(mt_dgraphs_server:new([{g, h},
                                                                           {h, g},
                                                                           {a, b},
                                                                           {b, c}]))).

is_tree_test() ->
        true = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new(directed, [a], []))),
        true = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new(undirected))),
        true = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new(undirected, [a], []))),
        true = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new(directed, [a], []))),
        false = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new(undirected, [{a, b}]))),
        true = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b}]))),
        true =
                mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b},
                                                                        {a, c},
                                                                        {a, d},
                                                                        {a, e}]))),
        true =
                mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b},
                                                                        {a, c},
                                                                        {a, d},
                                                                        {a, e},
                                                                        {b, f},
                                                                        {b, g},
                                                                        {g, h}]))),
        false =
                mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b},
                                                                        {a, c},
                                                                        {a, d},
                                                                        {a, e},
                                                                        {b, f},
                                                                        {b, g},
                                                                        {g, h},
                                                                        {b, h}]))),
        false = mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b}, {b, a}]))),
        false =
                mt_dgraphs_server:is_tree(second(mt_dgraphs_server:new([{a, b},
                                                                        {b, c},
                                                                        {g, h},
                                                                        {h, g}]))).

is_subgraph_test() ->
        {ok, GSimple} = mt_dgraphs_server:new([{a, b}, {c, d}, {e, f}]),
        true = mt_dgraphs_server:is_subgraph(GSimple, GSimple),
        true =
                mt_dgraphs_server:is_subgraph(second(mt_dgraphs_server:new([{a, b},
                                                                            {c, d},
                                                                            {e, f}])),
                                              GSimple),
        true = mt_dgraphs_server:is_subgraph(GSimple, second(mt_dgraphs_server:new())),
        true =
                mt_dgraphs_server:is_subgraph(second(mt_dgraphs_server:new()),
                                              second(mt_dgraphs_server:new())),
        false = mt_dgraphs_server:is_subgraph(second(mt_dgraphs_server:new()), GSimple),
        true = mt_dgraphs_server:is_subgraph(GSimple, second(mt_dgraphs_server:new([{a, b}]))),
        false =
                mt_dgraphs_server:is_subgraph(GSimple,
                                              second(mt_dgraphs_server:new([{a,
                                                                             b,
                                                                             [{label, "A->B"}]}]))),
        true =
                mt_dgraphs_server:is_subgraph(GSimple,
                                              second(mt_dgraphs_server:new([{a,
                                                                             b,
                                                                             [{label, "A->B"}]}])),
                                              [{strict, false}]),
        true =
                mt_dgraphs_server:is_subgraph(second(mt_dgraphs_server:new([{a,
                                                                             b,
                                                                             [{label, "A->B"}]},
                                                                            {c, d},
                                                                            {e, f}])),
                                              second(mt_dgraphs_server:new([{a,
                                                                             b,
                                                                             [{label, "A->B"}]}]))),
        false =
                mt_dgraphs_server:is_subgraph(GSimple,
                                              second(mt_dgraphs_server:new([{a, [{label, "A"}]}],
                                                                           [{a, b}]))),
        true =
                mt_dgraphs_server:is_subgraph(GSimple,
                                              second(mt_dgraphs_server:new([{a, [{label, "A"}]}],
                                                                           [{a, b}])),
                                              [{strict, false}]),
        true =
                mt_dgraphs_server:is_subgraph(second(mt_dgraphs_server:new([{a, [{label, "A"}]}],
                                                                           [{a, b},
                                                                            {b, c},
                                                                            {c, e}])),
                                              second(mt_dgraphs_server:new([{a, [{label, "A"}]}],
                                                                           [{a, b}]))).

topsort_test() ->
        {ok, [a, c, b, d]} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new([{a, b},
                                                                        {b, d},
                                                                        {a, c},
                                                                        {c, d}]))),
        {ok, [h, a, c, d, g, b, e, f, k]} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new([{a, b},
                                                                        {a, c},
                                                                        {a, d},
                                                                        {d, f},
                                                                        {d, g},
                                                                        {h, a},
                                                                        {f, k},
                                                                        {a, e},
                                                                        {d, e},
                                                                        {e, f}]))),
        {ok, [a, b, d, c]} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new([{a, b},
                                                                        {a, c},
                                                                        {b, d},
                                                                        {d, c}]))),
        {ok, [1, 2, 3, 4, 5]} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new([{1, 2},
                                                                        {2, 3},
                                                                        {2, 4},
                                                                        {3, 4},
                                                                        {2, 5},
                                                                        {4, 5}]))),
        {ok, []} = mt_dgraphs_server:topsort(second(mt_dgraphs_server:new())),
        {error, cycle_detected} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new([{a, b}, {b, a}]))),
        {error, cycle_detected} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new(undirected, [{a, b}]))),
        {ok, [a, b]} =
                mt_dgraphs_server:topsort(second(mt_dgraphs_server:new(undirected, [a, b], []))).

load_graph_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        G = mt_dgraphs:new([{a, b}, {b, c}]),
        ok = mt_dgraphs_server:load_graph(Graph, G),
        2 = mt_dgraphs_server:num_edges(Graph).

udpate_graph_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        ok =
                mt_dgraphs_server:update_graph(Graph,
                                               fun(G) ->
                                                  {_, NG} = mt_dgraphs:add_nodes(G, [1, 2, 3]),
                                                  NG
                                               end),
        {error, _} = mt_dgraphs_server:update_graph(Graph, fun(G) -> G end, 0).

udpate_graph_2_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        _RLink = spawn_link(mt_dgraphs_server_test, sleep_and_change, [Graph, self(), 1]),
        timer:sleep(10),
        mt_dgraphs_server:set_node(Graph, z),
        receive
                M ->
                        {error, _} = M
        after 200 ->
                error(timeout)
        end.

udpate_graph_3_test() ->
        {ok, Graph} = mt_dgraphs_server:new(),
        _RLink = spawn_link(mt_dgraphs_server_test, sleep_and_change, [Graph, self(), 2]),
        timer:sleep(10),
        mt_dgraphs_server:set_node(Graph, z),
        receive
                M ->
                        ok = M
        after 200 ->
                error(timeout)
        end.

sleep_and_change(Graph, Report, Retries) ->
        Report
        ! mt_dgraphs_server:update_graph(Graph,
                                         fun(G) ->
                                            timer:sleep(100),
                                            {_, NG} = mt_dgraphs:add_nodes(G, [1, 2, 3]),
                                            NG
                                         end,
                                         Retries).
