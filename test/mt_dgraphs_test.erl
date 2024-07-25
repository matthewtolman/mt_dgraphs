-module(mt_dgraphs_test).

-include_lib("eunit/include/eunit.hrl").

-include("mt_dgraphs.hrl").

basic_test() ->
        {4, Graph} =
                mt_dgraphs:add_nodes(
                        mt_dgraphs:new(), [a, b, c, d]),
        NN = mt_dgraphs:num_nodes(Graph),
        ?assert(NN =:= 4).

new_type_test() ->
        Graph1 = mt_dgraphs:new(directed),
        T1 = mt_dgraphs:type(Graph1),
        ?assert(T1 =:= directed),
        Graph2 = mt_dgraphs:new(undirected),
        T2 = mt_dgraphs:type(Graph2),
        ?assert(T2 =:= undirected).

new_nodes_edges_test() ->
        Graph1 = mt_dgraphs:new([a, b, c], [{b, c}]),
        NE1 = mt_dgraphs:num_edges(Graph1),
        NN1 = mt_dgraphs:num_nodes(Graph1),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 3).

new_edges_test() ->
        Graph1 = mt_dgraphs:new(directed, [{a, b}]),
        Graph2 = mt_dgraphs:new(undirected, [{a, b}, {d, e}]),
        NE1 = mt_dgraphs:num_edges(Graph1),
        NE2 = mt_dgraphs:num_edges(Graph2),
        NN1 = mt_dgraphs:num_nodes(Graph1),
        NN2 = mt_dgraphs:num_nodes(Graph2),
        ?assert(NE1 =:= 1),
        ?assert(NE2 =:= 2),
        ?assert(NN1 =:= 2),
        ?assert(NN2 =:= 4).

new_all_test() ->
        Graph1 = mt_dgraphs:new(directed, [a, b, c], [{d, e}]),
        NE1 = mt_dgraphs:num_edges(Graph1),
        NN1 = mt_dgraphs:num_nodes(Graph1),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 5).

add_edges_test() ->
        Graph = mt_dgraphs:new(),
        NE1 = mt_dgraphs:num_edges(Graph),
        NN1 = mt_dgraphs:num_nodes(Graph),
        ?assert(NE1 =:= 0),
        ?assert(NN1 =:= 0),
        {2, Graph2} = mt_dgraphs:add_edges(Graph, [{a, b}, {b, c}]),
        NE2 = mt_dgraphs:num_edges(Graph2),
        NN2 = mt_dgraphs:num_nodes(Graph2),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        {2, Graph3} =
                mt_dgraphs:add_edges(
                        mt_dgraphs:new(undirected), [{a, b}, {b, c}]),
        NE3 = mt_dgraphs:num_edges(Graph3),
        NN3 = mt_dgraphs:num_nodes(Graph3),
        ?assert(NE3 =:= 2),
        ?assert(NN3 =:= 3),
        {0, Graph4} = mt_dgraphs:add_edges(Graph3, [{a, b, [{label, "Test"}]}]),
        {ok, nil} = mt_dgraphs:get_edge_label(Graph4, a, b).

add_edge_test() ->
        {ok, Graph} =
                mt_dgraphs:add_edge(
                        mt_dgraphs:new(), a, b),
        NE1 = mt_dgraphs:num_edges(Graph),
        NN1 = mt_dgraphs:num_nodes(Graph),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 2),
        {ok, Graph2} = mt_dgraphs:add_edge(Graph, c, a, [{label, "C->A"}]),
        NE2 = mt_dgraphs:num_edges(Graph2),
        NN2 = mt_dgraphs:num_nodes(Graph2),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        {already_exists, _} = mt_dgraphs:add_edge(Graph2, c, a, []).

set_edge_test() ->
        Graph = mt_dgraphs:set_edge(
                        mt_dgraphs:new(), a, b),
        NE1 = mt_dgraphs:num_edges(Graph),
        NN1 = mt_dgraphs:num_nodes(Graph),
        {ok, nil} = mt_dgraphs:get_edge_label(Graph, a, b),
        ?assert(NE1 =:= 1),
        ?assert(NN1 =:= 2),
        Graph2 = mt_dgraphs:set_edge(Graph, a, b, [{label, "A->B"}]),
        NE2 = mt_dgraphs:num_edges(Graph2),
        NN2 = mt_dgraphs:num_nodes(Graph2),
        ?assert(NE2 =:= 1),
        ?assert(NN2 =:= 2),
        {ok, "A->B"} = mt_dgraphs:get_edge_label(Graph2, a, b).

set_edges_test() ->
        Graph = mt_dgraphs:new(),
        NE1 = mt_dgraphs:num_edges(Graph),
        NN1 = mt_dgraphs:num_nodes(Graph),
        ?assert(NE1 =:= 0),
        ?assert(NN1 =:= 0),
        Graph2 = mt_dgraphs:set_edges(Graph, [{a, b}, {b, c}]),
        NE2 = mt_dgraphs:num_edges(Graph2),
        NN2 = mt_dgraphs:num_nodes(Graph2),
        ?assert(NE2 =:= 2),
        ?assert(NN2 =:= 3),
        Graph3 = mt_dgraphs:set_edges(
                         mt_dgraphs:new(undirected), [{a, b}, {b, c}]),
        NE3 = mt_dgraphs:num_edges(Graph3),
        NN2 = mt_dgraphs:num_nodes(Graph3),
        ?assert(NE3 =:= 2),
        ?assert(NN2 =:= 3),
        Graph4 = mt_dgraphs:set_edges(Graph3, [{a, b, [{label, "Test"}]}]),
        {ok, "Test"} = mt_dgraphs:get_edge_label(Graph4, a, b).

add_node_test() ->
        {ok, Graph} =
                mt_dgraphs:add_node(
                        mt_dgraphs:new(), a, [{label, "A"}]),
        1 = mt_dgraphs:num_nodes(Graph),
        0 = mt_dgraphs:num_edges(Graph),
        {ok, "A"} = mt_dgraphs:get_node_label(Graph, a),
        {ok, Graph2} = mt_dgraphs:add_node(Graph, b),
        2 = mt_dgraphs:num_nodes(Graph2),
        0 = mt_dgraphs:num_edges(Graph2),
        {already_exists, Graph2} = mt_dgraphs:add_node(Graph2, a).

by_id_test() ->
        Graph = mt_dgraphs:new([{a, [{label, "A"}]}, b], [{a, b, [{label, "A->B"}]}]),
        {ok, #mt_dgraphs_node{id = a, label = "A"}} = mt_dgraphs:node_by_id(Graph, a),
        {ok,
         #mt_dgraphs_edge{v1 = a,
                          v2 = b,
                          weight = 1,
                          label = "A->B"}} =
                mt_dgraphs:edge_by(Graph, a, b),
        {error, not_found} = mt_dgraphs:node_by_id(Graph, c),
        {error, not_found} = mt_dgraphs:edge_by(Graph, b, a).

node_exists_test() ->
        {2, Graph} =
                mt_dgraphs:add_nodes(
                        mt_dgraphs:new(), [{a, [{label, "A"}]}, b]),
        true = mt_dgraphs:node_exists(Graph, a),
        true = mt_dgraphs:node_exists(Graph, b),
        false = mt_dgraphs:node_exists(Graph, c),
        false = mt_dgraphs:node_exists(Graph, d),
        {1, Graph2} = mt_dgraphs:add_nodes(Graph, [c]),
        true = mt_dgraphs:node_exists(Graph2, a),
        true = mt_dgraphs:node_exists(Graph2, b),
        true = mt_dgraphs:node_exists(Graph2, c),
        false = mt_dgraphs:node_exists(Graph2, d).

add_nodes_test() ->
        {2, Graph} =
                mt_dgraphs:add_nodes(
                        mt_dgraphs:new(), [{a, [{label, "A"}]}, b]),
        2 = mt_dgraphs:num_nodes(Graph),
        0 = mt_dgraphs:num_edges(Graph),
        {ok, "A"} = mt_dgraphs:get_node_label(Graph, a),
        {ok, nil} = mt_dgraphs:get_node_label(Graph, b),
        {1, Graph2} = mt_dgraphs:add_nodes(Graph, [c]),
        3 = mt_dgraphs:num_nodes(Graph2),
        0 = mt_dgraphs:num_edges(Graph2),
        {0, Graph2} = mt_dgraphs:add_nodes(Graph2, [a]).

set_nodes_test() ->
        Graph = mt_dgraphs:set_nodes(
                        mt_dgraphs:new(), [{a, [{label, "A"}]}, b]),
        2 = mt_dgraphs:num_nodes(Graph),
        0 = mt_dgraphs:num_edges(Graph),
        {ok, "A"} = mt_dgraphs:get_node_label(Graph, a),
        {ok, nil} = mt_dgraphs:get_node_label(Graph, b),
        Graph2 = mt_dgraphs:set_nodes(Graph, [c]),
        3 = mt_dgraphs:num_nodes(Graph2),
        0 = mt_dgraphs:num_edges(Graph2),
        Graph3 = mt_dgraphs:set_nodes(Graph2, [a]),
        {ok, nil} = mt_dgraphs:get_node_label(Graph3, a).

nodes_test() ->
        Graph = mt_dgraphs:set_nodes(
                        mt_dgraphs:new(), [a, b, {c, [{label, "C"}]}]),
        [#mt_dgraphs_node{id = c, label = "C"},
         #mt_dgraphs_node{id = a, label = nil},
         #mt_dgraphs_node{id = b, label = nil}] =
                mt_dgraphs:nodes(Graph).

edges_test() ->
        Graph = mt_dgraphs:new([a, d], [{b, c}, {c, d, [{label, "C->D"}, {weight, 4}]}]),
        [#mt_dgraphs_edge{v1 = c,
                          v2 = d,
                          weight = 4,
                          label = "C->D"},
         #mt_dgraphs_edge{v1 = b,
                          v2 = c,
                          weight = 1,
                          label = nil}] =
                mt_dgraphs:edges(Graph).

node_ids_test() ->
        Graph = mt_dgraphs:set_nodes(
                        mt_dgraphs:new(), [a, b, {c, [{label, "C"}]}]),
        [c, a, b] = mt_dgraphs:node_ids(Graph).

get_node_label_test() ->
        Graph = mt_dgraphs:set_nodes(
                        mt_dgraphs:new(), [a, b, {c, [{label, "C"}]}]),
        {ok, "C"} = mt_dgraphs:get_node_label(Graph, c),
        {ok, nil} = mt_dgraphs:get_node_label(Graph, a),
        {error, not_found} = mt_dgraphs:get_node_label(Graph, z).

get_edge_label_test() ->
        Graph = mt_dgraphs:set_edges(
                        mt_dgraphs:new(),
                        [{a, b}, {b, a, [{weight, 5}]}, {c, d, [{label, "C->D"}]}]),
        {ok, "C->D"} = mt_dgraphs:get_edge_label(Graph, c, d),
        {ok, nil} = mt_dgraphs:get_edge_label(Graph, a, b),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, a, c),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, c, b),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, z, a),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, a, z),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, z, f).

get_edge_weight_test() ->
        Graph = mt_dgraphs:set_edges(
                        mt_dgraphs:new(),
                        [{a, b}, {b, a, [{weight, 5}]}, {c, d, [{label, "C->D"}]}]),
        {ok, 5} = mt_dgraphs:get_edge_weight(Graph, b, a),
        {ok, 1} = mt_dgraphs:get_edge_weight(Graph, c, d),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, a, c),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, c, b),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, z, a),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, a, z),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, z, f).

edges_from_test() ->
        Graph = mt_dgraphs:set_node(
                        mt_dgraphs:set_edges(
                                mt_dgraphs:new(),
                                [{a, b}, {b, a, [{weight, 5}]}, {c, d, [{label, "C->D"}]}, {a, c}]),
                        x),
        {ok,
         [#mt_dgraphs_edge{v1 = b,
                           v2 = a,
                           weight = 5,
                           label = nil}]} =
                mt_dgraphs:edges_from(Graph, b),
        {ok,
         [#mt_dgraphs_edge{v1 = a,
                           v2 = c,
                           weight = 1,
                           label = nil},
          #mt_dgraphs_edge{v1 = a,
                           v2 = b,
                           weight = 1,
                           label = nil}]} =
                mt_dgraphs:edges_from(Graph, a),
        {ok, []} = mt_dgraphs:edges_from(Graph, x),
        {error, not_found} = mt_dgraphs:edges_from(Graph, z).

edges_to_test() ->
        Graph = mt_dgraphs:set_node(
                        mt_dgraphs:set_edges(
                                mt_dgraphs:new(),
                                [{a, b, [{label, "A->B"}]}, {b, a, [{weight, 5}]}, {c, d}, {d, b}]),
                        x),
        {ok,
         [#mt_dgraphs_edge{v1 = a,
                           v2 = b,
                           weight = 1,
                           label = "A->B"},
          #mt_dgraphs_edge{v1 = d,
                           v2 = b,
                           weight = 1,
                           label = nil}]} =
                mt_dgraphs:edges_to(Graph, b),
        {ok,
         [#mt_dgraphs_edge{v1 = b,
                           v2 = a,
                           weight = 5,
                           label = nil}]} =
                mt_dgraphs:edges_to(Graph, a),
        {ok, []} = mt_dgraphs:edges_to(Graph, x),
        {error, not_found} = mt_dgraphs:edges_to(Graph, z).

path_exists_test() ->
        Graph = mt_dgraphs:new([a, b, c, d, e, f, g],
                               [{a, b}, {c, d}, {c, f}, {f, c}, {f, h}, {h, g}, {g, a}]),
        true = mt_dgraphs:path_exists(Graph, c, g),
        true = mt_dgraphs:path_exists(Graph, c, h),
        true = mt_dgraphs:path_exists(Graph, a, b),
        true = mt_dgraphs:path_exists(Graph, g, a),
        false = mt_dgraphs:path_exists(Graph, a, g),
        false = mt_dgraphs:path_exists(Graph, d, c),
        false = mt_dgraphs:path_exists(Graph, h, c),
        false = mt_dgraphs:path_exists(Graph, z, c),
        false = mt_dgraphs:path_exists(Graph, h, z).

path_to_unweighted_test() ->
        Graph = mt_dgraphs:new([a, b, c, d, e, f, g],
                               [{a, b}, {c, d}, {c, f}, {f, c}, {f, h}, {f, g}, {h, g}, {g, a}]),
        {ok, [c, f, g]} = mt_dgraphs:path_to(Graph, c, g),
        {ok, [c, f, h]} = mt_dgraphs:path_to(Graph, c, h),
        {ok, [a, b]} = mt_dgraphs:path_to(Graph, a, b),
        {ok, [g, a]} = mt_dgraphs:path_to(Graph, g, a),
        {error, none} = mt_dgraphs:path_to(Graph, a, g),
        {error, none} = mt_dgraphs:path_to(Graph, d, c),
        {error, none} = mt_dgraphs:path_to(Graph, h, c),
        {error, not_found} = mt_dgraphs:path_to(Graph, z, c),
        {error, not_found} = mt_dgraphs:path_to(Graph, h, z).

path_to_weighted_test() ->
        Graph = mt_dgraphs:new([a, b, c, d, e, f, g],
                               [{a, b},
                                {c, d},
                                {c, f},
                                {f, c},
                                {f, h},
                                {f, g, [{weight, 10}]},
                                {h, g},
                                {g, a}]),
        {ok, [c, f, h, g]} = mt_dgraphs:path_to(Graph, c, g),
        {ok, [c, f, h]} = mt_dgraphs:path_to(Graph, c, h),
        {ok, [a, b]} = mt_dgraphs:path_to(Graph, a, b),
        {ok, [g, a]} = mt_dgraphs:path_to(Graph, g, a),
        {error, none} = mt_dgraphs:path_to(Graph, a, g),
        {error, none} = mt_dgraphs:path_to(Graph, d, c),
        {error, none} = mt_dgraphs:path_to(Graph, h, c),
        {error, not_found} = mt_dgraphs:path_to(Graph, z, c),
        {error, not_found} = mt_dgraphs:path_to(Graph, h, z).

paths_test() ->
        Graph = mt_dgraphs:new([a, b, c, d, e, f, g],
                               [{a, b},
                                {c, d},
                                {c, f},
                                {f, c},
                                {f, h},
                                {f, g, [{weight, 10}]},
                                {h, g},
                                {g, a}]),
        {ok, [[c, f, h, g], [c, f, g]]} = mt_dgraphs:paths(Graph, c, g),
        {ok, [[c, f, h]]} = mt_dgraphs:paths(Graph, c, h),
        {ok, [[a, b]]} = mt_dgraphs:paths(Graph, a, b),
        {ok, [[g, a]]} = mt_dgraphs:paths(Graph, g, a),
        {ok, []} = mt_dgraphs:paths(Graph, a, g),
        {ok, []} = mt_dgraphs:paths(Graph, d, c),
        {ok, []} = mt_dgraphs:paths(Graph, h, c),
        {error, not_found} = mt_dgraphs:paths(Graph, z, c),
        {error, not_found} = mt_dgraphs:paths(Graph, h, z).

set_node_label_test() ->
        Graph = mt_dgraphs:new([a, b, c, d], []),
        {ok, nil} = mt_dgraphs:get_node_label(Graph, a),
        {error, not_found} = mt_dgraphs:get_node_label(Graph, z),
        Graph2 = mt_dgraphs:set_node_label(Graph, a, "Test"),
        {ok, "Test"} = mt_dgraphs:get_node_label(Graph2, a),
        {error, not_found} = mt_dgraphs:get_node_label(Graph2, z),
        Graph3 = mt_dgraphs:set_node_label(Graph2, z, "Zest"),
        {ok, "Test"} = mt_dgraphs:get_node_label(Graph3, a),
        {ok, "Zest"} = mt_dgraphs:get_node_label(Graph3, z).

set_edge_label_test() ->
        Graph = mt_dgraphs:new([a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        {ok, nil} = mt_dgraphs:get_edge_label(Graph, a, b),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph, z, e),
        Graph2 = mt_dgraphs:set_edge_label(Graph, {a, b}, "Test"),
        {ok, "Test"} = mt_dgraphs:get_edge_label(Graph2, a, b),
        {error, not_found} = mt_dgraphs:get_edge_label(Graph2, z, e),
        Graph3 = mt_dgraphs:set_edge_label(Graph2, {z, e}, "Zest"),
        {ok, "Test"} = mt_dgraphs:get_edge_label(Graph3, a, b),
        {ok, "Zest"} = mt_dgraphs:get_edge_label(Graph3, z, e).

set_edge_weight_test() ->
        Graph = mt_dgraphs:new([a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        {ok, 1} = mt_dgraphs:get_edge_weight(Graph, a, b),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph, z, e),
        Graph2 = mt_dgraphs:set_edge_weight(Graph, {a, b}, 3),
        {ok, 3} = mt_dgraphs:get_edge_weight(Graph2, a, b),
        {error, not_found} = mt_dgraphs:get_edge_weight(Graph2, z, e),
        Graph3 = mt_dgraphs:set_edge_weight(Graph2, {z, e}, 4),
        {ok, 3} = mt_dgraphs:get_edge_weight(Graph3, a, b),
        {ok, 4} = mt_dgraphs:get_edge_weight(Graph3, z, e).

edge_exists_test() ->
        Graph = mt_dgraphs:new(directed, [a, b, c, d], [{a, b}, {b, c}, {c, d}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        false = mt_dgraphs:edge_exists(Graph, z, e),
        false = mt_dgraphs:edge_exists(Graph, z, f),
        Graph2 = mt_dgraphs:set_edge(Graph, z, e),
        true = mt_dgraphs:edge_exists(Graph2, a, b),
        true = mt_dgraphs:edge_exists(Graph2, z, e),
        false = mt_dgraphs:edge_exists(Graph2, z, f).

delete_edge_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        {true, Graph2} = mt_dgraphs:delete_edge(Graph, a, b),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        true = mt_dgraphs:edge_exists(Graph2, a, g).

delete_edges_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, c, d),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        {2, Graph2} = mt_dgraphs:delete_edges(Graph, [{a, b}, {c, d}]),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        false = mt_dgraphs:edge_exists(Graph2, c, d),
        true = mt_dgraphs:edge_exists(Graph2, a, g).

delete_edge_undirected_test() ->
        Graph = mt_dgraphs:new(undirected, [], [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, b, a),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        {true, Graph2} = mt_dgraphs:delete_edge(Graph, a, b),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        false = mt_dgraphs:edge_exists(Graph2, b, a),
        true = mt_dgraphs:edge_exists(Graph2, a, g).

delete_edges_undirected_test() ->
        Graph = mt_dgraphs:new(undirected, [], [{a, b}, {c, d}, {d, e}, {a, g}, {e, g}, {g, f}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, b, a),
        true = mt_dgraphs:edge_exists(Graph, c, d),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        {2, Graph2} = mt_dgraphs:delete_edges(Graph, [{a, b}, {c, d}]),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        false = mt_dgraphs:edge_exists(Graph2, b, a),
        false = mt_dgraphs:edge_exists(Graph2, c, d),
        true = mt_dgraphs:edge_exists(Graph2, a, g).

delete_node_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {e, f}, {a, g}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        true = mt_dgraphs:node_exists(Graph, a),
        {true, Graph2} = mt_dgraphs:delete_node(Graph, a),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        false = mt_dgraphs:edge_exists(Graph2, a, g),
        false = mt_dgraphs:node_exists(Graph2, a),
        true = mt_dgraphs:node_exists(Graph2, g),
        {ok, []} = mt_dgraphs:edges_to(Graph2, g),
        {ok, []} = mt_dgraphs:edges_from(Graph2, g),
        {true, Graph3} = mt_dgraphs:delete_node(Graph2, g),
        {false, _} = mt_dgraphs:delete_node(Graph3, g),
        false = mt_dgraphs:node_exists(Graph3, g).

delete_nodes_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {e, f}, {a, g}]),
        true = mt_dgraphs:edge_exists(Graph, a, b),
        true = mt_dgraphs:edge_exists(Graph, a, g),
        true = mt_dgraphs:node_exists(Graph, a),
        true = mt_dgraphs:node_exists(Graph, g),
        {2, Graph2} = mt_dgraphs:delete_nodes(Graph, [a, g]),
        false = mt_dgraphs:edge_exists(Graph2, a, b),
        false = mt_dgraphs:edge_exists(Graph2, a, g),
        false = mt_dgraphs:node_exists(Graph2, a),
        false = mt_dgraphs:node_exists(Graph2, g).

in_degree_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {f, g}, {a, g}]),
        {ok, 2} = mt_dgraphs:in_degree(Graph, g),
        {ok, 1} = mt_dgraphs:in_degree(Graph, b),
        {ok, 0} = mt_dgraphs:in_degree(Graph, a).

out_degree_test() ->
        Graph = mt_dgraphs:new(directed, [], [{a, b}, {c, d}, {f, g}, {a, g}]),
        {ok, 0} = mt_dgraphs:out_degree(Graph, g),
        {ok, 1} = mt_dgraphs:out_degree(Graph, c),
        {ok, 2} = mt_dgraphs:out_degree(Graph, a).

info_test() ->
        Graph = mt_dgraphs:new(directed,
                               [{a, [{label, "A"}]}],
                               [{a, b}, {b, c}, {c, d, [{label, "C->D"}]}, {a, e}]),
        #{type := Type,
          edges := Edges,
          nodes := Nodes} =
                mt_dgraphs:info(Graph),
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

is_acyclic_test() ->
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(undirected, [])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(undirected, [a, b, c, d, e, f, g], [])),
        false =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(undirected, [{a, b}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [{a, b}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [{a, b}, {b, d}, {a, c}, {c, d}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [g], [{a, b}, {d, e}])),
        false =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [{a, b}, {b, a}])),
        false =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed, [{a, a}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed,
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
                                        {t, u}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed,
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
                                        {e, f}])),
        true =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed,
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
                                        {u, a}])),
        false =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new(directed,
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
                                        {u, a}])),
        false =
                mt_dgraphs:is_acyclic(
                        mt_dgraphs:new([{g, h}, {h, g}, {a, b}, {b, c}])).

is_tree_test() ->
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new(directed, [a], [])),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new(undirected)),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new(undirected, [a], [])),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new(directed, [a], [])),
        false =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new(undirected, [{a, b}])),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b}])),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b}, {a, c}, {a, d}, {a, e}])),
        true =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b}, {a, c}, {a, d}, {a, e}, {b, f}, {b, g}, {g, h}])),
        false =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b},
                                        {a, c},
                                        {a, d},
                                        {a, e},
                                        {b, f},
                                        {b, g},
                                        {g, h},
                                        {b, h}])),
        false =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b}, {b, a}])),
        false =
                mt_dgraphs:is_tree(
                        mt_dgraphs:new([{a, b}, {b, c}, {g, h}, {h, g}])).

is_subgraph_test() ->
        GSimple = mt_dgraphs:new([{a, b}, {c, d}, {e, f}]),
        true = mt_dgraphs:is_subgraph(GSimple, GSimple),
        true =
                mt_dgraphs:is_subgraph(
                        mt_dgraphs:new([{a, b}, {c, d}, {e, f}]), GSimple),
        true = mt_dgraphs:is_subgraph(GSimple, mt_dgraphs:new()),
        true =
                mt_dgraphs:is_subgraph(
                        mt_dgraphs:new(), mt_dgraphs:new()),
        false =
                mt_dgraphs:is_subgraph(
                        mt_dgraphs:new(), GSimple),
        true = mt_dgraphs:is_subgraph(GSimple, mt_dgraphs:new([{a, b}])),
        false = mt_dgraphs:is_subgraph(GSimple, mt_dgraphs:new([{a, b, [{label, "A->B"}]}])),
        true =
                mt_dgraphs:is_subgraph(GSimple,
                                       mt_dgraphs:new([{a, b, [{label, "A->B"}]}]),
                                       [{strict, false}]),
        true =
                mt_dgraphs:is_subgraph(
                        mt_dgraphs:new([{a, b, [{label, "A->B"}]}, {c, d}, {e, f}]),
                        mt_dgraphs:new([{a, b, [{label, "A->B"}]}])),
        false = mt_dgraphs:is_subgraph(GSimple, mt_dgraphs:new([{a, [{label, "A"}]}], [{a, b}])),
        true =
                mt_dgraphs:is_subgraph(GSimple,
                                       mt_dgraphs:new([{a, [{label, "A"}]}], [{a, b}]),
                                       [{strict, false}]),
        true =
                mt_dgraphs:is_subgraph(
                        mt_dgraphs:new([{a, [{label, "A"}]}], [{a, b}, {b, c}, {c, e}]),
                        mt_dgraphs:new([{a, [{label, "A"}]}], [{a, b}])).

topsort_test() ->
        {ok, [a, c, b, d]} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new([{a, b}, {b, d}, {a, c}, {c, d}])),
        {ok, [h, a, c, d, g, b, e, f, k]} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new([{a, b},
                                        {a, c},
                                        {a, d},
                                        {d, f},
                                        {d, g},
                                        {h, a},
                                        {f, k},
                                        {a, e},
                                        {d, e},
                                        {e, f}])),
        {ok, [a, b, d, c]} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new([{a, b}, {a, c}, {b, d}, {d, c}])),
        {ok, [1, 2, 3, 4, 5]} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new([{1, 2}, {2, 3}, {2, 4}, {3, 4}, {2, 5}, {4, 5}])),
        {ok, []} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new()),
        {error, cycle_detected} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new([{a, b}, {b, a}])),
        {error, cycle_detected} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new(undirected, [{a, b}])),
        {ok, [a, b]} =
                mt_dgraphs:topsort(
                        mt_dgraphs:new(undirected, [a, b], [])).
