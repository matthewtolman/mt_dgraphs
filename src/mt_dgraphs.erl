%% ----------------------------------
%%
%% @author Matthew Tolman
%% @copyright 2024 Matthew Tolman
%% @doc Mathematical graph library for representing nodes and connections. Has graph traversals, graph modifications, and graph querying methods.
%% @end
%% @version 0.0.1
%% @end
%%
%% ----------------------------------
-module(mt_dgraphs).

-export([new/0, new/1, new/2, new/3, add_edge/3, add_edge/4, set_edge/3, set_edge/4,
         add_edges/2, set_edges/2, add_node/2, add_node/3, set_node/2, set_node/3, set_nodes/2,
         add_nodes/2, nodes/1, edges/1, node_ids/1, node_by_id/2, edge_by/3, get_node_label/2,
         get_edge_label/3, get_edge_weight/3, edges_from/2, edges_to/2, path_exists/3, path_to/3,
         paths/3, bfs_path_to/3, set_edge_weight/3, set_edge_label/3, set_node_label/3,
         edge_exists/3, node_exists/2, roots/1, is_empty/1, delete_edge/3, delete_edges/2,
         delete_node/2, delete_nodes/2, in_degree/2, info/1, is_acyclic/1, is_cyclic/1, is_dag/1,
         is_subgraph/3, is_subgraph/2, is_tree/1, num_edges/1, num_nodes/1, out_degree/2,
         split_edge/4, topsort/1, transpose/1, type/1]).

         %% To Implement
         % k_cliques/1,
         % loop_nodes/1,
         % neighbors/2,
         % postorder/1,
         % postorder/2,
         % preorder/1Node,
         % preorder/2,
         % strong_components/1,
         % subgraph/2,
         % to_dot/1,
         % to_edgelist/1,

-export([node_id/1, node_label/1, edge_label/1, edge_weight/1, edge_start/1, edge_end/1]).

-define(MIN, 0.00001).
-define(MAX_DEPTH, 400).

-type graph_type() :: directed | undirected.
-type edge_weight() :: integer() | float().
-type edge_option() :: {weight, edge_weight()} | {label, any()}.
-type edge_options() :: [edge_option()].
-type node_option() :: {label, any()}.
-type node_options() :: [node_option()].
-type input_node() :: atom() | {atom(), node_options()}.
-type input_edge() :: {atom(), atom()} | {atom(), atom(), edge_options()}.
-type input_type() :: graph_type().
-type input_edges() :: [input_edge()].
-type input_nodes() :: [input_node()].

-include("mt_dgraphs.hrl").

-type graph() :: #mt_dgraphs{}.
-type graph_node() :: #mt_dgraphs_node{}.
-type graph_nodes() :: [graph_node()] | [].
-type graph_edge() :: #mt_dgraphs_edge{}.
-type graph_edges() :: [graph_edge()] | [].
-type subgraph_opts() :: [{strict, boolean()}].

-export_type([input_node/0, input_edge/0, input_type/0, graph_type/0, graph/0,
              graph_node/0, graph_edge/0, graph_nodes/0, graph_edges/0, edge_weight/0,
              input_edges/0, input_nodes/0, subgraph_opts/0, node_options/0, edge_options/0]).

%% ----------------------------------
%%
%% @doc Gets the ID for a node.
%% @end
%% @param Node The graph node to get the ID for.
%% @returns Node ID.
%% @end
%%
%% ----------------------------------
-spec node_id(graph_node()) -> atom().
node_id(_Node = #mt_dgraphs_node{id = Id}) ->
    Id.

%% ----------------------------------
%%
%% @doc Gets the label for a node.
%% @end
%% @param Node The graph node to operate on.
%% @returns Node label.
%% @end
%%
%% ----------------------------------
-spec node_label(graph_node()) -> any().
node_label(_Node = #mt_dgraphs_node{label = Label}) ->
    Label.

%% ----------------------------------
%%
%% @doc Gets the label for an edge.
%% @end
%% @param Edge Graph edge to operate on.
%% @returns Edge label.
%% @end
%%
%% ----------------------------------
-spec edge_label(graph_edge()) -> any().
edge_label(_Edge = #mt_dgraphs_edge{label = Label}) ->
    Label.

%% ----------------------------------
%%
%% @doc Gets the weight for an edge.
%% @end
%% @param Edge Graph edge to operate on.
%% @returns Edge weight.
%% @end
%%
%% ----------------------------------
-spec edge_weight(graph_edge()) -> edge_weight().
edge_weight(_Edge = #mt_dgraphs_edge{weight = Weight}) ->
    Weight.

%% ----------------------------------
%%
%% @doc Get starting node ID for an edge.
%% @end
%% @param Edge Edge to operate on.
%% @returns Node ID.
%% @end
%%
%% ----------------------------------
-spec edge_start(graph_edge()) -> atom().
edge_start(_Edge = #mt_dgraphs_edge{v1 = S}) ->
    S.

%% ----------------------------------
%%
%% @doc Gets the ending node ID for an edge.
%% @end
%% @param Edge Edge to operate on.
%% @returns Node ID.
%% @end
%%
%% ----------------------------------
-spec edge_end(graph_edge()) -> atom().
edge_end(_Edge = #mt_dgraphs_edge{v2 = E}) ->
    E.

%% ----------------------------------
%%
%% @doc Creates a new graph.
%% @end
%% @returns A graph.
%% @end
%%
%% ----------------------------------
-spec new() -> graph().
new() ->
    form_graph(directed, [], []).

%% ----------------------------------
%%
%% @doc Creates a new graph.
%% @end
%% @param Type_Or_Edges Type of graph (directed or undirected) or a list of graph edges.
%% @returns A graph.
%% @end
%%
%% ----------------------------------
-spec new(graph_type() | input_edges()) -> graph().
new(_Type_Or_Edges = directed) ->
    form_graph(directed, [], []);
new(_Type = undirected) ->
    form_graph(undirected, [], []);
new(Edges) ->
    form_graph(directed, [], Edges).

%% ----------------------------------
%%
%% @doc Creates a new graph.
%% @end
%% @param Type_Or_Nodes Type of graph (directed or undirected) or a list of graph nodes.
%% @param Edges A list of graph edges.
%% @returns A graph.
%% @end
%%
%% ----------------------------------
-spec new(graph_type() | input_nodes(), input_edges()) -> graph().
new(_Type_Or_Nodes = directed, Edges) when is_list(Edges) ->
    form_graph(directed, [], Edges);
new(undirected, Edges) when is_list(Edges) ->
    form_graph(undirected, [], Edges);
new(Nodes, Edges) when is_list(Nodes), is_list(Edges) ->
    form_graph(directed, Nodes, Edges).

%% ----------------------------------
%%
%% @doc Creates a new graph.
%% @end
%% @param Type The type of graph (directed or undirected).
%% @param Nodes The nodes of the graph.
%% @param Edges The edges of the graph.
%% @returns A graph.
%% @end
%%
%% ----------------------------------
-spec new(graph_type(), input_nodes(), input_edges()) -> graph().
new(_Type = directed, Nodes, Edges) when is_list(Nodes), is_list(Edges) ->
    form_graph(directed, Nodes, Edges);
new(undirected, Nodes, Edges) when is_list(Nodes), is_list(Edges) ->
    form_graph(undirected, Nodes, Edges).

%% ----------------------------------
%%
%% @doc Gets a node by ID.
%% @end
%% @param Graph The graph to search.
%% @param NodeId The ID of the node to get.
%% @returns {ok, NodeRecord} if found, {error, not_found} if the node doesn't exist.
%% @end
%%
%% ----------------------------------
-spec node_by_id(graph(), atom()) -> {ok, graph_node()} | {error, not_found}.
node_by_id(_Graph = #mt_dgraphs{nodes = Nodes}, NodeId) ->
    case maps:is_key(NodeId, Nodes) of
        true ->
            {ok, format_node(NodeId, maps:get(NodeId, Nodes))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Gets an edge by start and end node ids.
%% @end
%% @param Graph The graph to search.
%% @param Start The ID of the starting node.
%% @param End The ID of the ending node.
%% @returns {ok, EdgeRecord} if found, {error, not_found} if the node doesn't exist.
%% @end
%%
%% ----------------------------------
-spec edge_by(graph(), atom(), atom()) -> {ok, graph_edge()} | {error, not_found}.
edge_by(_Graph = #mt_dgraphs{edges = Edges}, Start, End) ->
    S = maps:get(Start, Edges, #{}),
    case maps:is_key(End, S) of
        true ->
            {ok, format_edge(Start, End, maps:get(End, S))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Get the graph type.
%% @end
%% @param Graph Graph to operate on.
%% @returns Type of graph (directed or undirected).
%% @end
%%
%% ----------------------------------
-spec type(graph()) -> graph_type().
type(_Graph = #mt_dgraphs{type = Type}) ->
    Type.

%% ----------------------------------
%%
%% @doc Sets the label for a node in the graph.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId The ID of the node to modify.
%% @param Label The node label.
%% @returns A new graph with the change made.
%% @end
%%
%% ----------------------------------
-spec set_node_label(graph(), atom(), any()) -> graph().
set_node_label(Graph = #mt_dgraphs{}, NodeId, Label) ->
    NewGraph =
        case add_node(Graph, NodeId) of
            {ok, G} ->
                G;
            _ ->
                Graph
        end,
    #mt_dgraphs{nodes = Nodes} = NewGraph,
    NodeList = maps:get(NodeId, Nodes),
    NewNodeList = lists:keystore(label, 1, NodeList, {label, Label}),
    NewNodes = maps:put(NodeId, NewNodeList, Nodes),
    #mt_dgraphs{nodes = NewNodes}.

%% ----------------------------------
%%
%% @doc Sets the weight for an edge in the graph.
%% @end
%% @param Graph Graph to operate on.
%% @param Edge The edge to operate on, represented by the tuple {Start, End}.
%% @param Weight The edge weight.
%% @returns A new graph with the change made.
%% @end
%%
%% ----------------------------------
-spec set_edge_weight(graph(), {atom(), atom()}, edge_weight()) -> graph().
set_edge_weight(Graph = #mt_dgraphs{}, _Edge = {Start, End}, Weight) ->
    NewGraph =
        case add_edge(Graph, Start, End) of
            {ok, G} ->
                G;
            _ ->
                Graph
        end,
    #mt_dgraphs{edges = Edges} = NewGraph,
    Outgoing = maps:get(Start, Edges),
    Edge = maps:get(End, Outgoing),
    NewEdge = lists:keystore(weight, 1, Edge, {weight, Weight}),
    NewOutgoing = maps:put(End, NewEdge, Outgoing),
    NewEdges = maps:put(Start, NewOutgoing, Edges),
    NewGraph#mt_dgraphs{edges = NewEdges}.

%% ----------------------------------
%%
%% @doc Sets the label for an edge in the graph.
%% @end
%% @param Graph Graph to operate on.
%% @param Edge The edge to operate on, represented by the tuple {Start, End}.
%% @param Label The edge label.
%% @returns A new graph with the change made.
%% @end
%%
%% ----------------------------------
-spec set_edge_label(graph(), {atom(), atom()}, any()) -> graph().
set_edge_label(Graph = #mt_dgraphs{}, _Edge = {Start, End}, Label) ->
    NewGraph =
        case add_edge(Graph, Start, End) of
            {ok, G} ->
                G;
            _ ->
                Graph
        end,
    #mt_dgraphs{edges = Edges} = NewGraph,
    Outgoing = maps:get(Start, Edges),
    Edge = maps:get(End, Outgoing),
    NewEdge = lists:keystore(label, 1, Edge, {label, Label}),
    NewOutgoing = maps:put(End, NewEdge, Outgoing),
    NewEdges = maps:put(Start, NewOutgoing, Edges),
    NewGraph#mt_dgraphs{edges = NewEdges}.

%% ----------------------------------
%%
%% @doc Set an edge's data to the default label and weight. Will insert the edge if not present.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Node ID for the start of the edge.
%% @param End Node ID for the end of the edge.
%% @returns A graph with the changes made.
%% @end
%%
%% ----------------------------------
-spec set_edge(graph(), atom(), atom()) -> graph().
set_edge(Graph = #mt_dgraphs{}, Start, End) ->
    set_edge(Graph, Start, End, []).

%% ----------------------------------
%%
%% @doc Set an edge's data to the given label and/or weight. Will insert the edge if not present.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Node ID for the start of the edge.
%% @param End Node ID for the end of the edge.
%% @param Opts A list of edge options. {label, Label} and {weight, Weight} are possible options.
%% @returns A graph with the changes made.
%% @end
%%
%% ----------------------------------
-spec set_edge(graph(), atom(), atom(), edge_options()) -> graph().
set_edge(Graph =
             #mt_dgraphs{type = Type,
                         nodes = Nodes,
                         edges = Edges},
         Start,
         End,
         Opts) ->
    NewEdgeGraph = edge_map(Type, [{Start, End, filter_default_edge_opts(Opts)}]),
    NodesWithStart =
        case maps:is_key(Start, Nodes) of
            false ->
                maps:put(Start, [], Nodes);
            _ ->
                Nodes
        end,
    NodesWithEnd =
        case maps:is_key(End, NodesWithStart) of
            false ->
                maps:put(End, [], NodesWithStart);
            _ ->
                NodesWithStart
        end,
    Graph#mt_dgraphs{nodes = NodesWithEnd,
                     edges =
                         maps:merge_with(fun(_, V1, V2) -> maps:merge(V1, V2) end,
                                         Edges,
                                         NewEdgeGraph)}.

%% ----------------------------------
%%
%% @doc Adds an edge to a graph only if it does not yet exist.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Node ID for the edge start.
%% @param End Node ID for the edge end.
%% @returns {ok, Graph} if a node was added, {already_exists, Graph} if a node was not added.
%% @end
%%
%% ----------------------------------
-spec add_edge(graph(), atom(), atom()) -> {ok, graph()} | {already_exists, graph()}.
add_edge(Graph = #mt_dgraphs{}, Start, End) ->
    add_edge(Graph, Start, End, []).

%% ----------------------------------
%%
%% @doc Adds an edge to a graph only if it does not yet exist.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Node ID for the edge start.
%% @param End Node ID for the edge end.
%% @param Opts A list of options for the edge. Possible options: {label, Label}, {weight: Weight}.
%% @returns {ok, Graph} if a node was added, {already_exists, Graph} if a node was not added.
%% @end
%%
%% ----------------------------------
-spec add_edge(graph(), atom(), atom(), edge_options()) ->
                  {ok, graph()} | {already_exists, graph()}.
add_edge(Graph = #mt_dgraphs{}, Start, End, Opts) ->
    case edge_exists(Graph, Start, End) of
        false ->
            {ok, set_edge(Graph, Start, End, Opts)};
        true ->
            {already_exists, Graph}
    end.

%% ----------------------------------
%%
%% @doc Sets node data in a graph with a default label. Will insert a node if not present.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId ID of node to set.
%% @returns Graph with the changes made.
%% @end
%%
%% ----------------------------------
-spec set_node(graph(), atom()) -> graph().
set_node(Graph = #mt_dgraphs{}, NodeId) ->
    set_node(Graph, NodeId, []).

%% ----------------------------------
%%
%% @doc Sets node data in a graph with the provided options. Will insert a node if not present.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId ID of node to set.
%% @param Opts List of options to use for setting additional node data. Possible options: {label, Label}.
%% @returns Graph with the changes made.
%% @end
%%
%% ----------------------------------
-spec set_node(graph(), atom(), node_options()) -> graph().
set_node(Graph = #mt_dgraphs{nodes = Nodes}, NodeId, Opts) ->
    Graph#mt_dgraphs{nodes = maps:put(NodeId, filter_default_node_opts(Opts), Nodes)}.

%% ----------------------------------
%%
%% @doc Sets node data for a list of nodes. Will insert any nodes that don't exist and overwite any that do exist.
%% @end
%% @param Graph Graph to operate on.
%% @param Nodes List of nodes to set. Elements are of either NodeId :: atom() or {NodeId :: atom(), Options :: node_options()}.
%% @returns The updated graph.
%% @end
%%
%% ----------------------------------
-spec set_nodes(graph(), [input_node()]) -> graph().
set_nodes(Graph = #mt_dgraphs{}, _Nodes = [{Node, Opt} | Rest]) ->
    set_nodes(set_node(Graph, Node, Opt), Rest);
set_nodes(Graph = #mt_dgraphs{}, [Node | Rest]) when is_atom(Node) ->
    set_nodes(set_node(Graph, Node), Rest);
set_nodes(Graph = #mt_dgraphs{}, []) ->
    Graph.

%% ----------------------------------
%%
%% @doc Adds nodes to a graph. Will ignore any nodes that already existed.
%% @end
%% @param Graph Graph to operate on.
%% @param Nodes List of nodes to add. Elements are of either NodeId :: atom() or {NodeId :: atom(), Options :: node_options()}.
%% @returns The number of inserted nodes and the updated graph.
%% @end
%%
%% ----------------------------------
-spec add_nodes(graph(), input_nodes()) -> {integer(), graph()}.
add_nodes(Graph = #mt_dgraphs{}, Nodes) ->
    lists:foldl(fun(Node, {Count, G}) ->
                   case add_node_impl(G, Node) of
                       {ok, NG} -> {Count + 1, NG};
                       _ -> {Count, G}
                   end
                end,
                {0, Graph},
                Nodes).

add_node_impl(Graph, {NodeId, Opts}) ->
    mt_dgraphs:add_node(Graph, NodeId, Opts);
add_node_impl(Graph, Node) ->
    mt_dgraphs:add_node(Graph, Node).

%% ----------------------------------
%%
%% @doc Adds a node to the graph if it doesn't already exist.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId ID of node to add.
%% @returns {ok, Graph} if node was added, {already_exists, Graph} if node was not added.
%% @end
%%
%% ----------------------------------
-spec add_node(graph(), atom()) -> {ok, graph()} | {already_exists, graph()}.
add_node(Graph = #mt_dgraphs{}, NodeId) ->
    add_node(Graph, NodeId, []).

%% ----------------------------------
%%
%% @doc Adds a node to the graph if it doesn't already exist.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId ID of node to add.
%% @param Opts List of node options. Possible options: {label, Label}.
%% @returns {ok, Graph} if node was added, {already_exists, Graph} if node was not added.
%% @end
%%
%% ----------------------------------
-spec add_node(graph(), atom(), node_options()) ->
                  {ok, graph()} | {already_exists, graph()}.
add_node(Graph = #mt_dgraphs{}, NodeId, Opts) ->
    case node_exists(Graph, NodeId) of
        false ->
            {ok, set_node(Graph, NodeId, Opts)};
        true ->
            {already_exists, Graph}
    end.

%% ----------------------------------
%%
%% @doc Sets multiple edges in a graph. Will overwrite existing edges and add non-existing edges.
%% @end
%% @param Graph Graph edit.
%% @param Edges List of edges to add to the node. Format is either {Start :: atom(), End :: atom()} or {Start :: atom(), End :: atom(), Opts :: edge_options()}.
%% @returns Updated graph.
%% @end
%%
%% ----------------------------------
-spec set_edges(graph(), input_edges()) -> graph().
set_edges(Graph = #mt_dgraphs{}, _Edges = [{Start, End, Opts} | Rest]) ->
    set_edges(set_edge(Graph, Start, End, Opts), Rest);
set_edges(Graph = #mt_dgraphs{}, [{Start, End} | Rest]) ->
    set_edges(set_edge(Graph, Start, End), Rest);
set_edges(Graph = #mt_dgraphs{}, []) ->
    Graph.

%% ----------------------------------
%%
%% @doc
%% Adds multiple edges in a graph. Will ignore existing edges and add non-existing edges.
%% @end
%% @param Graph Graph to edit.
%% @param Edges List of edges to add to the node. Format is either {Start :: atom(), End :: atom()} or {Start :: atom(), End :: atom(), Opts :: edge_options()}.
%% @returns Number of inserted edges and the updated graph.
%% @end
%%
%% ----------------------------------
-spec add_edges(graph(), input_edges()) -> {integer(), graph()}.
add_edges(Graph = #mt_dgraphs{}, Edges) ->
    lists:foldl(fun(Edge, {Count, G}) ->
                   case add_edge(G, Edge) of
                       {ok, NG} -> {Count + 1, NG};
                       _ -> {Count, G}
                   end
                end,
                {0, Graph},
                Edges).

add_edge(Graph, {Start, End}) ->
    add_edge(Graph, Start, End, []);
add_edge(Graph, {Start, End, Opts}) ->
    add_edge(Graph, Start, End, Opts).

%% ----------------------------------
%%
%% @doc Gets the edges for a graph.
%% @end
%% @returns Return.
%% @end
%%
%% ----------------------------------
-spec edges(graph()) -> graph_edges().
edges(_Graph = #mt_dgraphs{edges = Edges}) ->
    format_edges(Edges).

%% ----------------------------------
%%
%% @doc Gets nodes for a graph.
%% @end
%% @param Graph Graph to get nodes for.
%% @returns List of nodes.
%% @end
%%
%% ----------------------------------
-spec nodes(graph()) -> graph_nodes().
nodes(_Graph = #mt_dgraphs{nodes = Nodes}) ->
    format_nodes(Nodes).

%% ----------------------------------
%%
%% @doc Gets list of all node ids.
%% @end
%% @param Graph Graph to query.
%% @returns List of all node ids.
%% @end
%%
%% ----------------------------------
-spec node_ids(graph()) -> [atom()].
node_ids(_Graph = #mt_dgraphs{nodes = Nodes}) ->
    maps:keys(Nodes).

%% ----------------------------------
%%
%% @doc Gets the weight for an edge.
%% @end
%% @param Graph Graph to Query.
%% @param Start Starting node id for the edge.
%% @param End Ending node id for the edge.
%% @returns {ok, Weight} if the edge exists or  {error, not_found} if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec get_edge_weight(graph(), atom(), atom()) ->
                         {ok, edge_weight()} | {error, not_found}.
get_edge_weight(_Graph = #mt_dgraphs{edges = Edges}, Start, End) ->
    StartMap = maps:get(Start, Edges, #{}),
    case maps:is_key(End, StartMap) of
        true ->
            {ok, opt_weight(maps:get(End, StartMap))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Gets the label for an edge.
%% @end
%% @param Graph Graph to Query.
%% @param Start Starting node id for the edge.
%% @param End Ending node id for the edge.
%% @returns {ok, Label} if the edge exists or {error, not_found} if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec get_edge_label(graph(), atom(), atom()) -> {ok, any()} | {error, not_found}.
get_edge_label(_Graph = #mt_dgraphs{edges = Edges}, Start, End) ->
    StartMap = maps:get(Start, Edges, #{}),
    case maps:is_key(End, StartMap) of
        true ->
            {ok, opt_label(maps:get(End, StartMap))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Gets the label tied to a node.
%% @end
%% @param Graph Graph to query.
%% @param NodeId ID of node to get label for.
%% @returns {ok, Label} if node exists or {error, not_found} if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec get_node_label(graph(), atom()) -> {ok, any()} | {error, not_found}.
get_node_label(_Graph = #mt_dgraphs{nodes = Nodes}, NodeId) ->
    case maps:is_key(NodeId, Nodes) of
        true ->
            {ok, opt_label(maps:get(NodeId, Nodes))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Gets the edges coming from a node.
%% @end
%% @param Graph Graph to query.
%% @param NodeId ID of node to query.
%% @returns {ok, Edges} if node exists or {error, not_found} if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec edges_from(graph(), atom()) -> {ok, graph_edges()} | {error, not_found}.
edges_from(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}, NodeId) ->
    case {maps:is_key(NodeId, Edges), maps:is_key(NodeId, Nodes)} of
        {true, _} ->
            {ok, format_edges_from(NodeId, maps:get(NodeId, Edges))};
        {_, true} ->
            {ok, []};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Gets the edges going to a node.
%% @end
%% @param Graph Graph to query.
%% @param NodeId ID of node to query.
%% @returns {ok, Edges} if node exists or {error, not_found} if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec edges_to(graph(), atom()) -> {ok, graph_edges()} | {error, not_found}.
edges_to(_Graph = #mt_dgraphs{edges = Edges, nodes = Nodes}, NodeId) ->
    case maps:is_key(NodeId, Nodes) of
        true ->
            {ok,
             [format_edge(Start, End, Opts)
              || {Start, V} <- maps:to_list(Edges), {End, Opts} <- maps:to_list(V), End == NodeId]};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Checks if an edge exists.
%% @end
%% @param Graph Graph to query.
%% @param Start Starting node ID.
%% @param End Ending node ID.
%% @returns A boolean indicating if the edge exists.
%% @end
%%
%% ----------------------------------
-spec edge_exists(graph(), atom(), atom()) -> boolean().
edge_exists(_Graph = #mt_dgraphs{edges = Edges}, Start, End) ->
    maps:is_key(End, maps:get(Start, Edges, #{})).

%% ----------------------------------
%%
%% @doc Checks if a node exists.
%% @end
%% @param Graph Graph to query.
%% @param NodeId ID of node to query.
%% @returns A boolean indicating if the edge exists.
%% @end
%%
%% ----------------------------------
-spec node_exists(graph(), atom()) -> boolean().
node_exists(_Graph = #mt_dgraphs{nodes = Nodes}, NodeId) ->
    maps:is_key(NodeId, Nodes).

%% ----------------------------------
%%
%% @doc Checks if a path exists between two nodes.
%% @end
%% @param Graph Graph to query.
%% @param Start ID of starting node.
%% @param End ID of ending node.
%% @returns A boolean indicating if there is a path from start to end.
%% @end
%%
%% ----------------------------------
-spec path_exists(graph(), atom(), atom()) -> boolean().
path_exists(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}, From, To) ->
    case {maps:is_key(From, Nodes), maps:is_key(To, Nodes)} of
        {true, true} ->
            Visited = ordsets:new(),
            Queue = [From],
            path_exists_impl(To, Edges, Queue, Visited);
        _ ->
            false
    end.

%% ----------------------------------
%%
%% @doc Retrieves the lowest weighted path between to nodes. Weights =&lt; 0 are treated as if they're of weight ?MIN.
%% @end
%% @param Graph Graph to query.
%% @param Start ID of starting node.
%% @param End ID of ending node.
%% @returns {ok, Path} if a path is found, {error, none} if there is no path, or {error, not_found} if the start or end is invalid.
%% @end
%%
%% ----------------------------------
-spec path_to(graph(), atom(), atom()) -> {ok, [atom()]} | {error, not_found | none}.
path_to(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}, From, To) ->
    case {maps:is_key(From, Nodes), maps:is_key(To, Nodes)} of
        {true, true} ->
            Visited = #{From => {start, 0}},
            Queue = [{From, 0}],
            path_to_impl(To, Edges, Queue, Visited);
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Retrieves a list of all non-looping paths between two nodes.
%% @end
%% @param Graph Graph to query.
%% @param Start ID of starting node.
%% @param End ID of ending node.
%% @returns {ok, [Path]|[]} if a path is found, or {error, not_found} if the start or end is invalid.
%% @end
%%
%% ----------------------------------
-spec paths(graph(), atom(), atom()) -> {ok, [[atom()]] | []} | {error, not_found | none}.
paths(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}, From, To) ->
    case {maps:is_key(From, Nodes), maps:is_key(To, Nodes)} of
        {true, true} ->
            Stack = [From],
            Visited = ordsets:new(),
            {ok,
             lists:map(fun({P}) -> P end, lists:flatten(paths_impl(To, Edges, Stack, Visited)))};
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Retrieves the path with the fewest connections between to nodes.
%% @end
%% @param Graph Graph to query.
%% @param Start ID of starting node.
%% @param End ID of ending node.
%% @returns {ok, Path} if a path is found, {error, none} if there is no path, or {error, not_found} if the start or end is invalid.
%% @end
%%
%% ----------------------------------
-spec bfs_path_to(graph(), atom(), atom()) -> {ok, [atom()]} | {error, not_found | none}.
bfs_path_to(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}, From, To) ->
    case {maps:is_key(From, Nodes), maps:is_key(To, Nodes)} of
        {true, true} ->
            Visited = #{From => start},
            Queue = queue:from_list([From]),
            bfs_path_to_impl(To, Edges, Queue, Visited);
        _ ->
            {error, not_found}
    end.

%% ----------------------------------
%%
%% @doc Deletes an edge in a graph. If the edge is not present then it will return the graph.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Starting node ID.
%% @param End Ending node ID.
%% @returns The final graph.
%% @end
%%
%% ----------------------------------
-spec delete_edge(graph(), atom(), atom()) -> {boolean(), graph()}.
delete_edge(Graph =
                #mt_dgraphs{edges = Edges,
                            nodes = Nodes,
                            type = directed},
            Start,
            End) ->
    case {maps:is_key(Start, Nodes),
          maps:is_key(End, Nodes),
          maps:is_key(Start, Edges),
          maps:is_key(End, maps:get(Start, Edges, #{}))}
    of
        {true, true, true, true} ->
            StartEdges = maps:get(Start, Edges),
            WithoutEnd = maps:remove(End, StartEdges),
            NewMapSize = maps:size(WithoutEnd),
            NewEdges =
                if NewMapSize =:= 0 ->
                       maps:remove(Start, Edges);
                   true ->
                       maps:put(Start, WithoutEnd, Edges)
                end,
            {true, Graph#mt_dgraphs{edges = NewEdges}};
        _ ->
            {false, Graph}
    end;
delete_edge(Graph = #mt_dgraphs{type = undirected}, Start, End) ->
    {_, GD} = delete_edge(Graph#mt_dgraphs{type = directed}, Start, End),
    {Res, GD2} = delete_edge(GD, End, Start),
    {Res, GD2#mt_dgraphs{type = undirected}}.

%% ----------------------------------
%%
%% @doc Deletes multiple edges from a graph edges that don't exist are ignored.
%% @end
%% @param Graph Graph to operate on.
%% @param Edges List of edges to remove.
%% @returns The final graph.
%% @end
%%
%% ----------------------------------
-spec delete_edges(graph(), [{atom(), atom()}]) -> {integer(), graph()}.
delete_edges(Graph = #mt_dgraphs{}, Edges) ->
    lists:foldl(fun(Edge, {Count, G}) ->
                   case delete_edge(G, Edge) of
                       {true, NG} -> {Count + 1, NG};
                       _ -> {Count, G}
                   end
                end,
                {0, Graph},
                Edges).

delete_edge(Graph, {Start, End}) ->
    delete_edge(Graph, Start, End).

%% ----------------------------------
%%
%% @doc Deletes a node from the graph. Does nothing if the node doesn't exist. Deletes all edges tied to the node.
%%
%% Returns whether a node was deleted or not.
%% @end
%% @param Graph Graph to operate on.
%% @param NodeId ID of node to delete.
%% @returns The final graph.
%% @end
%%
%% ----------------------------------
-spec delete_node(graph(), atom()) -> {boolean(), graph()}.
delete_node(Graph = #mt_dgraphs{nodes = Nodes}, NodeId) ->
    Node = NodeId,
    case maps:is_key(Node, Nodes) of
        false ->
            {false, Graph};
        _ ->
            {ok, To} = edges_to(Graph, Node),
            ToSimple = [{F, T} || #mt_dgraphs_edge{v1 = F, v2 = T} <- To],
            {_, G2} = delete_edges(Graph, ToSimple),
            {ok, From} = edges_from(G2, Node),
            FromSimple = [{F, T} || #mt_dgraphs_edge{v1 = F, v2 = T} <- From],
            {_, G3} = delete_edges(Graph, FromSimple),
            NewNodes = maps:remove(Node, Nodes),
            {true, G3#mt_dgraphs{nodes = NewNodes}}
    end.

%% ----------------------------------
%%
%% @doc Deletes multiple nodes from the graph.
%% Returns how many were deleted.
%% @end
%% @param Graph The graph to operate on.
%% @param Nodes The list of nodes to delete.
%% @returns The deletion count and the final graph.
%% @end
%%
%% ----------------------------------
-spec delete_nodes(graph(), [atom()]) -> {integer(), graph()}.
delete_nodes(Graph = #mt_dgraphs{}, Nodes) ->
    lists:foldl(fun(Node, {Count, G}) ->
                   case delete_node(G, Node) of
                       {true, NG} -> {Count + 1, NG};
                       _ -> {Count, G}
                   end
                end,
                {0, Graph},
                Nodes).

%% ----------------------------------
%%
%% @doc Gets the number of edges coming into a node.
%% @end
%% @param Graph The graph to query.
%% @param NodeId The ID of the node coming in.
%% @returns {ok, EdgeCount} if node exists, {error, not_found} if it doesn't.
%% @end
%%
%% ----------------------------------
-spec in_degree(graph(), atom()) -> {ok, integer()} | {error, not_found}.
in_degree(Graph = #mt_dgraphs{}, NodeId) ->
    case node_exists(Graph, NodeId) of
        false ->
            {error, not_found};
        true ->
            {ok, Edges} = edges_to(Graph, NodeId),
            {ok, length(Edges)}
    end.

%% ----------------------------------
%%
%% @doc Gets the number of edges leaving a node.
%% @end
%% @param Graph The graph to query.
%% @param NodeId The ID of the node coming in.
%% @returns {ok, EdgeCount} if node exists, {error, not_found} if it doesn't.
%% @end
%%
%% ----------------------------------
-spec out_degree(graph(), atom()) -> {ok, integer()} | {error, not_found}.
out_degree(Graph = #mt_dgraphs{}, NodeId) ->
    case node_exists(Graph, NodeId) of
        false ->
            {error, not_found};
        true ->
            {ok, Edges} = edges_from(Graph, NodeId),
            {ok, length(Edges)}
    end.

%% ----------------------------------
%%
%% @doc Returns a map describing the fields in a graph.
%% @end
%% @param Graph Graph to query.
%% @returns A map.
%% @end
%%
%% ----------------------------------
-spec info(graph()) -> map().
info(_Graph =
         #mt_dgraphs{type = Type,
                     edges = Edges,
                     nodes = Nodes}) ->
    #{type => Type,
      edges => format_edges(Edges),
      nodes => format_nodes(Nodes)}.

%% ----------------------------------
%%
%% @doc Checks whether a graph is acyclic.
%% @end
%% @param Graph Graph to check.
%% @returns true if acyclic, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_acyclic(graph()) -> boolean().
is_acyclic(_Graph = #mt_dgraphs{type = undirected, edges = Edges}) ->
    maps:size(Edges) =:= 0;
is_acyclic(Graph = #mt_dgraphs{edges = Edges}) ->
    case roots(Graph) of
        [] ->
            maps:size(Edges) =:= 0;
        Roots ->
            case is_acyclic_impl(Graph, Roots, ordsets:new()) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end
    end.

%% ----------------------------------
%%
%% @doc Gets a list of node ids for nodes that are roots (in degree of 0).
%% @end
%% @param Graph Graph to check.
%% @returns List of roots.
%% @end
%%
%% ----------------------------------
-spec roots(graph()) -> [atom()].
roots(_Graph = #mt_dgraphs{nodes = Nodes, edges = Edges}) ->
    NonRoot = ordsets:from_list([E || L <- maps:values(Edges), E <- maps:keys(L)]),
    lists:filter(fun(N) -> not ordsets:is_element(N, NonRoot) end, maps:keys(Nodes)).

%% ----------------------------------
%%
%% @doc Checks whether a graph is empty.
%% @end
%% @param Graph Graph to check.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_empty(graph()) -> boolean().
is_empty(_Graph = #mt_dgraphs{nodes = Nodes}) ->
    maps:size(Nodes) =:= 0.

%% ----------------------------------
%%
%% @doc Checks whether a graph is cyclic.
%% @end
%% @param Graph Graph to check.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_cyclic(graph()) -> boolean().
is_cyclic(Graph = #mt_dgraphs{}) ->
    not is_acyclic(Graph).

%% ----------------------------------
%%
%% @doc Checks whether a graph is a directed acyclic graph.
%% @end
%% @param Graph Graph to check.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_dag(graph()) -> boolean().
is_dag(_Graph = #mt_dgraphs{type = undirected}) ->
    false;
is_dag(Graph = #mt_dgraphs{}) ->
    is_acyclic(Graph).

%% ----------------------------------
%%
%% @doc Checks whether a graph is a tree.
%% @end
%% @param Graph Graph to check.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_tree(graph()) -> boolean().
is_tree(_Graph = #mt_dgraphs{edges = Edges, nodes = Nodes})
    when map_size(Edges) =:= 0, map_size(Nodes) =< 1 ->
    true;
is_tree(#mt_dgraphs{type = undirected}) ->
    false;
is_tree(Graph = #mt_dgraphs{nodes = Nodes}) ->
    case {length(roots(Graph)), is_acyclic(Graph)} of
        {1, true} ->
            lists:all(fun(R) ->
                         {ok, Degree} = in_degree(Graph, R),
                         Degree =< 1
                      end,
                      maps:keys(Nodes));
        _ ->
            false
    end.

%% ----------------------------------
%%
%% @doc Checks whether a graph is a subgraph of another graph. Strict checks means all attributes of nodes must match. Non-strict checks mean only the ids must match.
%% @end
%% @param RootGraph Main graph for check.
%% @param SubGraph Subgraph to check if part of root graph.
%% @param Opts A list of check options. Valid options are: {strict, boolean()}.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_subgraph(graph(), graph(), subgraph_opts()) -> boolean().
is_subgraph(RootGraph = #mt_dgraphs{}, SubGraph = #mt_dgraphs{}, _)
    when RootGraph =:= SubGraph ->
    true;
is_subgraph(Graph1 = #mt_dgraphs{}, Graph2 = #mt_dgraphs{}, Opts) when is_list(Opts) ->
    Strict =
        case lists:keyfind(strict, 1, Opts) of
            false ->
                true;
            {_, S} ->
                S
        end,
    case {is_subnodes(Graph1, Graph2, Strict), is_subedges(Graph1, Graph2, Strict)} of
        {true, true} ->
            true;
        _ ->
            false
    end.

%% ----------------------------------
%%
%% @doc Checks whether a graph is a subgraph of another graph.
%% @end
%% @param RootGraph Main graph for check.
%% @param SubGraph Subgraph to check if part of root graph.
%% @returns true if so, false otherwise.
%% @end
%%
%% ----------------------------------
-spec is_subgraph(graph(), graph()) -> boolean().
is_subgraph(RootGraph = #mt_dgraphs{}, SubGraph = #mt_dgraphs{})
    when RootGraph =:= SubGraph ->
    true;
is_subgraph(Graph1 = #mt_dgraphs{}, Graph2 = #mt_dgraphs{}) ->
    is_subgraph(Graph1, Graph2, []).

%% ----------------------------------
%%
%% @doc Performs a topological sort on a graph.
%% @end
%% @param Graph Graph to query.
%% @returns A list of node ids in topological order.
%% @end
%%
%% ----------------------------------
-spec topsort(graph) -> {ok, [atom()]} | {error, cycle_detected}.
topsort(Graph = #mt_dgraphs{}) ->
    case {is_empty(Graph), is_acyclic(Graph)} of
        {true, _} ->
            {ok, []};
        {_, false} ->
            {error, cycle_detected};
        _ ->
            Roots = roots(Graph),
            {_, Res} = topsort(Graph, Roots, 0, #{}),
            Sorted = lists:sort(fun({_, W1}, {_, W2}) -> W1 =< W2 end, maps:to_list(Res)),
            {ok, [K || {K, _} <- Sorted]}
    end.

%% ----------------------------------
%%
%% @doc Splits an edge with either an existing or new node.
%% @end
%% @param Graph Graph to operate on.
%% @param Start Node ID at start of edge.
%% @param End Node ID at end of edge.
%% @param Node Node to use to split edges..
%% @returns Return.
%% @end
%%
%% ----------------------------------
-spec split_edge(graph(), atom(), atom(), input_node()) ->
                    {ok, graph()} | {error, not_found}.
split_edge(Graph, Start, End, Node) ->
    case edge_exists(Graph, Start, End) of
        false ->
            {error, not_found};
        true ->
            WithNode =
                case Node of
                    {ID, Opts} ->
                        mt_dgraphs:set_node(Graph, ID, Opts);
                    ID ->
                        {_, New} = mt_dgraphs:add_node(Graph, ID),
                        New
                end,
            NodeId = id_from_input_node(Node),
            {_, WithoutEdge} = mt_dgraphs:delete_edge(WithNode, Start, End),
            {_, NewGraph} = mt_dgraphs:add_edges(WithoutEdge, [{Start, NodeId}, {NodeId, End}]),
            {ok, NewGraph}
    end.

%% ----------------------------------
%%
%% @doc Returns the number of edges in a graph.
%% @end
%% @param Graph Graph to query.
%% @returns Number of edges.
%% @end
%%
%% ----------------------------------
-spec num_edges(graph()) -> integer().
num_edges(Graph = #mt_dgraphs{type = directed}) ->
    raw_num_edges(Graph);
num_edges(Graph = #mt_dgraphs{type = undirected}) ->
    raw_num_edges(Graph) div 2.

%% ----------------------------------
%%
%% @doc Retuns the number of nodes in a graph.
%% @end
%% @param Graph Graph to query.
%% @returns Number of nodes.
%% @end
%%
%% ----------------------------------
-spec num_nodes(graph()) -> integer().
num_nodes(#mt_dgraphs{nodes = Nodes}) ->
    maps:size(Nodes).

%% ----------------------------------
%%
%% @doc Reverses the direction of all edges in a graph.
%% @end
%% @param Graph Graph to operate on.
%% @returns The final graph.
%% @end
%%
%% ----------------------------------
-spec transpose(graph()) -> graph().
transpose(Graph = #mt_dgraphs{type = undirected}) ->
    Graph;
transpose(G = #mt_dgraphs{edges = E}) ->
    Edges =
        [{V2, V1, [{label, L}, {weight, W}]}
         || #mt_dgraphs_edge{v1 = V1,
                             v2 = V2,
                             label = L,
                             weight = W}
                <- format_edges(E)],
    #mt_dgraphs{edges = NewEdges} = form_graph(directed, [], Edges),
    G#mt_dgraphs{edges = NewEdges}.

%% Implementation

topsort(Graph = #mt_dgraphs{}, [Start | Roots], Weight, Visited) ->
    CurWeight = maps:get(Start, Visited, 0),
    NewVisited =
        if CurWeight =< Weight ->
               maps:put(Start, Weight, Visited);
           true ->
               Visited
        end,
    NewWeight = Weight + 1,
    {EndWeight, EndVisited} = topsort(Graph, Start, NewWeight, NewVisited),
    topsort(Graph, Roots, EndWeight, EndVisited);
topsort(#mt_dgraphs{}, [], Weight, Visited) ->
    {Weight + 1, Visited};
topsort(Graph = #mt_dgraphs{}, Node, Weight, Visited) ->
    {ok, OutEdges} = edges_from(Graph, Node),
    Children = [E || #mt_dgraphs_edge{v2 = E} <- OutEdges],
    topsort(Graph, Children, Weight, Visited).

id_from_input_node({Id, _}) ->
    Id;
id_from_input_node(Id) ->
    Id.

filter_default_node_opts(Opts) ->
    lists:filter(fun(Opt) -> not is_default_node_opt(Opt) end, Opts).

is_default_node_opt({label, nil}) ->
    true;
is_default_node_opt(_) ->
    false.

filter_default_edge_opts(Opts) ->
    lists:filter(fun(Opt) -> not is_default_edge_opt(Opt) end, Opts).

is_default_edge_opt({label, nil}) ->
    true;
is_default_edge_opt({weight, 1}) ->
    true;
is_default_edge_opt(_) ->
    false.

is_acyclic_impl(Graph = #mt_dgraphs{}, [Root | Roots], Reachable) ->
    case is_acyclic_impl(Graph, Root, Reachable, ordsets:new()) of
        {error, Err} ->
            {error, Err};
        {ok, NewReachable} ->
            is_acyclic_impl(Graph, Roots, NewReachable)
    end;
is_acyclic_impl(#mt_dgraphs{nodes = Nodes}, [], Reachable) ->
    NodeSet =
        ordsets:from_list(
            maps:keys(Nodes)),
    case ordsets:subtract(NodeSet, Reachable) of
        [] ->
            {ok, Reachable};
        _ ->
            {error, nodes_unreachable}
    end.

is_acyclic_impl(Graph = #mt_dgraphs{}, Node, Reachable, Visited) ->
    NewReachable = ordsets:add_element(Node, Reachable),
    NewVisited = ordsets:add_element(Node, Visited),
    {ok, Outgoing} = edges_from(Graph, Node),
    is_acyclic_impl_loop(Graph, Outgoing, NewReachable, NewVisited).

is_acyclic_impl_loop(Graph = #mt_dgraphs{},
                     [#mt_dgraphs_edge{v2 = E} | Rest],
                     Reachable,
                     Visited) ->
    case is_acyclic_valid(Graph, E, Reachable, Visited) of
        E = {error, _} ->
            E;
        {ok, NewReachable} ->
            is_acyclic_impl_loop(Graph, Rest, NewReachable, Visited)
    end;
is_acyclic_impl_loop(#mt_dgraphs{}, [], Reachable, _) ->
    {ok, Reachable}.

is_acyclic_valid(Graph, E, Reachable, Visited) ->
    case {ordsets:is_element(E, Visited), ordsets:is_element(E, Reachable)} of
        {true, _} ->
            {error, cycle_detected};
        % memorization optimization
        {_, true} ->
            {ok, Reachable};
        _ ->
            is_acyclic_impl(Graph, E, Reachable, Visited)
    end.

bfs_path_to_impl(To, Edges, OldQueue, Visited) ->
    case queue:peek(OldQueue) of
        {value, To} ->
            {ok, retrieve_path_bfs(Visited, To, [])};
        {value, Cur} ->
            Queue = queue:drop(OldQueue),
            case maps:is_key(Cur, Edges) of
                false ->
                    bfs_path_to_impl(To, Edges, Queue, Visited);
                true ->
                    Outgoing = maps:get(Cur, Edges),
                    NewVisited = [End || End <- maps:keys(Outgoing), not maps:is_key(End, Visited)],
                    NewVisitedMap = maps:from_list([{End, {node, Cur}} || End <- NewVisited]),
                    NewQueue = queue:join(Queue, queue:from_list(NewVisited)),
                    NewMap = maps:merge(Visited, NewVisitedMap),
                    bfs_path_to_impl(To, Edges, NewQueue, NewMap)
            end;
        _ ->
            {error, none}
    end.

paths_impl(To, _Edges, CurPath = [To | _], _) ->
    [{lists:reverse(CurPath)}];
paths_impl(To, Edges, CurPath = [Cur | _], Visited) when length(CurPath) < ?MAX_DEPTH ->
    case maps:is_key(Cur, Edges) of
        false ->
            [];
        true ->
            NewV = ordsets:add_element(Cur, Visited),
            Outgoing = maps:get(Cur, Edges),
            lists:filter(fun(V) ->
                            case V of
                                [] -> false;
                                _ -> true
                            end
                         end,
                         [paths_impl(To, Edges, [E | CurPath], NewV)
                          || E <- maps:keys(Outgoing), not ordsets:is_element(E, NewV)])
    end;
paths_impl(_, _, _, _) ->
    [].

path_to_impl(To, _Edges, _Queue = [{To, _} | _], Visited) ->
    {ok, retrieve_path(Visited, To, [])};
path_to_impl(To, Edges, [{Cur, Weight} | Queue], Visited) ->
    case maps:is_key(Cur, Edges) of
        false ->
            path_to_impl(To, Edges, Queue, Visited);
        true ->
            Outgoing = maps:get(Cur, Edges),
            NewVisited =
                [{End, new_weight(opt_weight(Opts), Weight)}
                 || {End, Opts} <- maps:to_list(Outgoing),
                    shorter_path(End, Opts, Weight, Visited)],
            NewVisitedMap = maps:from_list([{V, {{node, Cur}, W}} || {V, W} <- NewVisited]),
            NewQueue = mt_dgraphs_pqueue:in_all(Queue, NewVisited),
            NewMap = maps:merge(Visited, NewVisitedMap),
            path_to_impl(To, Edges, NewQueue, NewMap)
    end;
path_to_impl(_To, _Edges, [], _Visited) ->
    {error, none}.

v_weight({_, W}) ->
    W.

new_weight(EdgeWeight, Weight) when EdgeWeight + Weight > Weight ->
    EdgeWeight + Weight;
new_weight(_, Weight) ->
    Weight + ?MIN.

shorter_path(Node, Opts, CurWeight, Visited) ->
    case maps:is_key(Node, Visited) of
        false ->
            true;
        _ ->
            case {v_weight(maps:get(Node, Visited)), new_weight(opt_weight(Opts), CurWeight)} of
                {C, N} when C > N ->
                    true;
                _ ->
                    false
            end
    end.

retrieve_path(V, T, P) ->
    case maps:get(T, V) of
        {start, _} ->
            [T | P];
        {{node, N}, _} ->
            retrieve_path(V, N, [T | P])
    end.

retrieve_path_bfs(V, T, P) ->
    case maps:get(T, V) of
        start ->
            [T | P];
        {node, N} ->
            retrieve_path_bfs(V, N, [T | P])
    end.

path_exists_impl(To, _Edges, [To | _], _Visited) ->
    true;
path_exists_impl(To, Edges, [Cur | Queue], Visited) ->
    case maps:is_key(Cur, Edges) of
        false ->
            path_exists_impl(To, Edges, Queue, Visited);
        true ->
            Outgoing = maps:get(Cur, Edges),
            NewVisited = [End || End <- maps:keys(Outgoing), not ordsets:is_element(End, Visited)],
            path_exists_impl(To,
                             Edges,
                             Queue ++ NewVisited,
                             ordsets:union(Visited, ordsets:from_list(NewVisited)))
    end;
path_exists_impl(_To, _Edges, [], _Visited) ->
    false.

form_graph(Type, Nodes, Edges) when is_list(Nodes), is_list(Edges) ->
    GraphNodes = maps:merge(edge_nodes(Edges), nodes_from_list(Nodes)),
    GraphEdges = edge_map(Type, Edges),
    #mt_dgraphs{type = Type,
                nodes = GraphNodes,
                edges = GraphEdges}.

edge_map(directed, Edges) ->
    edge_map_impl(Edges, #{});
edge_map(undirected, Edges) ->
    maps:merge_with(fun(_, V1, V2) -> maps:merge(V1, V2) end,
                    edge_map_impl(Edges, #{}),
                    edge_map_impl(edges_reverse(Edges), #{})).

edge_map_impl([{Start, End} | Rest], State) ->
    edge_map_impl([{Start, End, []} | Rest], State);
edge_map_impl([{Start, End, Opts} | Rest], State) ->
    NewState =
        maps:put(Start,
                 maps:put(End, filter_default_edge_opts(Opts), maps:get(Start, State, #{})),
                 State),
    edge_map_impl(Rest, NewState);
edge_map_impl([], State) ->
    State.

edges_reverse(Edges) ->
    lists:map(fun(Edge) -> reverse_edge(Edge) end, Edges).

reverse_edge({Start, End}) ->
    {End, Start, []};
reverse_edge({Start, End, Opts}) ->
    {End, Start, Opts}.

edge_nodes(Edges) ->
    maps:from_list([{E, []}
                    || E
                           <- ordsets:to_list(
                                  ordsets:from_list(
                                      lists:flatten([[p_edge_start(E), p_edge_end(E)]
                                                     || E <- Edges])))]).

p_edge_start({S, _}) ->
    S;
p_edge_start({S, _, _}) ->
    S.

p_edge_end({_, E}) ->
    E;
p_edge_end({_, E, _}) ->
    E.

nodes_from_list(Nodes) ->
    nodes_from_list(Nodes, #{}).

nodes_from_list([{Id, Opts} | Rest], Nodes = #{}) ->
    nodes_from_list(Rest, maps:put(Id, filter_default_node_opts(Opts), Nodes));
nodes_from_list([Id | Rest], Nodes = #{}) ->
    nodes_from_list(Rest, maps:put(Id, [], Nodes));
nodes_from_list([], Nodes = #{}) ->
    Nodes.

raw_num_edges(#mt_dgraphs{edges = Edges}) ->
    length([{A, B} || {A, O} <- maps:to_list(Edges), B <- maps:keys(O)]).

format_edges_from(Start, Edges = #{}) ->
    [#mt_dgraphs_edge{v1 = Start,
                      v2 = End,
                      weight = opt_weight(O),
                      label = opt_label(O)}
     || {End, O} <- maps:to_list(Edges)].

format_edges(Edges = #{}) ->
    [#mt_dgraphs_edge{v1 = ID,
                      v2 = E,
                      weight = opt_weight(O),
                      label = opt_label(O)}
     || {ID, F} <- maps:to_list(Edges), {E, O} <- maps:to_list(F)].

format_node(NodeId, NodeData) ->
    #mt_dgraphs_node{id = NodeId, label = opt_label(NodeData)}.

format_nodes(Nodes = #{}) ->
    [format_node(ID, O) || {ID, O} <- maps:to_list(Nodes)].

opt_weight(Opts) ->
    case lists:keyfind(weight, 1, Opts) of
        false ->
            1;
        {_, W} ->
            W
    end.

opt_label(Opts) ->
    case lists:keyfind(label, 1, Opts) of
        false ->
            nil;
        {_, L} ->
            L
    end.

format_edge(Start, End, Opts) ->
    #mt_dgraphs_edge{v1 = Start,
                     v2 = End,
                     weight = opt_weight(Opts),
                     label = opt_label(Opts)}.

is_subnodes(#mt_dgraphs{nodes = N1}, #mt_dgraphs{nodes = N2}, false)
    when map_size(N1) >= map_size(N2) ->
    Nodes1 =
        ordsets:from_list(
            maps:keys(N1)),
    Nodes2 =
        ordsets:from_list(
            maps:keys(N2)),
    case ordsets:subtract(Nodes2, Nodes1) of
        [] ->
            true;
        _ ->
            false
    end;
is_subnodes(#mt_dgraphs{nodes = N1}, #mt_dgraphs{nodes = N2}, true)
    when map_size(N1) >= map_size(N2) ->
    Nodes1 = ordsets:from_list(format_nodes(N1)),
    Nodes2 = ordsets:from_list(format_nodes(N2)),
    case ordsets:subtract(Nodes2, Nodes1) of
        [] ->
            true;
        _ ->
            false
    end;
is_subnodes(#mt_dgraphs{}, #mt_dgraphs{}, _) ->
    false.

is_subedges(#mt_dgraphs{edges = E1}, #mt_dgraphs{edges = E2}, false)
    when map_size(E1) >= map_size(E2) ->
    Edges1 = ordsets:from_list([{S, E} || {S, M} <- maps:to_list(E1), E <- maps:keys(M)]),
    Edges2 = ordsets:from_list([{S, E} || {S, M} <- maps:to_list(E2), E <- maps:keys(M)]),
    case ordsets:subtract(Edges2, Edges1) of
        [] ->
            true;
        _ ->
            false
    end;
is_subedges(#mt_dgraphs{edges = E1}, #mt_dgraphs{edges = E2}, true)
    when map_size(E1) >= map_size(E2) ->
    Edges1 = ordsets:from_list(format_edges(E1)),
    Edges2 = ordsets:from_list(format_edges(E2)),
    case ordsets:subtract(Edges2, Edges1) of
        [] ->
            true;
        _ ->
            false
    end;
is_subedges(#mt_dgraphs{}, #mt_dgraphs{}, _) ->
    false.
