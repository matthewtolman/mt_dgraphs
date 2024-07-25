-module(mt_dgraphs_server).

-behaviour(gen_server).

-export([new/0, new/1, new/2, new/3, nodes/1, node_ids/1, node_by_id/2, add_node/2,
         add_node/3, set_node/2, delete_node/2, delete_nodes/2, delete_edges/2, get_node_label/2,
         set_node_label/3, node_exists/2, in_degree/2, out_degree/2, edges/1, add_edge/3,
         add_edge/4, set_edge/3, set_edge/4, delete_edge/3, edge_by/3, edge_exists/3,
         get_edge_label/3, get_edge_weight/3, edges_to/2, edges_from/2, set_edge_weight/3,
         set_edge_label/3, split_edge/4, path_exists/3, path_to/3, paths/3, bfs_path_to/3, roots/1,
         info/1, is_empty/1, is_cyclic/1, is_acyclic/1, is_dag/1, is_tree/1, num_edges/1,
         num_nodes/1, topsort/1, transpose/1, type/1, is_subgraph/3, is_subgraph/2, raw_graph/1,
         destroy/1, add_edges/2, add_nodes/2, set_edges/2, set_nodes/2, load_graph/2, version/1,
         update_graph/3, update_graph/2, start_link/0, start_link/1, start_link/2, start_link/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-include("mt_dgraphs.hrl").

-type graph_server() :: pid().

-export_type([graph_server/0]).

%% Client

%% ----------------------------------
%%
%% @doc Creates and links to a new graph server.
%% @end
%%
%% ----------------------------------
-spec start_link() -> {ok, graph_server()} | ignore | {error, term()}.
start_link() ->
    start_link(directed, [], []).

%% ----------------------------------
%%
%% @doc Creates and links to a new graph server.
%% @end
%%
%% ----------------------------------
-spec start_link(mt_dgraphs:graph_type() | mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
start_link(Type) when is_atom(Type) ->
    start_link(Type, [], []);
start_link(Edges) when is_list(Edges) ->
    start_link(directed, [], Edges).

%% ----------------------------------
%%
%% @doc Creates and links to a new graph server.
%% @end
%%
%% ----------------------------------
-spec start_link(mt_dgraphs:graph_type() | mt_dgraphs:graph_nodes(), mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
start_link(Type, Edges) when is_atom(Type) ->
    start_link(Type, [], Edges);
start_link(Nodes, Edges) ->
    start_link(directed, Nodes, Edges).

%% @equiv mt_dgraphs:start_link()
-spec new() -> {ok, graph_server()} | ignore | {error, term()}.
new() ->
    new(directed, [], []).

%% @equiv mt_dgraphs:start_link(Type)
-spec new(mt_dgraphs:graph_type() | mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
new(Type) when is_atom(Type) ->
    new(Type, [], []);
new(Edges) when is_list(Edges) ->
    new(directed, [], Edges).

%% @equiv mt_dgraphs:start_link(Type, Edges)
-spec new(mt_dgraphs:graph_type() | mt_dgraphs:graph_nodes(), mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
new(Type, Edges) when is_atom(Type) ->
    new(Type, [], Edges);
new(Nodes, Edges) ->
    new(directed, Nodes, Edges).

%% @equiv mt_dgraphs:start_link(Type, Nodes, Edges)
-spec new(mt_dgraphs:graph_type(), mt_dgraphs:graph_nodes(), mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
new(Type, Nodes, Edges) ->
    start_link(Type, Nodes, Edges).

%% ----------------------------------
%%
%% @doc Creates and links to a new graph server.
%% @end
%%
%% ----------------------------------
-spec start_link(mt_dgraphs:graph_type(), mt_dgraphs:graph_nodes(), mt_dgraphs:graph_edges()) ->
             {ok, graph_server()} | ignore | {error, term()}.
start_link(Type, Nodes, Edges) ->
    gen_server:start_link(?MODULE, [Type, Nodes, Edges], []).

%% ----------------------------------
%%
%% @doc Gets all nodes on a graph server.
%% @end
%%
%% ----------------------------------
-spec nodes(graph_server()) -> mt_dgraphs:graph_nodes().
nodes(Graph) ->
    gen_server:call(Graph, nodes).

%% ----------------------------------
%%
%% @doc Gets all node ids on a graph server.
%% @end
%%
%% ----------------------------------
-spec node_ids(graph_server()) -> [atom()] | [].
node_ids(Graph) ->
    gen_server:call(Graph, node_ids).

%% ----------------------------------
%%
%% @doc Gets a node by id or returns an error if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec node_by_id(graph_server(), atom()) ->
                    {ok, mt_dgraphs:graph_node()} | {error, not_found}.
node_by_id(Graph, NodeId) ->
    gen_server:call(Graph, {node, NodeId}).

%% ----------------------------------
%%
%% @doc Adds a new node if it doesn't already exist.
%% @end
%%
%% ----------------------------------
-spec add_node(graph_server(), atom()) -> ok | already_exists.
add_node(Graph, NodeId) ->
    add_node(Graph, NodeId, []).

%% ----------------------------------
%%
%% @doc Adds a new node if it doesn't already exist.
%% @end
%%
%% ----------------------------------
-spec add_node(graph_server(), atom(), mt_dgraphs:node_options()) -> ok | already_exists.
add_node(Graph, NodeId, Opts) ->
    gen_server:call(Graph, {add_node, NodeId, Opts}).

%% ----------------------------------
%%
%% @doc Adds new nodes. Ignores any that already exist.
%% @end
%%
%% ----------------------------------
-spec add_nodes(graph_server(), mt_dgraphs:input_nodes()) -> ok.
add_nodes(Graph, Nodes) ->
    gen_server:call(Graph, {add_nodes, Nodes}).

%% ----------------------------------
%%
%% @doc Sets a node to default data. Adds it if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec set_node(graph_server(), atom()) -> ok.
set_node(Graph, NodeId) ->
    set_node(Graph, NodeId, []).

%% ----------------------------------
%%
%% @doc Sets a node's data. Adds it if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec set_node(graph_server(), atom(), mt_dgraphs:node_options()) -> ok.
set_node(Graph, NodeId, Opts) ->
    gen_server:call(Graph, {set_node, NodeId, Opts}).

%% ----------------------------------
%%
%% @doc Sets data for serveral nodes. Adds any that don't exist.
%% @end
%%
%% ----------------------------------
-spec set_nodes(graph_server(), mt_dgraphs:input_nodes()) -> ok.
set_nodes(Graph, Nodes) ->
    gen_server:call(Graph, {set_nodes, Nodes}).

%% ----------------------------------
%%
%% @doc Deletes a node (if it exists).
%% @end
%%
%% ----------------------------------
-spec delete_node(graph_server(), atom()) -> ok.
delete_node(Graph, NodeId) ->
    gen_server:call(Graph, {delete_node, NodeId}).

%% ----------------------------------
%%
%% @doc Deletes nodes.
%% @end
%%
%% ----------------------------------
-spec delete_nodes(graph_server(), [atom()]) -> ok.
delete_nodes(Graph, NodeIds) ->
    gen_server:call(Graph, {delete_nodes, NodeIds}).

%% ----------------------------------
%%
%% @doc Gets the label for a node. Returns an error if the node doesn't exist.
%% @end
%%
%% ----------------------------------
-spec get_node_label(graph_server(), atom()) -> {ok, any()} | {error, not_found}.
get_node_label(Graph, NodeId) ->
    gen_server:call(Graph, {node, label, NodeId}).

%% ----------------------------------
%%
%% @doc Sets a node's label. Adds a node if it doesn't exist.
%% @end
%%
%% ----------------------------------
-spec set_node_label(graph_server(), atom(), any()) -> ok.
set_node_label(Graph, NodeId, Label) ->
    gen_server:call(Graph, {set_node_label, NodeId, Label}).

%% ----------------------------------
%%
%% @doc Returns whether a node exists.
%% @end
%%
%% ----------------------------------
-spec node_exists(graph_server(), atom()) -> boolean().
node_exists(Graph, NodeId) ->
    gen_server:call(Graph, {node, exists, NodeId}).

%% ----------------------------------
%%
%% @doc Returns number of edges moving into a node.
%% @end
%%
%% ----------------------------------
-spec in_degree(graph_server(), atom()) -> integer().
in_degree(Graph, NodeId) ->
    gen_server:call(Graph, {in_degree, NodeId}).

%% ----------------------------------
%%
%% @doc Returns number of edges leaving a node.
%% @end
%%
%% ----------------------------------
-spec out_degree(graph_server(), atom()) -> integer().
out_degree(Graph, NodeId) ->
    gen_server:call(Graph, {out_degree, NodeId}).

%% ----------------------------------
%%
%% @doc Gets all edges on a graph server.
%% @end
%%
%% ----------------------------------
-spec edges(graph_server()) -> mt_dgraphs:graph_edges().
edges(Graph) ->
    gen_server:call(Graph, edges).

%% ----------------------------------
%%
%% @doc Adds an edge to a graph server.
%% @end
%%
%% ----------------------------------
-spec add_edge(graph_server(), atom(), atom()) -> ok | already_exists.
add_edge(Graph, Start, End) ->
    add_edge(Graph, Start, End, []).

%% ----------------------------------
%%
%% @doc Adds an edge to the graph if it doesn't already exist.
%% @end
%%
%% ----------------------------------
-spec add_edge(graph_server(), atom(), atom(), mt_dgraphs:edge_options()) ->
                  ok | already_exists.
add_edge(Graph, Start, End, Opts) ->
    gen_server:call(Graph, {add_edge, Start, End, Opts}).

%% ----------------------------------
%%
%% @doc Addes edges to the graph that don't already exist. Returns number of added edges
%% @end
%%
%% ----------------------------------
-spec add_edges(graph_server(), mt_dgraphs:input_edges()) -> integer().
add_edges(Graph, Edges) ->
    gen_server:call(Graph, {add_edges, Edges}).

%% ----------------------------------
%%
%% @doc Sets multiple edges. Will insert any that are missing.
%% @end
%%
%% ----------------------------------
-spec set_edges(graph_server(), mt_dgraphs:input_edges()) -> ok.
set_edges(Graph, Edges) ->
    gen_server:call(Graph, {set_edges, Edges}).

%% ----------------------------------
%%
%% @doc Sets an edge to have default label and weight. Inserts edge if missing.
%% @end
%%
%% ----------------------------------
-spec set_edge(graph_server(), atom(), atom()) -> ok.
set_edge(Graph, Start, End) ->
    set_edge(Graph, Start, End, []).

%% ----------------------------------
%%
%% @doc Sets an edge. Inserts if missing
%% @end
%%
%% ----------------------------------
-spec set_edge(graph_server(), atom(), atom(), mt_dgraphs:edge_options()) -> ok.
set_edge(Graph, Start, End, Opts) ->
    gen_server:call(Graph, {set_edge, Start, End, Opts}).

%% ----------------------------------
%%
%% @doc Deletes an edge. Returns whether an edge was deleted
%% @end
%%
%% ----------------------------------
-spec delete_edge(graph_server(), atom(), atom()) -> boolean().
delete_edge(Graph, Start, End) ->
    gen_server:call(Graph, {delete_edge, Start, End}).

%% ----------------------------------
%%
%% @doc Deletes edges. Returns number deleted
%% @end
%%
%% ----------------------------------
-spec delete_edges(graph_server(), [atom()]) -> integer().
delete_edges(Graph, Edges) ->
    gen_server:call(Graph, {delete_edges, Edges}).

%% ----------------------------------
%%
%% @doc Gets an edge by the start and end node ids.
%% @end
%%
%% ----------------------------------
-spec edge_by(graph_server(), atom(), atom()) ->
                 {ok, mt_dgraphs:graph_edge()} | {error, not_found}.
edge_by(Graph, Start, End) ->
    gen_server:call(Graph, {edge, Start, End}).

%% ----------------------------------
%%
%% @doc Checks if an edge exists.
%% @end
%%
%% ----------------------------------
-spec edge_exists(graph_server(), atom(), atom()) -> boolean().
edge_exists(Graph, Start, End) ->
    gen_server:call(Graph, {edge, exists, Start, End}).

%% ----------------------------------
%%
%% @doc Gets the label tied to an edge.
%% @end
%%
%% ----------------------------------
-spec get_edge_label(graph_server(), atom(), atom()) -> {ok, any()} | {error, not_found}.
get_edge_label(Graph, Start, End) ->
    gen_server:call(Graph, {edge, label, Start, End}).

%% ----------------------------------
%%
%% @doc Gets the weight tied to an edge.
%% @end
%%
%% ----------------------------------
-spec get_edge_weight(graph_server(), atom(), atom()) ->
                         {ok, mt_dgraphs:edge_weight()} | {error, not_found}.
get_edge_weight(Graph, Start, End) ->
    gen_server:call(Graph, {edge, weight, Start, End}).

%% ----------------------------------
%%
%% @doc Gets all edges connecting to a node.
%% @end
%%
%% ----------------------------------
-spec edges_to(graph_server(), atom()) ->
                  {ok, mt_dgraphs:graph_edges()} | {error, not_found}.
edges_to(Graph, NodeId) ->
    gen_server:call(Graph, {edges_to, NodeId}).

%% ----------------------------------
%%
%% @doc Gets all edges extending from a node.
%% @end
%%
%% ----------------------------------
-spec edges_from(graph_server(), atom()) ->
                    {ok, mt_dgraphs:graph_edges()} | {error, not_found}.
edges_from(Graph, NodeId) ->
    gen_server:call(Graph, {edges_from, NodeId}).

%% ----------------------------------
%%
%% @doc Sets an edge's weight. Will create an edge if missing.
%% @end
%%
%% ----------------------------------
-spec set_edge_weight(graph_server(), {atom(), atom()}, mt_dgraphs:edge_weight()) -> ok.
set_edge_weight(Graph, _Edge = {Start, End}, Weight) ->
    gen_server:call(Graph, {set_edge_weight, Start, End, Weight}).

%% ----------------------------------
%%
%% @doc Sets an edge's label. Will create an edge if missing.
%% @end
%%
%% ----------------------------------
-spec set_edge_label(graph_server(), {atom(), atom()}, any()) -> ok.
set_edge_label(Graph, _Edge = {Start, End}, Label) ->
    gen_server:call(Graph, {set_edge_label, Start, End, Label}).

%% ----------------------------------
%%
%% @doc Splits an edge with a new or existing node.
%%
%% If the node is only a node id, then it will try to reuse an existing node.
%% If the ndoe is {Node, Opts}, then it will overwrite an existing node
%% @end
%%
%% ----------------------------------
-spec split_edge(graph_server(), atom(), atom(), mt_dgraphs:input_node()) ->
                    ok | {error, not_found}.
split_edge(Graph, Start, End, Node) ->
    gen_server:call(Graph, {split_edge, Start, End, Node}).

%% ----------------------------------
%%
%% @doc Checks if a path exists between two nodes.
%% @end
%%
%% ----------------------------------
-spec path_exists(graph_server(), atom(), atom()) -> boolean().
path_exists(Graph, Start, End) ->
    gen_server:call(Graph, {path, exists, Start, End}).

%% ----------------------------------
%%
%% @doc Gets the lowest weighted path between two nodes.
%% @end
%%
%% ----------------------------------
-spec path_to(graph_server(), atom(), atom()) ->
                 {ok, [atom()]} | {error, not_found | none}.
path_to(Graph, Start, End) ->
    gen_server:call(Graph, {path, to, Start, End}).

%% ----------------------------------
%%
%% @doc Gets a list of all non-looping paths between two nodes.
%% @end
%%
%% ----------------------------------
-spec paths(graph_server(), atom(), atom()) ->
               {ok, [] | [[atom()]]} | {error, not_found | none}.
paths(Graph, Start, End) ->
    gen_server:call(Graph, {path, all, Start, End}).

%% ----------------------------------
%%
%% @doc Gets the path with the fewest nodes between two paths.
%% @end
%%
%% ----------------------------------
-spec bfs_path_to(graph_server(), atom(), atom()) ->
                     {ok, [atom()]} | {error, not_found | none}.
bfs_path_to(Graph, Start, End) ->
    gen_server:call(Graph, {path, bfs_to, Start, End}).

%% ----------------------------------
%%
%% @doc Gets the root nodes in a grpah.
%% @end
%%
%% ----------------------------------
-spec roots(graph_server()) -> [] | [atom()].
roots(Graph) ->
    gen_server:call(Graph, roots).

%% ----------------------------------
%%
%% @doc Gets graph information in a map from a graph.
%% @end
%%
%% ----------------------------------
-spec info(graph_server()) -> #{}.
info(Graph) ->
    gen_server:call(Graph, info).

%% ----------------------------------
%%
%% @doc Checks if a graph is empty.
%% @end
%%
%% ----------------------------------
-spec is_empty(graph_server()) -> boolean().
is_empty(Graph) ->
    gen_server:call(Graph, is_empty).

%% ----------------------------------
%%
%% @doc Checks if a graph is acyclic.
%% @end
%%
%% ----------------------------------
-spec is_acyclic(graph_server()) -> boolean().
is_acyclic(Graph) ->
    gen_server:call(Graph, is_acyclic).

%% ----------------------------------
%%
%% @doc Checks if a graph is cyclic.
%% @end
%%
%% ----------------------------------
-spec is_cyclic(graph_server()) -> boolean().
is_cyclic(Graph) ->
    gen_server:call(Graph, is_cyclic).

%% ----------------------------------
%%
%% @doc Checks if a graph is a directed acyclic graph.
%% @end
%%
%% ----------------------------------
-spec is_dag(graph_server()) -> boolean().
is_dag(Graph) ->
    gen_server:call(Graph, is_dag).

%% ----------------------------------
%%
%% @doc Checks if a graph is a tree.
%% @end
%%
%% ----------------------------------
-spec is_tree(graph_server()) -> boolean().
is_tree(Graph) ->
    gen_server:call(Graph, is_tree).

%% ----------------------------------
%%
%% @doc Gets number of edges in the graph.
%% @end
%%
%% ----------------------------------
-spec num_edges(graph_server()) -> integer().
num_edges(Graph) ->
    gen_server:call(Graph, num_edges).

%% ----------------------------------
%%
%% @doc Gets number of nodes in the graph.
%% @end
%%
%% ----------------------------------
-spec num_nodes(graph_server()) -> integer().
num_nodes(Graph) ->
    gen_server:call(Graph, num_nodes).

%% ----------------------------------
%%
%% @doc Gets the node ids in topological sort order.
%% @end
%%
%% ----------------------------------
-spec topsort(graph_server()) -> [] | [atom()].
topsort(Graph) ->
    gen_server:call(Graph, topsort).

%% ----------------------------------
%%
%% @doc Transposes a graph by inverting the direction of edges.
%% @end
%%
%% ----------------------------------
-spec transpose(graph_server()) -> ok.
transpose(Graph) ->
    gen_server:call(Graph, transpose).

%% ----------------------------------
%%
%% @doc Gets the type of the graph.
%% @end
%%
%% ----------------------------------
-spec type(graph_server()) -> mt_dgraphs:graph_type().
type(Graph) ->
    gen_server:call(Graph, type).

%% ----------------------------------
%%
%% @doc Checks if another graph is a subgraph of the root graph.
%% @end
%%
%% ----------------------------------
-spec is_subgraph(graph_server(), graph_server() | mt_dgraphs:graph()) -> boolean().
is_subgraph(RootGraph, SubGraph) ->
    is_subgraph(RootGraph, SubGraph, []).

%% ----------------------------------
%%
%% @doc Checks if another graph is a subgraph of the root graph.
%% @end
%%
%% ----------------------------------
-spec is_subgraph(graph_server(),
                  graph_server() | mt_dgraphs:graph(),
                  mt_dgraphs:subgraph_opts()) ->
                     boolean().
is_subgraph(RootGraph, SubGraph, Opts) ->
    gen_server:call(RootGraph, {is_subgraph, SubGraph, Opts}).

%% ----------------------------------
%%
%% @doc Returns the underlying graph object from a server.
%% @end
%%
%% ----------------------------------
-spec raw_graph(graph_server()) -> mt_dgraphs:graph().
raw_graph(Graph) ->
    gen_server:call(Graph, raw).

%% ----------------------------------
%%
%% @doc Loads graph data from a graph or another graph server. Overwrites all data in memory.
%% @end
%%
%% ----------------------------------
-spec load_graph(graph_server(), graph_server() | mt_dgraphs:graph()) -> ok.
load_graph(Graph, Source) ->
    gen_server:call(Graph, {load_graph, Source}).

%% ----------------------------------
%%
%% @doc Gets the version number of the graph.
%%
%% Version numbers indicate how many changes have been made to a graph server.
%% They can be used for optimistic locking
%% @end
%%
%% ----------------------------------
-spec version(graph_server()) -> integer().
version(Graph) ->
    gen_server:call(Graph, version).

-spec state(graph_server()) -> {integer(), mt_dgraphs:graph()}.
state(Graph) ->
    gen_server:call(Graph, state).

%% ----------------------------------
%%
%% @doc 
%% Performs an optimistic update on the graph.
%%
%% Optimistic updates will grab the current graph state, perform a mutation on it,
%% and then save the output only if the graph state did not change. If the graph state
%% did change, then it will grab the new state and retry applying the transformation
%% up to N times.
%%
%% This allows performing complex transformation changes on a graph without blocking reads.
%% Do notes that this can cause failures or high CPU usage if there are lots of competing optimistic updates.
%% @end
%%
%% ----------------------------------
-spec update_graph(graph_server(),
                   fun((mt_dgraphs:graph()) ->
                           mt_dgraphs:graph() | {ok, mt_dgraphs:graph()} | {error, any()})) ->
                      ok | {error, exceeded_retries}.
update_graph(Graph, Fun) ->
    update_graph(Graph, Fun, 5).

%% ----------------------------------
%%
%% @doc 
%% Performs an optimistic update on the graph.
%%
%% Optimistic updates will grab the current graph state, perform a mutation on it,
%% and then save the output only if the graph state did not change. If the graph state
%% did change, then it will grab the new state and retry applying the transformation
%% up to N times.
%%
%% This allows performing complex transformation changes on a graph without blocking reads.
%% Do notes that this can cause failures or high CPU usage if there are lots of competing optimistic updates.
%% @end
%%
%% ----------------------------------
update_graph(Graph, Fun, Retries) when Retries > 0 ->
    {V, R} = state(Graph),
    {Status, NewGraph} =
        case Fun(R) of
            {ok, G} ->
                {ok, G};
            {error, Err} ->
                {{error, Err}, Graph};
            G = #mt_dgraphs{} ->
                {ok, G};
            Err ->
                {{error, Err}, Graph}
        end,
    case Status of
        E = {error, _} ->
            E;
        _ ->
            Res = gen_server:call(Graph, {optimistic_update, V, NewGraph}),
            case Res of
                ok ->
                    ok;
                _ ->
                    update_graph(Graph, Fun, Retries - 1)
            end
    end;
update_graph(_, _, _) ->
    {error, exceeded_retries}.

%% ----------------------------------
%%
%% @doc Shuts down a graph server.
%% @end
%%
%% ----------------------------------
-spec destroy(graph_server()) -> ok.
destroy(Graph) ->
    gen_server:cast(Graph, stop).

%% Gen Server

%% @doc gen_server:init/1 callback
init([Type, Nodes, Edges]) ->
    {ok, {0, mt_dgraphs:new(Type, Nodes, Edges)}}.

%% @doc gen_server:terminate/2 callback
terminate(_, _) ->
    ok.

%% @doc gen_server:handle_call/3 callback
handle_call(nodes, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:nodes(Graph), {Version, Graph}};
handle_call(node_ids, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:node_ids(Graph), {Version, Graph}};
handle_call({node, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:node_by_id(Graph, NodeId), {Version, Graph}};
handle_call({add_node, NodeId, Opts}, _From, {Version, Graph}) ->
    {Res, NewGraph} = mt_dgraphs:add_node(Graph, NodeId, Opts),
    NewVersion =
        case Res of
            ok ->
                Version + 1;
            _ ->
                Version
        end,
    {reply, Res, {NewVersion, NewGraph}};
handle_call({add_nodes, Nodes}, _From, {Version, Graph}) ->
    {Res, NewGraph} = mt_dgraphs:add_nodes(Graph, Nodes),
    NewVersion =
        case Res of
            0 ->
                Version;
            _ ->
                Version + 1
        end,
    {reply, Res, {NewVersion, NewGraph}};
handle_call({set_nodes, Nodes}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_nodes(Graph, Nodes)}};
handle_call({node, label, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:get_node_label(Graph, NodeId), {Version, Graph}};
handle_call({set_node_label, NodeId, Label}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_node_label(Graph, NodeId, Label)}};
handle_call({node, exists, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:node_exists(Graph, NodeId), {Version, Graph}};
handle_call({set_node, NodeId, Opts}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_node(Graph, NodeId, Opts)}};
handle_call({delete_node, NodeId}, _From, {Version, Graph}) ->
    {Res, NG} = mt_dgraphs:delete_node(Graph, NodeId),
    NewVersion =
        case Res of
            true ->
                Version + 1;
            _ ->
                Version
        end,
    {reply, Res, {NewVersion, NG}};
handle_call({delete_nodes, NodeIds}, _From, {Version, Graph}) ->
    {Res, NG} = mt_dgraphs:delete_nodes(Graph, NodeIds),
    NewVersion =
        case Res of
            0 ->
                Version;
            _ ->
                Version + 1
        end,
    {reply, Res, {NewVersion, NG}};
handle_call({in_degree, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:in_degree(Graph, NodeId), {Version, Graph}};
handle_call({out_degree, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:out_degree(Graph, NodeId), {Version, Graph}};
handle_call(edges, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:edges(Graph), {Version, Graph}};
handle_call({add_edge, Start, End, Opts}, _From, {Version, Graph}) ->
    {Res, NewGraph} = mt_dgraphs:add_edge(Graph, Start, End, Opts),
    NewVersion =
        case Res of
            ok ->
                Version + 1;
            _ ->
                Version
        end,
    {reply, Res, {NewVersion, NewGraph}};
handle_call({set_edge, Start, End, Opts}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_edge(Graph, Start, End, Opts)}};
handle_call({add_edges, Edges}, _From, {Version, Graph}) ->
    {Res, NewGraph} = mt_dgraphs:add_edges(Graph, Edges),
    NewVersion =
        case Res of
            0 ->
                Version;
            _ ->
                Version + 1
        end,
    {reply, Res, {NewVersion, NewGraph}};
handle_call({set_edges, Edges}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_edges(Graph, Edges)}};
handle_call({delete_edges, Edges}, _From, {Version, Graph}) ->
    {Res, NewGraph} = mt_dgraphs:delete_edges(Graph, Edges),
    NewVersion =
        case Res of
            0 ->
                Version;
            _ ->
                Version + 1
        end,
    {reply, Res, {NewVersion, NewGraph}};
handle_call({delete_edge, Start, End}, _From, {Version, Graph}) ->
    {Res, NG} = mt_dgraphs:delete_edge(Graph, Start, End),
    NewVersion =
        case Res of
            true ->
                Version + 1;
            _ ->
                Version
        end,
    {reply, Res, {NewVersion, NG}};
handle_call({edge, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:edge_by(Graph, Start, End), {Version, Graph}};
handle_call({edge, exists, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:edge_exists(Graph, Start, End), {Version, Graph}};
handle_call({edge, label, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:get_edge_label(Graph, Start, End), {Version, Graph}};
handle_call({edge, weight, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:get_edge_weight(Graph, Start, End), {Version, Graph}};
handle_call({edges_to, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:edges_to(Graph, NodeId), {Version, Graph}};
handle_call({edges_from, NodeId}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:edges_from(Graph, NodeId), {Version, Graph}};
handle_call({set_edge_weight, Start, End, Weight}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_edge_weight(Graph, {Start, End}, Weight)}};
handle_call({set_edge_label, Start, End, Label}, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:set_edge_label(Graph, {Start, End}, Label)}};
handle_call({split_edge, Start, End, Node}, _From, {Version, Graph}) ->
    {Res, NewGraph} =
        case mt_dgraphs:split_edge(Graph, Start, End, Node) of
            R = {ok, _} ->
                R;
            E ->
                {E, Graph}
        end,
    {reply, Res, {Version + 1, NewGraph}};
handle_call({path, exists, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:path_exists(Graph, Start, End), {Version, Graph}};
handle_call({path, to, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:path_to(Graph, Start, End), {Version, Graph}};
handle_call({path, all, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:paths(Graph, Start, End), {Version, Graph}};
handle_call({path, bfs_to, Start, End}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:bfs_path_to(Graph, Start, End), {Version, Graph}};
handle_call(roots, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:roots(Graph), {Version, Graph}};
handle_call(info, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:info(Graph), {Version, Graph}};
handle_call(is_empty, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_empty(Graph), {Version, Graph}};
handle_call(is_acyclic, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_acyclic(Graph), {Version, Graph}};
handle_call(is_cyclic, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_cyclic(Graph), {Version, Graph}};
handle_call(is_dag, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_dag(Graph), {Version, Graph}};
handle_call(is_tree, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_tree(Graph), {Version, Graph}};
handle_call(num_edges, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:num_edges(Graph), {Version, Graph}};
handle_call(num_nodes, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:num_nodes(Graph), {Version, Graph}};
handle_call(topsort, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:topsort(Graph), {Version, Graph}};
handle_call(transpose, _From, {Version, Graph = #mt_dgraphs{type = undirected}}) ->
    {reply, ok, {Version, Graph}};
handle_call(transpose, _From, {Version, Graph}) ->
    {reply, ok, {Version + 1, mt_dgraphs:transpose(Graph)}};
handle_call(type, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:type(Graph), {Version, Graph}};
handle_call({is_subgraph, SubGraph, Opts}, _From, {Version, Graph})
    when is_pid(SubGraph), SubGraph =:= self() ->
    {reply, mt_dgraphs:is_subgraph(Graph, Graph, Opts), {Version, Graph}};
handle_call({is_subgraph, SubGraph, Opts}, _From, {Version, Graph})
    when is_pid(SubGraph) ->
    {reply, mt_dgraphs:is_subgraph(Graph, raw_graph(SubGraph), Opts), {Version, Graph}};
handle_call({is_subgraph, SubGraph, Opts}, _From, {Version, Graph}) ->
    {reply, mt_dgraphs:is_subgraph(Graph, SubGraph, Opts), {Version, Graph}};
handle_call({load_graph, Source}, _From, {Version, Graph})
    when is_pid(Source), Source =:= self() ->
    {reply, ok, {Version, Graph}};
handle_call({load_graph, Source}, _From, {Version, _Graph}) when is_pid(Source) ->
    {reply, ok, {Version + 1, raw_graph(Source)}};
handle_call({load_graph, Source}, _From, {Version, _Graph}) ->
    {reply, ok, {Version + 1, Source}};
handle_call(raw, _From, {Version, Graph}) ->
    {reply, Graph, {Version, Graph}};
handle_call(version, _From, {Version, Graph}) ->
    {reply, Version, {Version, Graph}};
handle_call(state, _From, {Version, Graph}) ->
    {reply, {Version, Graph}, {Version, Graph}};
handle_call({optimistic_update, OldVersion, NewGraph = #mt_dgraphs{}},
            _From,
            {Version, Graph}) ->
    if OldVersion =:= Version ->
           {reply, ok, {Version + 1, NewGraph}};
       true ->
           {reply, {error, mismatch_versions}, {Version, Graph}}
    end;
handle_call(_Msg, _From, {Version, Graph}) ->
    {reply, {error, bad_msg}, {Version, Graph}}.

%% @doc gen_server:handle_cast/2 callback
handle_cast(stop, {Version, Graph}) ->
    {stop, normal, {Version, Graph}};
handle_cast(_Msg, {Version, Graph}) ->
    {noreply, {Version, Graph}}.

%% @doc gen_server:handle_info/2 callback
handle_info(_Msg, {Version, Graph}) ->
    {noreply, {Version, Graph}}.

%% @doc gen_server:code_change/3 callback
code_change(_OldVsn, {Version, Graph}, _Extra) ->
    {ok, {Version + 1, Graph}}.
