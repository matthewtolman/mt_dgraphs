mt_dgraphs
=====

Mathematical graph library. Supports directed and undirected graphs.

## Build

    $ rebar3 compile

## Modules

- **mt_dgraphs** - Immutable grpah library
- **mt_dgraphs_server** - GenServer wrapper around mt_dgraph

### mt_dgraphs

This module is for working with immutable graphs. Each "update" returns a new graph with the update applied. There are two types of graphs: directed and undirected. Directed graphs can have one way edges (A -> B). Undirected graphs have only two-way edges. For undirected graphs, the "two-way" edges are represented by having two directed edges (one for each direction). This means that methods that return all the edges for a graph will return twice as many edges for undirected graphs as they would directed graphs. It also means edge updating operations work on a pair of edges rather than on a single edge. This difference of behavior can cause subtle differences between directed and undirected graphs.

Methods available for working on graphs:

* **add_edge** - Adds an edge to a graph only if it doesn't exist. Creates nodes if needed.
* **add_edges** - Adds edges to a graph only if they don't exist. Creates nodes if needed.
* **add_node** - Adds a node to the graph if it doesn't exist.
* **add_nodes** - Adds nodes to a graph if it doesn't exist
* **set_edge** - Updates or inserts an edge. Creates nodes if needed.
* **set_edges** - Updates or inserts edges. Creates nodes if needed.
* **set_node** - Updates or inserts a node.
* **set_nodes** - Updates or inserts nodes.
* **set_edge_label** - Sets the label for an edge
* **set_edge_weight** - Sets the weight for an edge
* **set_node_label** - Sets the label for a node
* **delete_edge** - Deletes an edge if it exists
* **delete_edges** - Deletes edges if they exist
* **delete_node** - Deletes a node (and associated edges) if it exists
* **delete_nodes** - Deletes nodes (and associated edges)
* **edge_by** - Gets an edge by the start and ending node id
* **edge_exists** - Checks if an edge exists from one node to another
* **edges** - Gets the edges for a graph
* **edges_from** - Gets the edges leading out of a node
* **edges_to** - Gets the edges leading to a node
* **get_edge_label** - Gets the label for an edge (if it exists)
* **get_edge_weight** - Gets the weight for an edge (if it exists)
* **get_node_label** - Gets the label for a node (if it exists)
* **in_degree** - Gets the number of edges going into a node
* **out_degree** - Gets the number of edges coming out of a node
* **info** - Returns a map describing the graph
* **is_acyclic** - Checks if the graph is acyclic
* **is_cyclic** - Checks if the graph is cyclic
* **is_dag** - Checks that the graph is of type directed and is acyclic
* **is_empty** - Checks if the graph is empty (no nodes or edges)
* **is_subgraph** - Checks if a graph is a subgraph of another graph
* **is_tree** - Checks if a graph is a tree
* **new** - Creates a new graph
* **node_by_id** - Gets a node by id (if it exists)
* **node_ids** - Gets a list of all ids for all nodes
* **nodes** - Gets a list of all nodes
* **num_edges** - Counts the number of edges in a graph (for undirected graphs this number is pre-divided by 2)
* **num_nodes** - Counts the number of nodes in a graph
* **path_exists** - Checks if a path exists between two nodes
* **path_to** - Gets the lowest weight path between two nodes
* **bfs_path_to** - Gets the shortest path between two nodes
* **paths** - Returns all non-cyclic paths between two nodes
* **roots** - Return a list of all node ids that have an in degree of 0
* **split_edge** - Splits an edge with a node (will create the node if needed)
* **topsort** - Returns a list of node ids in topological sort order (must be acyclic)
* **transpose** - Reverses the direction of all edges (noop on undirected graphs)
* **type** - Returns the type of the graph (directed or undirected)

Methods available for working on record types:

* **edge_end** - Gets the ending node id for an edge
* **edge_start** - Gets the starting node id for an edge
* **edge_label** - Gets the label for an edge
* **edge_weight** - Gets the weight of an edge
* **node_id** - Gets the id of a node
* **node_label** - Gets the label of a node

### mt_dgraphs_server

This is a gen server which has a mt_dgraphs as the internal state. It allows sharing graph data between processes as well as having a "mutable" graph.

This has all of the graph functions that mt_dgraphs does (but not the record functions) as well as the following new methods:

* **destroy** - Shuts down the server
* **version** - Gets the version number of the graph. Version numbers are specific to each server and increment automatically whenever a mutating operation is done
* **start_link** - Creates a new server
* **new** - Alias for **start_link**
* **update_graph** - Perform an optimistic update of the graph

