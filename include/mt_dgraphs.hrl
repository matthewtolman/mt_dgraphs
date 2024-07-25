
-record(mt_dgraphs,
        {type = directed :: directed | undirected, nodes = #{} :: map(), edges = #{} :: map()}).
-record(mt_dgraphs_node, {id = nil :: atom(), label = nil :: any()}).
-record(mt_dgraphs_edge,
        {v1 = nil :: atom(),
         v2 = nil :: atom(),
         weight = 1 :: integer() | float(),
         label = nil :: any()}).

