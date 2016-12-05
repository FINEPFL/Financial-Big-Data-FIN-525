community_detection = function(distance_matrix, select_algorithm = 1){

  community_graph = graph_from_adjacency_matrix(distance_matrix, mode='undirected', weighted = TRUE, diag=FALSE, add.colnames=NULL)
  min_span_tree = minimum.spanning.tree(community_graph)

  if (select_algorithm == 1){
    detected_community = cluster_fast_greedy(min_span_tree)
  }
  else{
    detected_community = cluster_walktrap(min_span_tree)
  }

  return_list = list(minimum_spanning_tree = min_span_tree, community = detected_community)
  return(return_list)
}
