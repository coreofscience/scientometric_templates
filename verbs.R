pattern_authors <- 
  SPC %R% 
  one_or_more(WRD) %R%
  SPC %R%
  one_or_more(or(WRD, ANY_CHAR))

pattern_titles <- 
  OPEN_PAREN %R% 
  repeated(DGT, 4) %R% 
  CLOSE_PAREN %R%
  one_or_more(or(WRD,ANY_CHAR))

pattern_year <- 
  OPEN_PAREN %R% 
  repeated(DGT, 4) %R% 
  CLOSE_PAREN 

pattern_journal <- 
  one_or_more(or(WRD,SPC))

pattern_volume <-
  one_or_more(or(WRD, SPC))

pattern_pages <- 
  "PP. " %R%
  one_or_more(or(DGT, ANY_CHAR))
  
  

add_clusters_to_nodes <- function(graph) {
  
  clusters <- 
    graph |> 
    as.undirected(mode = "each") |> 
    igraph::cluster_louvain()
  
  graph_clusters <- 
    graph |> 
    set_vertex_attr(name = "cluster",
                    value = membership(clusters))
  
  return(graph_clusters)
  
}
