get_references <- function(data) {
  
  references_df <-
    data |>
    dplyr::filter(!duplicated(TI)) |>
    dplyr::select(SR, CR) |>
    na.omit()  |>
    tidyr::separate_rows(CR, sep = "; ") |>
    dplyr::mutate(PY = str_extract(CR, "\\([0-9]{4}\\)"),
                  PY = str_remove_all(PY, "[\\(\\)]"),
                  PY = as.numeric(PY)) |>
    na.omit() |>
    dplyr::mutate(AU = str_extract(CR, ".*\\([0-9]{4}\\)"),
                  AU = str_extract(AU, ".*\\.,"),
                  AU = gsub("([^,]+,[^,]+),", "\\1;", AU),
                  AU = str_sub(AU, 1, nchar(AU)-1),
                  AU = str_replace_all(AU,
                                       pattern = "; ",
                                       replacement = ";"),
                  AU = str_remove_all(AU, pattern = "\\."),
                  AU = str_remove_all(AU, pattern = ",")) |>
    dplyr::mutate(type_ref = if_else(str_detect(CR,
                                                "\\., \\("), 2, # books
                                     if_else(str_detect(CR,
                                                        "^\\([0-9]{4}\\)"), 3,
                                             if_else(str_detect(CR,
                                                                " \\([0-9]{4}\\), "), 4,
                                                     1)))) |> # papers
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_extract(CR,
                                           ".*\\([0-9]{4}\\)"),
                               CR)) |>
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_remove(TI, "\\([0-9]{4}\\)"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_remove(TI, ".*\\., "),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_trim(TI),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_remove(TI, "\""),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 1,
                               str_remove(TI, "\"$"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 2,
                               str_extract(CR,
                                           "\\([0-9]{4}\\).*"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 2,
                               str_remove(TI, "\\([0-9]{4}\\)"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 2,
                               str_remove(TI, ", [0-9].*"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 2,
                               str_trim(TI),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 3,
                               str_remove(CR, "\\([0-9]{4}\\)"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 3,
                               str_remove(TI, ", ,.*"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 3,
                               str_trim(TI),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 4,
                               str_extract(CR,
                                           ".* \\([0-9]{4}\\) "),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 4,
                               str_remove(TI, "\\([0-9]{4}\\)"),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 4,
                               str_remove(TI, ".*\\., "),
                               TI)) |>
    dplyr::mutate(TI = if_else(type_ref == 4,
                               str_trim(TI),
                               TI)) |>
    dplyr::mutate(JI = if_else(type_ref == 1,
                               str_remove(CR, ".*\\([0-9]{4}\\)"),
                               CR)) |>
    dplyr::mutate(JI = if_else(type_ref == 1,
                               str_remove(JI, ", .*"),
                               JI)) |>
    dplyr::mutate(JI = if_else(type_ref == 1,
                               str_trim(JI),
                               JI)) |>
    dplyr::filter(JI != "") |>
    dplyr::mutate(JI = str_remove_all(JI, "\\.")) |>
    dplyr::mutate(SR_ref = gsub("^(.*?);.*", "\\1", AU),
                  SR_ref = str_c(SR_ref, ", ", PY, ", ", JI, sep = "")) |>
    dplyr::select(SR, SR_ref, TI, AU, JI, PY,
                  ref_type = type_ref, CR_ref = CR) |>
    dplyr::filter(!is.na(SR_ref))
  # Finding right SR_ref in main dataset
  # references_df_1 <- tibble()
  #
  # list_ref_TI <-
  #   references_df |>
  #   filter(ref_type == 1) |>
  #   select(TI) |>
  #   dplyr::distinct()
  
  # for (i in list_ref_TI$TI) {
  #
  #   df_1 <-
  #     scopus_df |>
  #     filter(TI == i)
  #
  #   if (length(df_1$TI) != 0) {
  #
  #     ref_1 <- references_df %>% filter(!is.na(TI))
  #     ref_1$SR_ref[ref_1$TI == df_1$TI] <- df_1$SR
  #
  #     references_df$SR_ref[references_df$TI == df_1$TI] <- df_1$SR
  #
  #     references_df_1 <-
  #       references_df |>
  #       mutate(SR_ref = replace(SR_ref,
  #                               TI == i,
  #                               df_1 |>
  #                                 filter(TI == i) |>
  #                                 select(SR) |>
  #                                 pull()))
  #   }
  #
  # }
  
  # Finding duplicate titles in references with different SR_ref
  
  TI_ref_duplicates <-
    references_df |>
    dplyr::filter(ref_type == 1 | ref_type == 2) |>
    dplyr::group_by(TI, SR_ref) |>
    dplyr::count(TI, sort = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(TI) |>
    dplyr::group_by(TI) |>
    dplyr::count(TI) |>
    dplyr::filter(n > 1) |>
    dplyr::select(TI)
  
  # Create the
  
  df_1 <- references_df
  
  for (i in TI_ref_duplicates$TI) {
    
    SR_ref_1 <-
      references_df |>
      dplyr::filter(TI == i) |>
      dplyr::select(SR_ref) |>
      dplyr::group_by(SR_ref) |>
      dplyr::count(SR_ref, sort = TRUE) |>
      dplyr::ungroup() |>
      dplyr::slice(1) |>
      dplyr::select(SR_ref)
    
    df_1 <-
      df_1 |>
      dplyr::mutate(SR_ref = if_else(TI == i, SR_ref_1$SR_ref,
                                     SR_ref))
    
  }
  
  # Converting long journal names in short names.
  
  df_2 <-
    data |>
    dplyr::select(JI, SO) |>
    dplyr::distinct() |>
    dplyr::filter(!duplicated(SO)) |>
    dplyr::mutate(JI = str_remove_all(JI, "\\."),
                  JI = str_trim(JI)) |>
    dplyr::rename(JI_main = JI)
  
  df_3 <-
    references_df |>
    left_join(df_2, by = c("JI" = "SO")) |>
    dplyr::mutate(JI_main = if_else(is.na(JI_main), JI, JI_main),
                  SR_new = if_else(ref_type == 1,
                                   str_extract(SR_ref, "([^,]*,[^,]*)"),
                                   SR_ref),
                  SR_new = if_else(ref_type == 1,
                                   str_c(SR_new, JI_main, sep = ", "),
                                   SR_ref),
                  SR_new = if_else(is.na(SR_new), SR_ref,
                                   SR_new)) |>
    dplyr::filter(!is.na(SR_new)) |>
    dplyr::select(SR, SR_ref = SR_new, TI, AU, JI, PY, CR_ref, ref_type)
  
  
  return(references_df = df_3)
}


get_asn <- function(data, source = "scopus") {
  
  data_tidied_scopus_ref <-
    data |>
    dplyr::select(SR, CR) |>
    tidyr::separate_rows(CR, sep = "; ") |>
    na.omit() |>
    dplyr::mutate(PY = str_extract(CR, "\\([0-9]{4}\\)"),
                  PY = str_remove_all(PY, "[\\(\\)]")) |>
    na.omit() |>
    dplyr::mutate(AU = str_extract(CR, ".*\\([0-9]{4}\\)"),
                  AU = str_extract(AU, ".*\\.,"),
                  AU = gsub("([^,]+,[^,]+),", "\\1;", AU),
                  AU = str_sub(AU, 1, nchar(AU)-1),
                  AU = str_replace_all(AU,
                                       pattern = "; ",
                                       replacement = ";"),
                  AU = str_remove_all(AU, pattern = "\\."),
                  AU = str_remove_all(AU, pattern = ",")) |>
    na.omit()|>
    dplyr::rename(main_ref = SR,
                  id_ref = CR)
  
  dummy <-
    data_tidied_scopus_ref |>
    dplyr::select(AU, PY) |>
    dplyr::mutate(PY = as.numeric(PY)) |>
    dplyr::bind_rows(data |>
                       dplyr::select(AU, PY)) |>
    unique()
  
  asn <-
    biblioNetwork(M = data.frame(dummy),
                  analysis = "collaboration",
                  network = "authors") |>
    graph_from_adjacency_matrix(mode = "undirected",
                                weighted = TRUE) |>
    simplify() |>
    as_tbl_graph() |>
    activate(nodes) |>
    mutate(communities = group_components(type = "weak")) |>
    filter(communities == 1)
  
  return(asn)
}

get_asn_wos <- function(data) {
  
  ### 1. Selecting dois from refs
  
  DOI <-
    data |>
    dplyr::filter(ref_type == "wos") |>
    dplyr::select(CR) |>
    tidyr::separate_rows(CR, sep = ";") |>
    dplyr::filter(str_detect(CR, "DOI"))  |>
    dplyr::mutate(DOI = str_remove(CR, ".*DOI ")) |>
    distinct(DOI)
  
  # 2. Getting authors from dois ref
  
  # Extracci?n de la informaci?n de los art?culos de las referencias ####
  
  references <- data.frame(DI = character(),
                           PU = character(),
                           SO = character(),
                           J9 = character(),
                           PD = character(),
                           PY = character(),
                           TI = character(),
                           AF = character(),
                           stringsAsFactors = FALSE)
  authors <- data.frame(doi = character(),
                        author = character(),
                        year = character(),
                        month = character(),
                        stringsAsFactors = FALSE)
  for (i in DOI$DOI) {
    doi <- i
    url <- paste0("http://api.crossref.org/works/", doi, ".xml")
    xml_data_1 = try(xmlParse(url), silent = TRUE);
    if (class(xml_data_1) == "try-error") {
      next
    } else  {
      xml_data_2 <- xmlToList(xml_data_1)
      
      notfound =try(as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article", silent = TRUE);
      if (class(notfound) == "try-error"){
        next
      }else{
        
        if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
          
          # PUBLISHER-NAME
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])){
            PU <- as.character(NA)
          } else {
            
            publisher0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])
            publisher <- data.frame(publisher0)
            if(nrow(publisher) == 0){
              PU <- as.character(NA)
            }else{
              PU <- as.character(publisher$text[1])
            }
          }
          
          # JOURNAL FULL TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])){
            SO <- as.character(NA)
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              SO <- as.character(NA)
            }else{
              SO <- as.character(journal[1,1])
            }
          }
          
          # JOURNAL ABBREV TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])){
            J9 <- as.character(NA)
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              J9 <- as.character(NA)
            }else{
              J9 <- as.character(journal[1,1])
            }
          }
          
          # MONTH
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])){
            PD <- as.character(NA)
          } else {
            
            month0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])
            month <- data.frame(month0)
            if(nrow(month) == 0){
              PD <- as.character(NA)
            }else{
              PD <- as.character(month[1,1])
            }
          }
          
          # YEAR
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])){
            PY <- as.character(NA)
          } else {
            
            Year0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])
            Year <- data.frame(Year0)
            if(nrow(Year) == 0){
              PY <- as.character(NA)
            }else{
              PY <- as.character(Year[1,1])
            }
          }
          
          # TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])){
            TI <- as.character(NA)
          } else {
            
            title0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])
            title <- try(data.frame(title0), silent = TRUE);
            
            if(class(title) == "try-error"){
              titlex <- try(ldply(title0, data.frame), silent = TRUE);
              if(class(titlex) == "try-error"){
                TI <- as.character(NA)
              }else{
                TI0 <- as.character(titlex[1,2])
                TI <- trimws(TI0)
              }
            }else{
              if(nrow(title) == 0){
                TI <- as.character(NA)
              }else{
                TI <- as.character(title[1,1])
              }
            }
          }
          
          # CONTRIBUTORS
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
          {
            AF <- as.character(NA)
          } else {
            
            author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
            
            author_1_ref <- ldply(author_ref, data.frame)
            author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
            
            authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name), year = PY, month = PD)
            
            authors = rbind(authors, authorss)
            authorss$author <- trim(authorss$author)
            AF <- as.character(paste(authorss$author, collapse = ";   "))
          }
          
          references0 <- data.frame(DI = doi, PU = PU, SO = SO, J9 = J9, PD = PD, PY = PY, TI = TI, AF = AF)
          references = rbind(references, references0)
          
        }else {
          next}
      }
    }
  }
  authors$month <- sub("^[0]+", "", authors$month) # Elimina los ceros a la izquierda en los meses
  
  ### 3. Getting authors from main wos
  
  ##########################################################################
  #### Separaci?n de los nombres de los autores de los art?culos de WoS ####
  ##########################################################################
  wos_author <- # 1339 - 3
    wos_scopus_tos$df |>
    dplyr::filter(ref_type == "wos") |>
    dplyr::select(doi = DI, AU, PY) |>
    tidyr::separate_rows(AU, sep = ";") |>
    dplyr::rename(author = AU)
  
  ### 4. Merging main with refs dois with names
  
  #################################################################
  #### Uni?n de los datos de WoS y de las referencias de estos ####
  #################################################################
  authors$author <- gsub("(?<=, [[:alpha:]]).*", "", authors$author, perl=TRUE)
  wos_author_ref <- # 27051 3 doi author year
    authors |>
    dplyr::mutate(author = str_replace(author, ",", "")) |>
    dplyr::select(doi, author, year) |>
    dplyr::mutate(author = str_to_upper(author))
  
  ### 5. Getting edge list from (4)
  
  # We need to remove dois with only one author
  
  dois_one_author <-
    wos_author_ref |>
    count(doi) |>
    filter(n == 1) |> # 970 total
    select(doi)
  
  # We need to identify dois with high number of co-authors
  
  dois_high_author <-
    wos_author_ref |>
    count(doi, sort = TRUE) |>
    slice(1:9) |>
    select(doi)
  
  # We need to merge these two dataframes
  
  dois_to_remove <-
    dois_one_author |>
    bind_rows(dois_high_author)
  
  # We need to remove the dois from wos_author_ref
  
  wos_author_removed <-
    wos_author_ref |>
    filter(!(doi %in% dois_to_remove$doi))
  
  ###########################
  #### Listas de enlaces ####
  ###########################
  # # Para art?culos que tienen como m?nimo dos autores
  # author_1 <- # 4273 3 there a mistake with the duplicate values.
  #   wos_author |> # 190
  #   filter(!is.na(PY)) |>
  #   dplyr::rename(year = PY,
  #                 doi = DI) |>
  #   bind_rows(wos_author_ref |>
  #               mutate(year = as.numeric(year))) |> # 4104
  #   unique() |>
  #   filter(!(doi %in% "10.3109/13880209.2014.922589" &
  #          year == 2014)) # this is a duplicate value with != year
  edgelist_wos_authors <- data.frame(Source = character(),
                                     Target = character(),
                                     doi = character(),
                                     year = as.numeric(),
                                     stringsAsFactors = FALSE)
  # table_ids <- table(author_1$doi)
  # table_ids_0 <- data.frame(table_ids)
  # table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
  list_ids_1 <- unique(wos_author_removed$doi)
  for (i in list_ids_1) {
    df_1 = wos_author_removed[wos_author_removed$doi == i,] |>
      filter(!is.na(doi))
    df_2 = combn(df_1$author, 2, simplify = FALSE)
    df_3 = data.frame((t(data.frame(df_2))), i)
    colnames(df_3) = c("Source", "Target", "doi")
    df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
    edgelist_wos_authors = rbind(edgelist_wos_authors, df_4)
  }
  
  
  ### 6. Creating the graph object
  
  
  asn_wos <-
    graph.data.frame(edgelist_wos_authors,
                     directed = FALSE) |>
    simplify() |>
    as_tbl_graph()
  
  return(asn_wos)
}

get_citation_network <- function(scopus_df, references_df) {
  
  sca_citation_network <-
    references_df |>
    filter(ref_type == 1) |>
    select(SR, SR_ref) |>
    na.omit() |>
    dplyr::distinct() |>
    as_tbl_graph()
  
  sca_data_cleaned_1 <-
    scopus_df |>
    select(SR, TI, PY) |>
    bind_rows(references_df |>
                select(SR = SR_ref,
                       TI,
                       PY)) |>
    dplyr::distinct() |>
    dplyr::rename(name = SR) |>
    filter(!duplicated(name))
  
  sca_citation_network_1 <-
    sca_citation_network |>
    convert(to_simple) |>
    activate(nodes) |>
    left_join(sca_data_cleaned_1, by = "name")
  
  return(citation_network = sca_citation_network_1)
}

get_citation_network_tos <- function(citation_network) {
  
  citation_network_gc <-
    citation_network |>
    activate(nodes) |>
    mutate(component = group_components(type = "weak")) |>
    filter(component == 1) %>%
    mutate(in_degree = centrality_degree(mode = "in"),
           out_degree = centrality_degree(mode = "out")) |>
    filter(!(in_degree == 1 & out_degree == 0)) |>
    select(-in_degree, -out_degree)
  
  subfields <-
    citation_network_gc |>
    tidygraph::to_undirected() |>
    activate(nodes) |>
    mutate(subfield = tidygraph::group_louvain()) |>
    as_tibble() |>
    select(name, subfield)
  
  citation_network_subfield <-
    citation_network_gc |>
    activate(nodes) |>
    left_join(subfields, by = "name")
  
  df_tos = tibble()
  
  for (i in unique(subfields$subfield) ) {
    
    df_1 <-
      citation_network_subfield |>
      activate(nodes) |>
      filter(subfield == i) |>
      mutate(in_degree = centrality_degree(mode = "in"),
             out_degree = centrality_degree(mode = "out"),
             bet = centrality_betweenness()) |>
      as_tibble() |>
      select(name,
             in_degree,
             out_degree,
             bet
      )
    
    df_tos <-
      df_tos |>
      bind_rows(df_1)
    
  }
  
  citation_network_tos <-
    citation_network_subfield |>
    activate(nodes) |>
    left_join(df_tos, by = "name") |>
    select(-component)
  
  return(citation_network_tos = citation_network_tos)
  
}

get_sap <- function(citation_network_tos) {
  nodes <-
    citation_network_tos %>%
    activate(nodes) %>%
    data.frame() %>%
    rownames_to_column("rowid") %>%
    mutate(rowid = as.integer(rowid))
  
  # edges <-
  #   citation_network %>%
  #   activate(edges) %>%
  #   data.frame()
  #
  # for (i in 1 : nrow(edges)){
  #   from = edges[i, 1]
  #   to   = edges[i, 2]
  #   edges[i, 1] = nodes[from, 'name']
  #   edges[i, 2] = nodes[to, 'name']
  # }
  
  edges <-
    citation_network_tos |>
    as.igraph() |>
    get.edgelist()
  
  
  # g <- graph_from_data_frame(edges, directed = TRUE) %>%
  #   simplify()
  
  # # Se eliminan los vertices con indegree = 1 y con outdegree = 0
  # g1 <- delete.vertices(g,
  #                       which(degree(g, mode = "in") == 1 &
  #                               degree(g, mode = "out") == 0))
  
  # Se escoge el componente mas grande conectado
  # g2 <- giant_component_extract(g1, directed = TRUE)
  # g2 <- g2[[1]]
  
  
  # metricas.red <- tibble(
  #   id        = V(g2)$name,
  #   indegree  = degree(g2, mode = "in"),
  #   outdegree = degree(g2, mode = "out"),
  #   bet       = betweenness(g2))
  
  metricas.red <-
    citation_network_tos |>
    activate(nodes) |>
    mutate(id = .tidygraph_node_index,
           indegree = centrality_degree(mode = "in"),
           outdegree = centrality_degree(mode = "out"),
           betweenness = centrality_betweenness(),
           year = PY) |>
    as_tibble() |>
    select(id, indegree, outdegree, bet = betweenness, year)
  
  #
  #   metricas.red <- metricas.red %>%
  #     mutate(year = as.numeric(str_extract(id, "[0-9]{4}")))
  
  
  
  # Clasificacion de las raices
  
  Raices <- metricas.red[metricas.red$outdegree == 0, c("id","indegree")] %>%
    arrange(desc(indegree))
  Raices <- Raices[1:10,]
  
  # Clasificacion de las hojas
  Hojas.ext <- metricas.red[metricas.red$indegree == 0, c("id","outdegree","year")]
  act.year  <- as.numeric(format(Sys.Date(),'%Y'))
  Hojas.ext <- Hojas.ext %>%
    mutate(antiguedad = act.year - year) %>%
    arrange(antiguedad)
  Hojas     <- filter(Hojas.ext, antiguedad <= 5)
  
  # Se determina el numero del vertice de las Hojas
  num.vertices.hojas <- c()
  for (vertice in Hojas$id){
    num.vertices.hojas <- c(num.vertices.hojas,which(metricas.red$id == vertice))
  }
  
  # Se determina el numero del vertice de las raices
  num.vertices.raices <- c()
  for (vertice in Raices$id){
    num.vertices.raices <- c(num.vertices.raices,which(metricas.red$id == vertice))
  }
  
  
  # Calculo del SAP de las Hojas
  SAP_hojas <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(citation_network_tos,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")
    
    SAP_hojas   <- c(SAP_hojas, length(h[[1]]))
  }
  
  Hojas <- Hojas %>%
    mutate(SAP = SAP_hojas) %>%
    arrange(desc(SAP))
  
  Hojas <- Hojas[1:60,] %>%
    filter(SAP > 0)
  
  Caminos   <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(citation_network_tos,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")
    lista.nodos <- unique(unlist(h[1]))
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
    Caminos     <- c(Caminos,lista.nodos)
  }
  
  # Seleccion del tronco
  
  Tronco     <- metricas.red[unique(Caminos), c("id","indegree","year")]
  mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
  Tronco     <- Tronco %>%
    mutate(antiguedad = mas.nuevo - year)
  
  # Tree of science
  Raices$TOS <- "Root"
  Hojas$TOS  <- "Leaves"
  Tronco$TOS <- "Trunk"
  
  TOS   <- rbind(Raices[,c(1,3)], Tronco[,c(1,5)], Hojas[,c(1,6)])
  
  TOS <-
    TOS |>
    left_join(citation_network_tos |>
                activate(nodes) |>
                as_tibble() |>
                select(id = .tidygraph_node_index, TI, PY, name),
              by = "id")
  
  return(TOS)
  
}

get_journals <- function(data, reference_df) {
  
  # Getting journal names
  
  journals_all_abbr <- # From Scimago data
    journalabbr::abbrTable |>
    dplyr::select(journal, journal_abbr) |>
    dplyr::mutate(journal_abbr = str_remove_all(journal_abbr, "\\.")) |>
    dplyr::mutate(journal_abbr = str_to_upper(journal_abbr)) |>
    dplyr::select(journal_abbr) |>
    unique()
  
  df_1_journal <-
    data |>
    dplyr::select(SR,
                  JI_main = JI,
                  PY_main = PY) |>
    dplyr::right_join(reference_df |>
                        dplyr::select(SR,
                                      JI_ref = JI,
                                      PY_ref = PY)) |>
    dplyr::mutate(JI_main = str_remove_all(JI_main, "\\.")) |>
    dplyr::inner_join(journals_all_abbr,
                      by = c("JI_ref" = "journal_abbr")) |>
    dplyr::filter(!(JI_ref == "")) |>
    dplyr::filter(!(JI_main == JI_ref))
  
  return(journal_df = df_1_journal)
  
}

get_authors <- function(data, reference_df) {
  
  # We need to extract the edgelist from scopus references
  
  # asn_scopus_1 <-
  #   data |>
  #   dplyr::filter(ref_type == "scopus") |>
  #   dplyr::mutate(ID_TOS = str_extract(SR, ".*,"))
  
  asn_scopus_ref <-
    reference_df |>
    dplyr::select(CR_ref, AU, PY) |>
    dplyr::group_by(CR_ref) |>
    dplyr::filter(n() == 1) |> # Remove duplicate references
    tidyr::separate_rows(AU, sep = ";") |>
    dplyr::group_by(CR_ref) |>
    dplyr::filter(n() != 1) |> # remove isolated authors
    dplyr::group_by(CR_ref, PY) |>
    dplyr::mutate(AU = paste0(AU,
                              collapse = ";")) |>
    unique()
  
  list_refs <- unique(asn_scopus_ref$CR_ref)
  
  edgelist_scopus_ref <- tibble()
  
  for (i in list_refs) {
    
    df_1 <-
      asn_scopus_ref |>
      dplyr::filter(CR_ref == i) |>
      tidyr::separate_rows(AU, sep = ";")
    
    df_2 <-
      t(combn(df_1$AU, m = 2)) |>
      as_tibble() |>
      dplyr::rename(from = V1,
                    to = V2)|>
      mutate(PY = df_1$PY[1])
    
    edgelist_scopus_ref <-
      edgelist_scopus_ref |>
      bind_rows(df_2)
    
    
  }
  
  edgelist_scopus_main <-
    data |>
    dplyr::filter(ref_type == "wos") |>
    dplyr::group_by(SR) |>
    dplyr::filter(n() == 1) |>
    dplyr::select(SR, AU, PY) |>
    tidyr::separate_rows(AU, sep = ";") |>
    dplyr::group_by(SR) |>
    dplyr::filter(n() != 1) |> # remove isolated authors
    dplyr::group_by(SR, PY) |>
    dplyr::mutate(AU = paste0(AU,
                              collapse = ";")) |>
    unique()
  
  list_main <- unique(edgelist_scopus_main$SR)
  
  edgelist_scopus_main_dummy <- tibble()
  
  for (i in list_main) {
    
    df_1 <-
      edgelist_scopus_main |>
      filter(SR == i) |>
      tidyr::separate_rows(AU, sep = ";")
    
    df_2 <-
      t(combn(df_1$AU, m = 2)) |>
      as_tibble() |>
      dplyr::rename(from = V1,
                    to = V2)|>
      mutate(PY = df_1$PY[1])
    
    edgelist_scopus_main_dummy <-
      edgelist_scopus_main_dummy |>
      bind_rows(df_2)
    
  }
  
  edgelist_total_scopus <-
    edgelist_scopus_main_dummy |>
    bind_rows(edgelist_scopus_ref)
  
  ## WoS
  
  ### 1. Selecting dois from refs
  
  
  DOI <-
    data |>
    dplyr::filter(ref_type == "wos") |>
    dplyr::select(CR) |>
    tidyr::separate_rows(CR, sep = ";") |>
    dplyr::filter(str_detect(CR, "DOI"))  |>
    dplyr::mutate(DOI = str_remove(CR, ".*DOI ")) |>
    distinct(DOI)
  
  
  ### 2. Getting authors from dois ref
  
  
  ##########################################################################
  #### Extracci?n de la informaci?n de los art?culos de las referencias ####
  ##########################################################################
  references <- data.frame(DI = character(),
                           PU = character(),
                           SO = character(),
                           J9 = character(),
                           PD = character(),
                           PY = character(),
                           TI = character(),
                           AF = character(),
                           stringsAsFactors = FALSE)
  authors <- data.frame(doi = character(),
                        author = character(),
                        year = character(),
                        month = character(),
                        stringsAsFactors = FALSE)
  for (i in DOI$DOI) {
    doi <- i
    url <- paste0("http://api.crossref.org/works/", doi, ".xml")
    xml_data_1 = try(xmlParse(url), silent = TRUE);
    if (class(xml_data_1) == "try-error") {
      next
    } else  {
      xml_data_2 <- xmlToList(xml_data_1)
      
      notfound =try(as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article", silent = TRUE);
      if (class(notfound) == "try-error"){
        next
      }else{
        
        if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
          
          # PUBLISHER-NAME
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])){
            PU <- as.character(NA)
          } else {
            
            publisher0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])
            publisher <- data.frame(publisher0)
            if(nrow(publisher) == 0){
              PU <- as.character(NA)
            }else{
              PU <- as.character(publisher$text[1])
            }
          }
          
          # JOURNAL FULL TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])){
            SO <- as.character(NA)
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              SO <- as.character(NA)
            }else{
              SO <- as.character(journal[1,1])
            }
          }
          
          # JOURNAL ABBREV TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])){
            J9 <- as.character(NA)
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              J9 <- as.character(NA)
            }else{
              J9 <- as.character(journal[1,1])
            }
          }
          
          # MONTH
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])){
            PD <- as.character(NA)
          } else {
            
            month0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])
            month <- data.frame(month0)
            if(nrow(month) == 0){
              PD <- as.character(NA)
            }else{
              PD <- as.character(month[1,1])
            }
          }
          
          # YEAR
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])){
            PY <- as.character(NA)
          } else {
            
            Year0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])
            Year <- data.frame(Year0)
            if(nrow(Year) == 0){
              PY <- as.character(NA)
            }else{
              PY <- as.character(Year[1,1])
            }
          }
          
          # TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])){
            TI <- as.character(NA)
          } else {
            
            title0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])
            title <- try(data.frame(title0), silent = TRUE);
            
            if(class(title) == "try-error"){
              titlex <- try(ldply(title0, data.frame), silent = TRUE);
              if(class(titlex) == "try-error"){
                TI <- as.character(NA)
              }else{
                TI0 <- as.character(titlex[1,2])
                TI <- trimws(TI0)
              }
            }else{
              if(nrow(title) == 0){
                TI <- as.character(NA)
              }else{
                TI <- as.character(title[1,1])
              }
            }
          }
          
          # CONTRIBUTORS
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
          {
            AF <- as.character(NA)
          } else {
            
            author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
            
            author_1_ref <- ldply(author_ref, data.frame)
            author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
            
            authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name), year = PY, month = PD)
            
            authors = rbind(authors, authorss)
            authorss$author <- trim(authorss$author)
            AF <- as.character(paste(authorss$author, collapse = ";   "))
          }
          
          references0 <- data.frame(DI = doi, PU = PU, SO = SO, J9 = J9, PD = PD, PY = PY, TI = TI, AF = AF)
          references = rbind(references, references0)
          
        }else {
          next}
      }
    }
  }
  authors$month <- sub("^[0]+", "", authors$month) # Elimina los ceros a la izquierda
  
  ### 3. Getting authors from main wos
  
  
  ##########################################################################
  #### Separaci?n de los nombres de los autores de los art?culos de WoS ####
  ##########################################################################
  wos_author <- # 1339 - 3
    data |>
    dplyr::filter(ref_type == "wos") |>
    dplyr::select(doi = DI, AU, PY) |>
    tidyr::separate_rows(AU, sep = ";") |>
    dplyr::rename(author = AU)
  
  
  ### 4. Merging main with refs dois with names
  
  
  #################################################################
  #### Uni?n de los datos de WoS y de las referencias de estos ####
  #################################################################
  authors$author <- gsub("(?<=, [[:alpha:]]).*", "", authors$author, perl=TRUE)
  wos_author_ref <- # 27051 3 doi author year
    authors |>
    dplyr::mutate(author = str_replace(author, ",", "")) |>
    dplyr::select(doi, author, year) |>
    dplyr::mutate(author = str_to_upper(author))
  
  
  ### 5. Getting edge list from (4)
  
  # We need to remove dois with only one author
  
  
  dois_one_author <-
    wos_author_ref |>
    dplyr::count(doi) |>
    dplyr::filter(n == 1) |>
    dplyr::select(doi)
  
  
  # We need to identify dois with high number of co-authors
  
  
  dois_high_author <-
    wos_author_ref |>
    dplyr::count(doi, sort = TRUE) |>
    dplyr::slice(1:9) |>
    dplyr::select(doi)
  
  
  # We need to merge these two dataframes
  
  
  dois_to_remove <-
    dois_one_author |>
    dplyr::bind_rows(dois_high_author)
  
  
  # We need to remove the dois from wos_author_ref
  
  wos_author_removed <-
    wos_author_ref |>
    dplyr::filter(!(doi %in% dois_to_remove$doi))
  
  
  
  ###########################
  #### Listas de enlaces ####
  ###########################
  # # Para art?culos que tienen como m?nimo dos autores
  # author_1 <- # 4273 3 there a mistake with the duplicate values.
  #   wos_author |> # 190
  #   filter(!is.na(PY)) |>
  #   dplyr::rename(year = PY,
  #                 doi = DI) |>
  #   bind_rows(wos_author_ref |>
  #               mutate(year = as.numeric(year))) |> # 4104
  #   unique() |>
  #   filter(!(doi %in% "10.3109/13880209.2014.922589" &
  #          year == 2014)) # this is a duplicate value with != year
  edgelist_wos_authors <- data.frame(Source = character(),
                                     Target = character(),
                                     doi = character(),
                                     year = as.numeric(),
                                     stringsAsFactors = FALSE)
  # table_ids <- table(author_1$doi)
  # table_ids_0 <- data.frame(table_ids)
  # table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
  list_ids_1 <- unique(wos_author_removed$doi)
  for (i in list_ids_1) {
    df_1 = wos_author_removed[wos_author_removed$doi == i,] |>
      filter(!is.na(doi))
    df_2 = combn(df_1$author, 2, simplify = FALSE)
    df_3 = data.frame((t(data.frame(df_2))), i)
    colnames(df_3) = c("Source", "Target", "doi")
    df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
    edgelist_wos_authors = rbind(edgelist_wos_authors, df_4)
  }
  
  edgelist_scopus_wos <-
    edgelist_total_scopus |>
    dplyr::bind_rows(edgelist_wos_authors |>
                       dplyr::select(from = Source,
                                     to = Target,
                                     PY = year) |>
                       dplyr::mutate(PY = as.numeric(PY))) |>
    dplyr::filter(!str_detect(from, pattern = "\\* | \\. | [0-9]"),
                  !str_detect(to, pattern = "\\* | \\. | [0-9]"),
                  !str_detect(to, pattern = "\\*"),
                  !str_detect(to, pattern = "[0-9]"),
                  !str_detect(from, pattern = "\\*"),
                  !str_detect(from, pattern = "[0-9]"))
  
  return(edgelist_scopus_wos = edgelist_scopus_wos)
  
  
}
