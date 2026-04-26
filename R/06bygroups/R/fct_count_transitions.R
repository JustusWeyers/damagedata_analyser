count_transitions = function(c1) {
  
  # Daten filtern und nach id + Jahr sortieren
  df = data |>
    dplyr::filter(class1 == c1) |>
    dplyr::arrange(id, prufjahr)
  
  # Alle möglichen Zustände in dieser class1
  all_states = sort(unique(df$bsp_id))
  
  # Liste der Zustandsfolgen pro id
  state_list = df |>
    dplyr::group_by(id) |>
    dplyr::summarise(states = list(bsp_id)) |>
    dplyr::pull(states)
  
  # Übergänge zählen
  transitions = do.call(rbind, lapply(state_list, function(states) {
    if(length(states) < 2) return(NULL)  
    cbind(head(states, -1), tail(states, -1))
  }))
  
  if(is.null(transitions)) {
    # Leere quadratische Matrix
    return(matrix(0, nrow = 1, ncol = 1, dimnames = list("-", "-")))
  }
  
  # Tabelle erstellen
  tab = table(transitions[,1], transitions[,2])
  
  # Tabelle auf alle Zustände erweitern (quadratisch)
  tab_full = matrix(0, nrow = length(all_states), ncol = length(all_states),
                    dimnames = list(all_states, all_states))
  tab_full[rownames(tab), colnames(tab)] = tab
  
  return(tab_full)
}
