# Function to summarize baseline opioid use in gorups 
make_baseline_opioid_table <- function(data,
                                       cat.vars,
                                       cont.vars,
                                       omeCat = "omeCat",
                                       ome.var = "omeBaseTotal",
                                       group.var = "pain") {
  data.prep <- data %>%
    select(all_of(group.var), all_of(cat.vars), all_of(cont.vars), all_of(omeCat), all_of(ome.var))
  
  # Categorical variables: n (%)
  cat.summary <- data.prep %>%
    select(all_of(group.var), all_of(cat.vars)) %>%
    pivot_longer(-all_of(group.var), names_to = "variable", values_to = "value") %>%
    group_by(across(all_of(group.var)), variable) %>%
    summarise(n_group = n(), n_pos = sum(value == 1, na.rm = TRUE), pct = round(100 * n_pos / n_group, 1), .groups = "drop") %>%
    mutate(stat = paste0(n_pos, " (", pct, "%)")) %>%
    select(all_of(group.var), variable, stat)
  
  # Continuous variables: median (range) among recipients
  # cont.summary <- data.prep %>%
  #   select(all_of(group.var), all_of(cont.vars)) %>%
  #   pivot_longer(-all_of(group.var), names_to = "variable", values_to = "value") %>%
  #   filter(value != 0) %>%
  #   group_by(across(all_of(group.var)), variable) %>%
  #   summarise(median_val = round(median(value, na.rm = TRUE), 1), range = paste0(round(min(value, na.rm = TRUE), 1), "-", round(max(value, na.rm = TRUE), 1)), .groups = "drop") %>%
  #   mutate(stat = paste0(median_val, " (", range, ")")) %>%
  #   select(all_of(group.var), variable, stat)
  cont.summary <- data.prep %>% # Report IQR among recipients
    select(all_of(group.var), all_of(cont.vars)) %>%
    pivot_longer(-all_of(group.var), names_to = "variable", values_to = "value") %>%
    filter(value != 0) %>%
    group_by(across(all_of(group.var)), variable) %>%
    summarise(
      median_val = round(median(value, na.rm = TRUE), 1),
      iqr = paste0(round(quantile(value, 0.25, na.rm = TRUE), 1),"-",round(quantile(value, 0.75, na.rm = TRUE), 1)),.groups = "drop") %>%
    mutate(stat = paste0(median_val, " (", iqr, ")")) %>%
    select(all_of(group.var), variable, stat)
  
  # Get OME values corresponding to each omeCat level
  ome.ranges <- data.prep %>%
    filter(.data[[omeCat]] != 0, !is.na(.data[[omeCat]])) %>%
    group_by(.data[[omeCat]]) %>%
    summarise(min_ome = round(min(.data[[ome.var]], na.rm = TRUE), 2), max_ome = round(max(.data[[ome.var]], na.rm = TRUE), 2), .groups = "drop") %>%
    arrange(.data[[omeCat]]) %>%
    mutate(variable = paste0("Q", .data[[omeCat]], " (", min_ome, "-", max_ome, ")"))
  
  # Add the zero category
  ome.labels <- tibble(omeCat_value = 0, variable = "No opioid") %>%
    bind_rows(ome.ranges %>% transmute(omeCat_value = .data[[omeCat]], variable = variable))
  
  # Count patients in each omeCat category
  ome.summary <- data.prep %>%
    mutate(omeCat_value = .data[[omeCat]]) %>%
    filter(!is.na(omeCat_value)) %>%
    group_by(across(all_of(group.var)), omeCat_value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(across(all_of(group.var))) %>%
    mutate(n_group = sum(n), pct = round(100 * n / n_group, 1), stat = paste0(n, " (", pct, "%)")) %>%
    ungroup() %>%
    left_join(ome.labels, by = "omeCat_value") %>%
    select(all_of(group.var), variable, stat)
  
  final <- bind_rows(cat.summary, cont.summary, ome.summary) %>%
    pivot_wider(names_from = all_of(group.var), values_from = stat, names_prefix = paste0(group.var, "_"))
  
  # Get total counts for each group
  group.total <- data.prep %>%
    count(across(all_of(group.var)), name = "n_total") 
  
  total.row <- group.total %>%
    mutate( variable = "N", stat = as.character(n_total)) %>%
    select(-n_total) %>%
    pivot_wider( names_from = all_of(group.var), values_from = stat, names_prefix = paste0(group.var, "_"))
  
  # Add total row to top
  final <- bind_rows(total.row, final)
  
  return(final)
}