format_number <- function(x, k = 3, m = 0) {
  ifelse(x == 0, 
         "0.0", 
         ifelse(x >= 1000, 
                formatC(round(x, m), format = "f", big.mark = ",", digits = m),
                formatC(round(x, k), format = "f", big.mark = ",", digits = k)))
}


format_cost <- function(x) {
  ifelse(is.na(x), "", sprintf("$%s", formatC(x, format = "f", digits = 0, big.mark=",")))
}

create_cea_table <- function(cost, effect, strategies, return_data = FALSE) {
  tbl_data <-  1:length(res_cost) %>% map( ~ ({
    dampack::calculate_icers(cost = cost[[.x]],
                             effect = effect[[.x]],
                             strategies = strategies[[.x]])
  })) %>%
    set_names(names(res_cost)) %>%
    bind_rows(.id = "method") %>%
    mutate(
      Status = case_when(
        Status == "ND" ~ "",
        Status == "D" ~ "Dominated",
        Status == "ED" ~ "Dominatd (Extended)",
        TRUE ~ Status
      )
    )
  
  if (return_data) return(tbl_data) 
  
  ft <- flextable(tbl_data)
  
  ft <- merge_v(ft, j = "method")
  border_style <- officer::fp_border(color = "black",
                                     style = "solid",
                                     width = 0.5)
  ft <- border(ft, part = "body", border = border_style)
  ft %>%
    set_header_labels(
      Strategy = "Strategy",
      Cost = "Cost",
      Effect = "QALY",
      Inc_Cost = "Incremental Cost",
      Inc_Effect = "Incremental QALYs",
      ICER = "ICER",
      Status = "Status",
      method = ""
    ) %>%
    colformat_double(j = c("Effect", "Inc_Effect"), digits = 3)  %>%
    compose(j = "Cost", value = as_paragraph(as_chunk(format_cost(Cost)))) %>%
    compose(j = "Inc_Cost", value = as_paragraph(as_chunk(format_cost(Inc_Cost)))) %>%
    compose(j = "ICER", value = as_paragraph(as_chunk(format_cost(ICER))))
  
}
