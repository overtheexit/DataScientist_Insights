sort_function = function(data) {
  data |>
    filter(Rating != -1) |>
    arrange(desc(avgsalary))
}