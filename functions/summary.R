quant_summary <- function(.data, var){
  summarise(.data, 
            N = n(),
            nb_manquant = sum(is.na(!! sym(var))),
            #d1 = quantile(!! sym(var), .1, na.rm = TRUE),
            q1 = quantile(!! sym(var), .25, na.rm = TRUE),
            mean = mean(!! sym(var), na.rm = TRUE),
            sd = sd(!! sym(var), na.rm = TRUE),
            median = median(!! sym(var), na.rm = TRUE),
            q3 = quantile(!! sym(var), .75, na.rm = TRUE),
            d9 = quantile(!! sym(var), .9, na.rm = TRUE),
            #c99 = quantile(!! sym(var), .99, na.rm = TRUE),
  )
}