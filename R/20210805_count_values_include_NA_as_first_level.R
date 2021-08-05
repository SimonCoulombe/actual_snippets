zzz <- iris %>% 
  mutate(
    rounded = round(Sepal.Length), 
    added_missing = if_else(rounded==4, NA_real_, rounded),
  factored = factor(added_missing),
  factored_explicit = forcats::fct_explicit_na(factored, na_level = "(Missing)"),
  factored_explicit_releved = forcats::fct_relevel(factored_explicit, "(Missing)")
         )

zzz %>% 
  ggplot()+
  geom_bar(mapping = aes(x = factored_explicit_releved, y = stat(prop), group = 1))

