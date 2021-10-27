#https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names
df %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)
