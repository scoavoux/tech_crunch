
###### Funding rounds ######
fnd <- fnd %>% 
  # fix data type
  mutate(announced_on = ymd(announced_on),
  # fix 
         investor_uuids = str_remove_all(investor_uuids, "[\\{\\}]"))
