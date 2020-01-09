library(tidyverse)
library(lubridate) # charger avant here pour éviter conflit
library(stringr)
library(here)

###### Import des bases de données ######

# Importer la base organisations, en supprimant les variables inutiles
o <- read_csv(here("data", "organizations.csv"),
                col_types = cols_only( # on essaye de limiter l'import en supprimant les colonnes inutiles
                  uuid = col_character(),
                  name = col_character(),
                  type = col_character(),
                  permalink = col_character(),
                  # cb_url = col_character(),
                  rank = col_double(),
                  created_at = col_datetime(format = ""),
                  updated_at = col_datetime(format = ""),
                  # legal_name = col_character(),
                  roles = col_character(),
                  # domain = col_character(),
                  homepage_url = col_character(),
                  country_code = col_character(),
                  state_code = col_character(),
                  region = col_character(),
                  city = col_character(),
                  address = col_character(),
                  postal_code = col_character(),
                  status = col_character(),
                  short_description = col_character(),
                  category_list = col_character(),
                  category_groups_list = col_character(),
                  num_funding_rounds = col_double(),
                  total_funding_usd = col_double(),
                  total_funding = col_double(),
                  total_funding_currency_code = col_character(),
                  founded_on = col_date(format = ""),
                  last_funding_on = col_date(format = ""),
                  closed_on = col_date(format = ""),
                  employee_count = col_character(),
                  # email = col_character(),
                  # phone = col_character(),
                  # facebook_url = col_character(),
                  # linkedin_url = col_character(),
                  # twitter_url = col_character(),
                  # logo_url = col_character(),
                  # alias1 = col_character(),
                  # alias2 = col_character(),
                  # alias3 = col_character(),
                  primary_role = col_character(),
                  num_exits = col_double()
                  )
              )

# Importer la base des financements
fnd <- read_csv(here("data", "funding_rounds.csv"),
                col_types = cols_only(
                  uuid = col_character(),
                  name = col_character(),
                  type = col_character(),
                  # permalink = col_character(),
                  # cb_url = col_character(),
                  rank = col_double(),
                  created_at = col_datetime(format = ""),
                  updated_at = col_datetime(format = ""),
                  country_code = col_character(),
                  state_code = col_character(),
                  region = col_character(),
                  city = col_character(),
                  investment_type = col_character(),
                  announced_on = col_date(format = ""),
                  raised_amount_usd = col_double(),
                  raised_amount = col_double(),
                  raised_amount_currency_code = col_character(),
                  post_money_valuation_usd = col_double(),
                  post_money_valuation = col_double(),
                  post_money_valuation_currency_code = col_character(),
                  investor_count = col_double(),
                  org_uuid = col_character(),
                  org_name = col_character(),
                  lead_investor_uuids = col_character()
                )
                )

###### Restriction base des entreprises ######

# On restreint la base aux entreprises de la Silicon Valley.
# On se fonde sur la variable région et la modalité "SF Bay Area".
# Cependant, certaines entreprises ont une valeur manquante
# pour cette variable. On fait donc la liste des villes situées
# dans la Bay Area à partir des valeurs unique de la variable city,
# on les code à la main pour définir les limites de la BA (fait par Olivier)
# Enfin, on rattrape les entreprises lorsque la région est manquante
# mais que la ville fait partie de la BA (avec précautions pour les homonymes)

## Ce bout de code pour extraire les villes uniques
## des entreprises dont région est BA.

# freq(o$region, sort = "dec") %>% head(n = 100)
# BA_cities <- filter(o, region == "SF Bay Area") %>%
#   select(city) %>% 
#   distinct() %>% 
#   arrange(city)
# write_csv(BA_cities, path = "BA_cities.csv")
# Remove obvious errors
# excluded_cities <- c("Pittsburg", "Dublin", "")
# BA_cities <- BA_cities[!(BA_cities$city %in% excluded_cities), ]
## Prendre en compte state_code pour vérifier qu'on prend les Dublin, Pittsburg, 
## Etc. seulement s'ils sont en Californie

BA_cities <- read_csv(here("data", "BA_citiesOA.csv")) %>% 
  filter(SiliconValley == "o")

## Réduire aux entreprises qui sont dans la Bay Area
## ~40k org.
org <- filter(o, region == "California", city %in% BA_cities$city)

## Supprimer les organisations qui ne sont pas de type company.
## ~37k org.
org <- filter(org, str_detect(roles, "company"))

## Supprimer les entreprises qui n'ont pas de rounds d'investissement renseigné
## ~16k org.
org <- filter(org, uuid %in% unique(fnd$org_uuid))

###### Restreindre base des financements ######
## ~39k fnd.
fnd <- filter(fnd, org_uuid %in% org$uuid)

###### Enrichir base des investissements ######

## Il manque dans la base fnd la liste complète des investisseurs

iv <- read_csv(here("data", "investments.csv")) %>% 
  select(-rank) %>% 
  filter(funding_round_uuid %in% fnd$uuid) %>% 
  mutate(is_lead_investor = ifelse(is.na(is_lead_investor), FALSE, is_lead_investor))

## On commence par examiner les lead investors décrits dans les deux bases

fnd_li <- separate_rows(fnd, lead_investor_uuids, sep = ",") %>%
  select(uuid, lead_investor_uuids) %>%
  filter(!is.na(lead_investor_uuids)) %>%
  arrange(uuid, lead_investor_uuids)

iv_li <- filter(iv, is_lead_investor) %>%
  select(funding_round_uuid, investor_uuid, uuid) %>%
  rename(investment_uuid = "uuid") %>% 
  arrange(funding_round_uuid, investor_uuid)

## Dans quelques situations rares, les lead_investor sont renseignés dans la
## base investissement mais pas dans la base funding_rounds
## (deux cas sur la base du 18/12/2019)
# filter(iv_li, !(funding_round_uuid %in% fnd_li$uuid))

# Il y a également un cas où il y a trois lead investors mentionnés dans la base fnd mais 
# seulement deux dans la base investments
# 
# anti_join(fnd_li, iv_li, by = c(uuid = "funding_round_uuid", lead_investor_uuids = "investor_uuid"))
#
# Inversement, 5 cas où il y a un investisseurs principale renseigné dans iv mais pas dans fnd
#
# anti_join(iv_li, fnd_li, by = c("funding_round_uuid" = "uuid", "investor_uuid" = "lead_investor_uuids"))
# 
# si l'on veut le plus complet des deux, on peux
# Du coup, on fusionne les deux bases et on repart de là pour recréer une variable lead_investor_uuids complète

full_li <- full_join(fnd_li, iv_li, by = c(uuid = "funding_round_uuid", lead_investor_uuids = "investor_uuid"))
fnd <- group_by(full_li, uuid) %>% 
  summarise(lead_investor_uuids_neat = paste(lead_investor_uuids, collapse = ","),
            lead_investment_uuids_neat = paste(lead_investor_uuids, collapse = ",")) %>% 
  right_join(fnd)

rm(iv_li, fnd_li, full_li)

## Ensuite, on récupère la liste complète des investisseurs depuis la base iv

fnd <- group_by(iv, funding_round_uuid) %>% 
  summarise(full_investment_uuids = paste(uuid, collapse = ","),
            full_investor_uuids = paste(investor_uuid, collapse = ",")) %>% 
  right_join(fnd, by = c(funding_round_uuid = "uuid")) %>% 
  rename(uuid = funding_round_uuid)

## Enfin, on récupère la liste des investisseurs non-lead

fnd <- filter(iv, !is_lead_investor) %>% 
  group_by(funding_round_uuid) %>% 
  summarise(nonlead_investment_uuids = paste(uuid, collapse = ","),
            nonlead_investor_uuids = paste(investor_uuid, collapse = ",")) %>% 
  right_join(fnd, by = c(funding_round_uuid = "uuid")) %>% 
  rename(uuid = funding_round_uuid)


###### Créer base des acquisitions ######

acq <- read_csv(here("data", "acquisitions.csv")) %>% 
  filter(acquiree_uuid %in% org$uuid)

## 3553 acquisitions répértoriées de 3417 entreprises
## Là, encore, plus que de compagnies marques comme acquired dans la base
## org, avec le reste en closed.
# mutate(org, acquired = ifelse(uuid %in% acq$acquiree_uuid, "yes", "no")) %>% 
#    tabyl(acquired, status)
# Seulement 1231 entreprises acquisitrices sont dans la base des organisations
# sum(acq$acquirer_uuid %in% org$uuid)
# sum(acq$acquirer_uuid %in% inv$uuid)
# 
# 2110 entreprises acquisitrices répertoriées
# length(unique(acq$acquirer_uuid))

###### Créer base des investisseurs ######

inv <- read_csv(here("data", "investors.csv"),
                col_types = cols_only(
                  uuid = col_character(),
                  name = col_character(),
                  type = col_character(),
                  permalink = col_character(),
                  # cb_url = col_character(),
                  rank = col_double(),
                  created_at = col_datetime(format = ""),
                  updated_at = col_datetime(format = ""),
                  roles = col_character(),
                  # domain = col_character(),
                  country_code = col_character(),
                  state_code = col_character(),
                  region = col_character(),
                  city = col_character(),
                  investor_types = col_character(),
                  investment_count = col_double(),
                  total_funding_usd = col_double(),
                  total_funding = col_double(),
                  total_funding_currency_code = col_character(),
                  founded_on = col_date(format = ""),
                  closed_on = col_date(format = "")
                # facebook_url = col_character(),
                # linkedin_url = col_character(),
                # twitter_url = col_character(),
                # logo_url = col_character()
                ))

x <- separate_rows(fnd, full_investor_uuids, sep = ",") %>% 
  distinct(full_investor_uuids) %>% 
  filter(!is.na(full_investor_uuids))

inv <- filter(inv, uuid %in% x$full_investor_uuids)

inv_acq <- filter(o, uuid %in% acq$acquirer_uuid)

###### Créer base des ipos ######

ipo <- read_csv(here("data", "ipos.csv")) %>% 
  filter(org_uuid %in% org$uuid)

## 503 ipo répértoriés pour 486 entreprises
## Problème: il y en a plus qu'il n'y a d'ipo répertoriés
## dans la base org... Parce que ont pu faire l'objet d'acquisition
## ou sont marquées comme closed?
## À investiguer

# mutate(org, ipo = ifelse(uuid %in% ipo$org_uuid, "yes", "no")) %>% 
#  tabyl(ipo, status)

###### Netoyer les bases ######

## Supprimer tous les sauts de ligne ainsi que les dièses

clean_char_var <- function(var){
  str_replace_all(var, "[\r\n#]", " ")
}

org <- mutate_if(org, is.character, clean_char_var)
inv <- mutate_if(inv, is.character, clean_char_var)
inv_acq <- mutate_if(inv_acq, is.character, clean_char_var)
fnd <- mutate_if(fnd, is.character, clean_char_var)
ipo <- mutate_if(ipo, is.character, clean_char_var)
acq <- mutate_if(acq, is.character, clean_char_var)

#write_csv(org, "org_bay_area_funded.csv")
###### Sauvegarder ######

if(!dir.exists(here("data_sampled"))) dir.create(here("data_sampled"))

# Au format .RData (Samuel)
save(org,
     inv, 
     inv_acq,
     fnd, 
     ipo,
     acq,
     file = here("data_sampled", "sampled_data.RData"))

# En csv avec des # comme délimiteurs (Fabien)
write_delim(org, path = here("data_sampled", "data_sampled_org.csv"), delim = "#")
write_delim(inv, path = here("data_sampled", "data_sampled_inv.csv"), delim = "#")
write_delim(inv_acq, path = here("data_sampled", "data_sampled_inv_acq.csv"), delim = "#")
write_delim(fnd, path = here("data_sampled", "data_sampled_fnd.csv"), delim = "#")
write_delim(ipo, path = here("data_sampled", "data_sampled_ipo.csv"), delim = "#")
write_delim(acq, path = here("data_sampled", "data_sampled_acq.csv"), delim = "#")

