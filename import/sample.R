library(tidyverse)
library(lubridate) # charger avant here pour éviter conflit
library(stringr)
library(here)

## Données téléchargées avec
# https://api.crunchbase.com/bulk/v4/bulk_export.tar.gz?user_key=ad5a2e7b805d09b373251ab2fb2385e3

# Idée: on crée une base de donnée à jour au 31/12/2019
date_lim <- ymd("20191231")


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

o <- rename(o, org_uuid = "uuid")

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

fnd <- rename(fnd, funding_round_uuid = "uuid")

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
org <- filter(org, org_uuid %in% unique(fnd$org_uuid))

###### Restreindre base des financements ######
## À ceux fait dans des organisations de la base
## ~39k fnd.
fnd <- filter(fnd, org_uuid %in% org$org_uuid)

## À ceux fait avant le 1er janvier 2020
fnd <- filter(fnd, announced_on <= date_lim)

###### Enrichir base des investissements ######

## Il manque dans la base fnd la liste complète des investisseurs

iv <- read_csv(here("data", "investments.csv")) %>% 
  select(-rank) %>% 
  rename(investment_uuid = "uuid") %>% 
  filter(funding_round_uuid %in% fnd$funding_round_uuid) %>% 
  mutate(is_lead_investor = ifelse(is.na(is_lead_investor), FALSE, is_lead_investor))

## Note de Fabien: il y a quelques investisseurs qui apparaissent en double sur certain round...
## Pourquoi? En fait, pas le même investment_uuid... Décalage de peut-être 30 lignes.
## Donc, on laisse tomber ces lignes dupliquées. Quand is_lead_investor, on laisse tomber l'autre.
iv <- group_by(iv, funding_round_uuid, investor_uuid) %>% 
  mutate(n = n(), 
         c = ifelse(n > 1, 
                    ifelse(is_lead_investor,
                           1,
                           0), 
                    1)) %>% 
  ungroup() %>% 
  filter(c > 0.5) %>% 
  select(-n) %>% 
  distinct(funding_round_uuid, investor_uuid, .keep_all = TRUE)

## On commence par examiner les lead investors décrits dans les deux bases

fnd_li <- separate_rows(fnd, lead_investor_uuids, sep = ",") %>%
  select(funding_round_uuid, lead_investor_uuids) %>%
  filter(!is.na(lead_investor_uuids)) %>%
  arrange(funding_round_uuid, lead_investor_uuids)

iv_li <- filter(iv, is_lead_investor) %>%
  select(funding_round_uuid, investor_uuid, investment_uuid) %>%
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

full_li <- full_join(fnd_li, iv_li, by = c(funding_round_uuid = "funding_round_uuid", lead_investor_uuids = "investor_uuid"))
fnd <- group_by(full_li, funding_round_uuid) %>% 
  summarise(lead_investor_uuids_neat = paste(lead_investor_uuids, collapse = ","),
            lead_investment_uuids_neat = paste(investment_uuid, collapse = ",")) %>% 
  right_join(fnd)

rm(iv_li, fnd_li, full_li)

## Ensuite, on récupère la liste complète des investisseurs depuis la base iv

fnd <- group_by(iv, funding_round_uuid) %>% 
  summarise(full_investment_uuids = paste(investment_uuid, collapse = ","),
            full_investor_uuids = paste(investor_uuid, collapse = ",")) %>% 
  right_join(fnd, by = c("funding_round_uuid"))

## Enfin, on récupère la liste des investisseurs non-lead

fnd <- filter(iv, !is_lead_investor) %>% 
  group_by(funding_round_uuid) %>% 
  summarise(nonlead_investment_uuids = paste(investment_uuid, collapse = ","),
            nonlead_investor_uuids = paste(investor_uuid, collapse = ",")) %>% 
  right_join(fnd, by = "funding_round_uuid")

###### Créer base des acquisitions ######

acq <- read_csv(here("data", "acquisitions.csv"))

acq <- rename(acq, acquisition_uuid = "uuid")

## Filter par date
acq <- filter(acq, acquired_on <= date_lim)

## Pour les acquisitions, on veut deux choses:
## Les entreprises de la base org qui ont été acquises par une autre, ou qu'elles soient
## Les entreprises acquises par des entreprises de la base org, où qu'elles soient

acq <- filter(acq, acquiree_uuid %in% org$org_uuid | acquirer_uuid %in% org$org_uuid) 

acq <- left_join(acq, o, by = c(acquiree_uuid = "org_uuid"))

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

inv <- rename(inv, investor_uuid = "uuid")

x <- separate_rows(fnd, full_investor_uuids, sep = ",") %>% 
  distinct(full_investor_uuids) %>% 
  filter(!is.na(full_investor_uuids))

inv <- filter(inv, investor_uuid %in% x$full_investor_uuids)

inv_acq <- filter(o, org_uuid %in% acq$acquirer_uuid)

###### Créer base des ipos ######

ipo <- read_csv(here("data", "ipos.csv")) %>% 
  filter(org_uuid %in% org$org_uuid)

ipo <- rename(ipo, ipo_uuid = "uuid")
## 503 ipo répértoriés pour 486 entreprises
## Problème: il y en a plus qu'il n'y a d'ipo répertoriés
## dans la base org... Parce que ont pu faire l'objet d'acquisition
## ou sont marquées comme closed?
## À investiguer

# mutate(org, ipo = ifelse(uuid %in% ipo$org_uuid, "yes", "no")) %>% 
#  tabyl(ipo, status)

###### Corriger les montants de l'inflation ######

# données sur l'inflation récupéré sur l'OCDE:
# https://data.oecd.org/fr/price/inflation-ipc.htm

# On importe seulement les données pour les USA, annuelles,
# en index 100 en 2015

inf <- read_csv(here("data", "inflation_OECD.csv")) %>% 
  select(-`Flag Codes`) %>% 
  filter(LOCATION == "USA",
         SUBJECT == "TOT",
         FREQUENCY == "A",
         MEASURE == "IDX2015") %>% 
  mutate(Value = Value / 100)


# Recodage: on transforme les variables décrivant des sommes en variables
# corrigées de l'inflation

fnd <- mutate(fnd, TIME = year(announced_on)) %>% 
  left_join(select(inf, TIME, Value)) %>% 
  mutate(raised_amount_usd_corr        = raised_amount_usd / Value,
         post_money_valuation_usd_corr = post_money_valuation_usd / Value) %>% 
  select(-TIME, -Value)

acq <- mutate(acq, TIME = year(acquired_on)) %>% 
  left_join(select(inf, TIME, Value)) %>% 
  mutate(price_usd_corr        = price_usd / Value) %>% 
  select(-TIME, -Value)

ipo <- mutate(ipo, TIME = year(went_public_on)) %>% 
  left_join(select(inf, TIME, Value)) %>% 
  mutate(share_price_usd_corr  = share_price_usd / Value,
         valuation_price_usd_corr = valuation_price_usd / Value,
         money_raised_usd_corr = money_raised_usd / Value) %>% 
  select(-TIME, -Value)


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

