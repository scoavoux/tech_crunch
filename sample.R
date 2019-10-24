library(tidyverse)
library(lubridate) # charger avant here pour éviter conflit
library(stringr)
library(here)

###### Import des bases de données ######

# Importer la base organisations, en supprimant les variables inutiles
o <- read_csv(here("data", "organizations.csv"),
                col_types = cols_only( # on essaye de limiter l'import en supprimant les colonnes inutiles
                  company_name = col_character(),
                  roles = col_character(),
                  # permalink = col_character(),
                  # domain = col_character(),
                  # homepage_url = col_character(),
                  country_code = col_character(),
                  state_code = col_character(),
                  region = col_character(),
                  city = col_character(),
                  address = col_character(),
                  status = col_character(),
                  short_description = col_character(),
                  category_list = col_character(),
                  category_group_list = col_character(),
                  funding_rounds = col_integer(),
                  funding_total_usd = col_double(),  # important
                  founded_on = col_date(format = ""),
                  last_funding_on = col_date(format = ""),
                  closed_on = col_date(format = ""),
                  employee_count = col_character(),
                  # email = col_character(),
                  # phone = col_character(),
                  # facebook_url = col_character(),
                  # linkedin_url = col_character(),
                  # cb_url = col_character(),
                  # logo_url = col_character(),
                  # twitter_url = col_character(),
                  aliases = col_character(),
                  uuid = col_character(),
                  created_at = col_datetime(format = ""),
                  updated_at = col_datetime(format = ""),
                  primary_role = col_character(),
                  type = col_character()
                ))

# Importer la base des financements
fnd <- read_csv(here("data", "funding_rounds.csv"),
                col_types = cols(
                  company_name = col_character(),
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
                  post_money_currency_code = col_character(),
                  investor_count = col_integer(),
                  cb_url = col_character(),
                  company_uuid = col_character(),
                  funding_round_uuid = col_character(),
                  created_at = col_datetime(format = ""),
                  updated_at = col_datetime(format = ""),
                  investor_names = col_character(),
                  investor_uuids = col_character()
                ))

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
org <- filter(o, region == "SF Bay Area" | 
                (is.na(region) & city %in% BA_cities$city[is.na(BA_cities$Homonyme)]) |
                (is.na(region) & city %in% BA_cities$city[!is.na(BA_cities$Homonyme)] & state_code == "CA")
              )

## Supprimer les organisations qui ne sont pas de type company.
org <- filter(org, roles %in% c("company", "company,investor",
                                "investor,company", "school,company", "school,company,investor"))

## Supprimer les entreprises qui n'ont pas de rounds d'investissement renseigné
org <- filter(org, uuid %in% unique(fnd$company_uuid))

###### Restreindre base des financements ######
fnd <- filter(fnd, company_uuid %in% org$uuid)

###### Créer base des investisseurs ######

# À partir de la base o: récupérer les entreprises mentionnées 
# comme pourvoyeuses de fonds dans la base fundings.
unique_inv <- mutate(fnd, investor_uuids = str_remove_all(investor_uuids, "[\\{\\}]")) %>% 
  separate_rows(investor_uuids, sep = ",") %>% 
  distinct(investor_uuids) %>% 
  filter(investor_uuids != "", !is.na(investor_uuids))

inv <- filter(o, uuid %in% unique_inv$investor_uuids)

###### Netoyer les bases ######

## Supprimer tous les sauts de ligne ainsi que les dièses

clean_char_var <- function(var){
  str_replace_all(var, "[\r\n#]", " ")
}

org <- mutate_if(org, is.character, clean_char_var)
inv <- mutate_if(inv, is.character, clean_char_var)
fnd <- mutate_if(fnd, is.character, clean_char_var)

#write_csv(org, "org_bay_area_funded.csv")
###### Sauvegarder ######

if(!dir.exists(here("data_sampled"))) dir.create(here("data_sampled"))

# Au format .RData (Samuel)
save(org, inv, fnd, file = here("data_sampled", "sampled_data.RData"))

# En csv avec des # comme délimiteurs (Fabien)
write_delim(org, path = here("data_sampled", "data_sampled_org.csv"), delim = "#")
write_delim(inv, path = here("data_sampled", "data_sampled_inv.csv"), delim = "#")
write_delim(fnd, path = here("data_sampled", "data_sampled_fnd.csv"), delim = "#")
