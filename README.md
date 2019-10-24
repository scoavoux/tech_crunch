# Fabriquer la base de données

## Prérequis

Installer R et les packages R `tidyverse` et `here` (`install.packages(tidyverse, here)`).

Copier les données originelles (issues de `daily_export_2018_10_25`) dans le dossier `data`:

+ `funding_rounds.csv`
+ `organizations.csv`

## Fonctionnement

`make` produit dans le répertoire `data_sampled` trois fichiers csv, sans saut de ligne dans les observations, avec le signe `#` en séparateur:

+ `data_sampled_org.csv`: base des entreprises
+ `data_sampled_inv.csv`: base des investisseurs
+ `data_sampled_fnd.csv`: base des levées de fonds
