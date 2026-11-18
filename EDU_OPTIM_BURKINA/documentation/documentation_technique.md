# Documentation technique

## Architecture

```
EDU_OPTIM_BURKINA/
├── global.R              chargé une fois : packages, fonctions, modules, données préparées
├── ui.R                  structure de l'interface (barre latérale, en-tête, 14 pages)
├── server.R              session : connexion, périmètre, filtres globaux, navigation
├── lancer_application.R  lancement depuis RStudio
├── LANCER_EDU_OPTIM.bat  lancement par double-clic (Windows)
├── DESCRIPTION           dépendances (utilisé par renv)
├── config/
│   ├── config.yml        source des données (synthétique / réelle), chemins, options
│   └── utilisateurs.csv  comptes (empreintes SHA-256 salées)
├── R/                    fonctions générales (aucun code Shiny réactif)
│   ├── 00_config.R            constantes, palette, normes et coûts par défaut
│   ├── 01_utils.R             formats français, division protégée, Gini, journal
│   ├── 10_chargement.R        lecture du contrat de données EXX_01
│   ├── 11_controle_qualite.R  contrôles de cohérence, doublons, manquants
│   ├── 12_preparation.R       chaîne complète + cache + restriction par périmètre
│   ├── 20_indicateurs.R       ratios, déficits, agrégations
│   ├── 21_imde.R              indice IMDE, sensibilité, robustesse
│   ├── 22_econometrie.R       panel, tests, bootstrap
│   ├── 23_spatial.R           Moran, LISA, accessibilité, densité
│   ├── 24_previsions.R        6 méthodes, backtesting, besoins
│   ├── 25_priorisation.R      TOPSIS
│   ├── 26_optimisation.R      programme linéaire d'allocation
│   ├── 27_simulations.R       scénarios, redéploiement (transport)
│   ├── 28_rapports.R          PDF (grid), Excel, CSV, graphiques
│   ├── 29_authentification.R  comptes, profils, périmètres, écran de connexion
│   └── 30_ui_composants.R     cartes KPI, tableaux, thème des graphiques
├── modules/<nom>/mod_<nom>.R  un module Shiny par page (ui + server)
├── scripts/              00 installer, 01 générer, 02 contrôler, 03 préparer, 04 renv, run_all
├── data/
│   ├── modele/           classeur EXX_01 vide (structure de référence)
│   ├── synthetique/      base EXX_01 synthétique, établissements, contexte, géométries
│   ├── reelles/          vos données réelles
│   └── prepare/          caches .rds (supprimables sans risque)
├── www/                  css/edu-optim.css, js/edu-optim.js, images/
├── resultats/            rapports et exports produits par l'application
├── documentation/        ces documents (affichés dans le module Documentation)
└── tests/testthat/       tests unitaires
```

## Flux de données

```
config.yml ──► charger_donnees() ──► controler_panel() ──► nettoyer_etablissements()
                  (EXX_01)              (contrôles)            (doublons)
          ──► preparer_indicateurs() ──► calculer_imde() par cycle ──► cache data/prepare/
          ──► restreindre(périmètre utilisateur) ──► modules
```

- `global.R` appelle `preparer_tout()`. Le résultat est mis en cache, avec pour clé l'empreinte (MD5) des chemins et des dates de modification des fichiers sources. Modifier un fichier source invalide le cache automatiquement.
- Le fichier `R/_disable_autoload.R` désactive le chargement automatique de `R/` par Shiny : `global.R` source les fichiers dans l'ordre de leur préfixe numérique.

## Contrat d'un module

```r
mod_<nom>_ui <- function(id) { ns <- NS(id); tagList(...) }
mod_<nom>_server <- function(id, app) moduleServer(id, function(input, output, session) { ... })
```

`app` est une liste partagée :

| Élément | Type | Contenu |
|---|---|---|
| `app$D()` | réactif | données restreintes au périmètre (`ind`, `imde`, `imde_communes`, `d$geometries`, `d$normes`, `d$meta`) |
| `app$filtres()` | réactif | `list(annee, cycle)` issus de l'en-tête ; cycle = `"TOUS"` possible |
| `app$utilisateur()` | réactif | identifiant, profil, périmètre |
| `app$resultats` | `reactiveValues` | `optimisation`, `priorisation`, `simulations` (réutilisés par les rapports) |
| `app$aller(page)` | fonction | navigation programmatique |

**Ajouter un module** : créer `modules/nouveau/mod_nouveau.R` avec les deux fonctions, puis ajouter une ligne dans le tableau `PAGES` de `global.R` (identifiant, titre, icône Font Awesome, section).

## Sécurité et périmètres

`restreindre()` filtre les données **côté serveur**, avant qu'elles n'atteignent les modules. Pour un utilisateur régional ou provincial, les agrégats « nationaux » et régionaux sont recalculés sur le seul périmètre autorisé. Les mots de passe sont stockés sous forme d'empreinte SHA-256 salée (16 caractères aléatoires par compte).

## Performance

- Au premier démarrage, la préparation complète prend 15 à 30 s (environ 263 000 lignes établissement × année) ; ensuite, quelques secondes grâce au cache.
- Les sorties des pages masquées sont suspendues par Shiny (`suspendWhenHidden`) : seule la page affichée est calculée.
- Les calculs lourds (sensibilité, bootstrap, prévisions de toutes les provinces, optimisation, simulations) se lancent par bouton, avec une barre de progression.
- L'accessibilité utilise un plus proche voisin vectorisé par blocs (une à deux secondes pour environ 1 700 foyers et 20 000 établissements).

## Hors ligne

Aucune police Google ni CDN : polices système, icônes Font Awesome, plotly, leaflet et DataTables sont servis par les packages R. Seul le fond de carte en ligne (option `fond_carte_en_ligne`) nécessite Internet ; les cartes restent lisibles sans lui.

## Évolution vers PostgreSQL / PostGIS

Les fonctions `lire_*` de `R/10_chargement.R` sont les seuls points d'entrée des données. Pour une base PostGIS, il suffit d'écrire des fonctions équivalentes (`DBI::dbGetQuery()` pour le panel, `sf::st_read(conn, query=...)` pour les géométries) qui renvoient les mêmes colonnes, puis d'ajouter une source `postgis` dans `config.yml`. Les tables `DIM_GEO`, `FAIT_OBSERVATION` et `PANEL_PROVINCE` du classeur EXX_01 correspondent au modèle de stockage prévu (cf. « Architecture de stockage des données »).

## Tests

```r
testthat::test_dir("tests/testthat")
```

Couverture : utilitaires, formules des indicateurs et des déficits, agrégation sans compensation, contrôles de qualité, bornes et classes de l'IMDE, robustesse, TOPSIS (cas de référence et valeurs manquantes), optimisation (budget, bornes, disponibilités, équité, PLNE exacte contre relaxation), prévisions et backtesting, contrat de lecture EXX_01.
