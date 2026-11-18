# EDU-OPTIM BURKINA

**Plateforme d'optimisation de la répartition des infrastructures scolaires et du personnel enseignant**

Data Challenge Burkina 2026, Défi 3 (EDU-OPTIM). Équipe Datagenius.

EDU-OPTIM BURKINA est une application d'aide à la décision développée en R (Shiny). Elle mesure, pour chacune des 45 provinces, les déficits en salles de classe, en enseignants et en équipements de base au regard des normes nationales. Elle les synthétise dans un indice multidimensionnel de déficit éducatif (IMDE), projette les besoins à l'horizon 2030, hiérarchise les territoires prioritaires et propose une répartition optimale d'un budget donné.

## Principaux résultats (enseignement primaire, 2024-2025)

| Indicateur | Valeur |
|---|---|
| Déficit de salles de classe (norme : 50 élèves par classe) | 6 636 salles |
| Déficit d'enseignants (norme : 40 élèves par enseignant) | 589 enseignants |
| Besoins à l'horizon 2030, à stock constant | 8 347 salles et 2 405 enseignants |
| Provinces en situation critique selon l'IMDE | 6 (Loroum, Oudalan, Yagha, Soum, Kompienga, Yatenga) |
| Concentration géographique des déficits (I de Moran de l'IMDE) | 0,49 (p = 0,001) |
| Couverture des besoins chiffrés avec un budget de 100 milliards de FCFA | environ 70 % |

## Lancer l'application

**Prérequis :** R 4.2 ou plus récent ([cran.r-project.org](https://cran.r-project.org)) et une connexion Internet au premier lancement, pour installer les paquets R manquants. RStudio est facultatif.

**Windows :** télécharger le dépôt (bouton *Code*, puis *Download ZIP*), le décompresser, puis double-cliquer sur `EDU_OPTIM_BURKINA/LANCER_EDU_OPTIM.bat`. Le navigateur s'ouvre sur l'application.

**Tous systèmes (R ou RStudio) :** depuis le dossier `EDU_OPTIM_BURKINA`, exécuter :

```r
source("demarrer.R")
```

Le script installe les paquets manquants, prépare les données (environ une minute au premier lancement) puis ouvre l'application.

**Comptes de démonstration** (également affichés sur l'écran de connexion) :

| Profil | Identifiant | Mot de passe |
|---|---|---|
| National | `national` | `Demo-National-2026` |
| Régional | `centre-nord` ou `sahel` | `Demo-Region-2026` |
| Provincial | `soum` | `Demo-Province-2026` |

## Reproduire toute la chaîne

Depuis la racine du dépôt :

```bash
Rscript run_all.R            # traitement des données, préparation, 25 tests unitaires
Rscript run_all.R --app      # idem, puis lancement de l'application
Rscript run_all.R --renv     # restaure d'abord les versions exactes des paquets (renv.lock)
```

`run_all.R` enchaîne quatre étapes :

1. vérification de l'environnement (version de R, 26 paquets, versions enregistrées) ;
2. nettoyage et transformation des données collectées (`02_traitement_donnees.R`) : contrôles, typage des valeurs manquantes, imputations tracées cellule par cellule, export au format d'entrée de l'application ;
3. contrôle qualité et préparation des données de l'application (indicateurs, IMDE) ;
4. tests unitaires.

La chaîne complète s'exécute en moins d'une minute. Chaque passage est consigné dans `EDU_OPTIM_BURKINA/resultats/journal_run_all.txt`.

## Organisation du dépôt

| Chemin | Contenu |
|---|---|
| `EDU_OPTIM_BURKINA/` | Application Shiny |
| `EDU_OPTIM_BURKINA/R/` | Méthodes : indicateurs, IMDE, analyse spatiale, économétrie, prévisions, priorisation TOPSIS, optimisation, rapports |
| `EDU_OPTIM_BURKINA/modules/` | Les 14 pages de l'application |
| `EDU_OPTIM_BURKINA/config/` | Configuration, normes officielles sourcées, comptes de démonstration |
| `EDU_OPTIM_BURKINA/data/reelles/` | Base province × année au format EXX_01, variables de contexte, limites des 45 provinces |
| `EDU_OPTIM_BURKINA/documentation/` | Guide d'utilisation, méthodologie scientifique, documentation technique |
| `EDU_OPTIM_BURKINA/tests/` | Tests unitaires (testthat) |
| `02_traitement_donnees.R` | Nettoyage et transformation des données collectées |
| `run_all.R` | Reproduction de toute la chaîne |
| `03_preparer_depots.R` | Constitution des dossiers de dépôt 01 à 05 selon la convention de nommage du challenge |
| `Datagenius_01-collecte_D3_engineer_base-collecte-provinces-2015-2024_v6.xlsx` | Base collectée, étape 01 (entrée du traitement) |
| `depots/` | Livrables des étapes 01 à 05 (collecte, nettoyage, transformation, analyse, interprétation), nommés selon la convention du challenge, avec un LISEZ-MOI par étape |
| `documentation/` | Documentation du projet (PDF, 14 pages) |

## Méthodes

- **Déficits normatifs** calculés province par province à partir des normes officielles (MENA 2022, PSEF 2017-2030), puis agrégés.
- **IMDE** : cinq dimensions (infrastructures, ressources humaines, équipements, conditions d'apprentissage, accès), normalisation à bornes fixes, agrégation à poids égaux, robustesse testée par six variantes et 500 jeux de poids aléatoires (démarche OCDE/JRC).
- **Analyse spatiale** : indice de Moran et indicateurs locaux d'association spatiale.
- **Économétrie de panel** à effets fixes, erreurs-types robustes et bootstrap par grappes (associations, non causales).
- **Prévisions** à l'horizon 2030 par six méthodes départagées par validation sur les dernières années.
- **Priorisation multicritère** TOPSIS sur sept critères paramétrables.
- **Optimisation budgétaire** par programmation linéaire sous contraintes de budget, de besoin et d'équité.

Le détail figure dans `EDU_OPTIM_BURKINA/documentation/methodologie_scientifique.md` et dans la documentation du projet.

## Sources des données

Annuaires statistiques du MENAPLN (DGESS) et du CNS ; statistiques préscolaire et primaire 2016-2025 fournies par le challenge ; INSD (projections démographiques, RGPH 2019, EHCVM 2018 et 2021) ; ST-ESU (établissements fermés) ; CONASUR (personnes déplacées internes) ; geoBoundaries, d'après la Banque mondiale (limites administratives, licence CC BY 4.0) ; OCHA COD-AB (superficies). Les données sont utilisées dans le cadre du Data Challenge Burkina 2026.

## Limites

Les déficits en salles et en enseignants ne sont calculés que pour le primaire, seul cycle dont les stocks sont publiés par province. Les coûts unitaires des salles et des forages sont documentés ; les autres sont indicatifs et restent à confirmer. L'IMDE mesure des moyens et des conditions d'accueil, non les acquis des élèves.

## Dépannage

Sous Windows 11, la protection *Smart App Control* peut refuser le fichier `.dll` d'un paquet R (fenêtre « Image incorrecte »). Cliquer sur OK : le lanceur recherche automatiquement une autre copie de ce paquet sur le poste. S'il n'en trouve pas, utiliser un autre ordinateur.
