# Installation et exécution

## Prérequis

- **R ≥ 4.2** (testé avec R 4.6.0 sous Windows) et **RStudio**.
- Connexion Internet **uniquement** pour installer les packages la première fois.

## Première installation (environ 10 minutes)

1. Ouvrez `EDU_OPTIM_BURKINA.Rproj` dans RStudio. Le répertoire de travail devient alors la racine du projet.
2. Installez les packages :
   ```r
   source("scripts/00_installer_packages.R")
   ```
3. Générez les données synthétiques (environ 2 minutes) :
   ```r
   source("scripts/01_generer_donnees_synthetiques.R", encoding = "UTF-8")
   ```
   Prérequis : le classeur modèle `data/modele/EXX_01_modele_structure.xlsx` (copie de `EXX_01-collecte_D3_architect_structure-base-donnees_v1.xlsx`).
4. (Facultatif) Contrôlez les données et lancez les tests :
   ```r
   source("scripts/02_controler_donnees.R", encoding = "UTF-8")
   testthat::test_dir("tests/testthat")
   ```

## Lancer l'application

- Dans RStudio : `shiny::runApp()`, ou ouvrir `ui.R` puis cliquer sur **Run App** ;
- sans RStudio : double-clic sur **`LANCER_EDU_OPTIM.bat`**.

Le premier démarrage prépare les indicateurs (15 à 30 s) ; les suivants sont quasi immédiats.

## Comptes de démonstration

| Identifiant | Mot de passe | Profil | Périmètre |
|---|---|---|---|
| `national` | `Demo-National-2026` | National | Burkina Faso |
| `centre-nord` | `Demo-Region-2026` | Régional | Centre-Nord |
| `sahel` | `Demo-Region-2026` | Régional | Sahel |
| `soum` | `Demo-Province-2026` | Provincial | Soum |

Ces comptes sont créés automatiquement si `config/utilisateurs.csv` est vide. **Supprimez-les avant tout usage réel.** Pour désactiver la connexion en démonstration locale : `authentification: false` dans `config/config.yml`.

## Figer les versions (renv)

Une fois l'application fonctionnelle :

```r
source("scripts/04_figer_dependances_renv.R")   # crée renv.lock
```

Sur un autre poste : `install.packages("renv")`, puis `renv::restore()`.

## Chaîne complète

```r
source("scripts/run_all.R", encoding = "UTF-8")
```

Elle génère les données, les contrôle, les prépare et lance les tests.

## Déploiement

- **Poste local ou réseau interne** : `shiny::runApp(host = "0.0.0.0", port = 3838)` rend l'application accessible aux autres postes du réseau.
- **Serveur** : Shiny Server (gratuit), Posit Connect ou ShinyProxy (Docker). Servez l'application en HTTPS et déléguez l'authentification au serveur (LDAP, SSO). Conservez `R/29_authentification.R` pour les périmètres.
- Le dossier `data/prepare/` peut être supprimé à tout moment : il se reconstruit.

## Dépannage

| Symptôme | Cause probable | Solution |
|---|---|---|
| « Fichier de configuration introuvable » | répertoire de travail incorrect | ouvrir le `.Rproj`, ou `setwd()` vers la racine du projet |
| « Base introuvable » | données non générées ou chemin erroné | lancer le script 01 ou corriger `config.yml` |
| Accents illisibles dans les noms | fichier R lu sans UTF-8 | `source(..., encoding = "UTF-8")` ; RStudio : *File › Reopen with Encoding › UTF-8* |
| Cartes vides | aucune géométrie déclarée | renseigner `geometries` dans `config.yml` |
| Calculs anciens après changement des données | cache | supprimer `data/prepare/*.rds` |
