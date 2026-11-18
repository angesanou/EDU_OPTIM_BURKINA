# Remplacer les données synthétiques par vos données réelles

L'application ne lit qu'**un format** : le classeur **EXX_01** (`EXX_01-collecte_D3_architect_structure-base-donnees_v1.xlsx`). Les données synthétiques sont écrites dans ce format ; la sortie de `remplir_base_v3.py` (`Collecte_data_rempli_v3.xlsx`) aussi. Le basculement tient donc en **trois étapes**.

## 1. Copier le classeur rempli

Placez votre fichier dans `data/reelles/`, par exemple `data/reelles/Collecte_data_rempli_v3.xlsx`.

## 2. Modifier `config/config.yml`

```yaml
source_donnees: "reel"

donnees:
  reel:
    base_exx: "data/reelles/Collecte_data_rempli_v3.xlsx"
    etablissements: ""     # optionnel
    contexte: ""           # optionnel
    geometries: ""         # optionnel, voir § 4
    mention: "Plateforme d'optimisation de la répartition des infrastructures scolaires et du personnel enseignant"
```

## 3. Contrôler, puis relancer

```r
source("scripts/02_controler_donnees.R", encoding = "UTF-8")   # rapport Excel dans resultats/exports/
shiny::runApp()
```

Le bandeau supérieur affiche alors la mention de la source réelle au lieu de « DONNÉES SYNTHÉTIQUES ».

## Ce que l'application attend

| Élément | Obligatoire | Détail |
|---|---|---|
| Feuille `DIM_GEO` | oui | `code_geo`, `nom_geo`, `niveau_geo`, `code_parent` (45 provinces, 13 régions) |
| Feuille `PANEL_PROVINCE` | oui | en-tête repéré par la colonne `id_ligne` ; clés `cycle`, `annee_debut`, `niveau_geo`, `code_geo` |
| Variables du panel | non | toute variable absente est créée vide : les indicateurs qui en dépendent valent « – » et l'IMDE se repondère |
| Feuille `NORMES` | non | `code_parametre`, `cycle`, `valeur` ; à défaut, normes de démonstration |
| Feuilles `PANEL_REGION`, `DICTIONNAIRE` | non | affichage et contrôle |

**Cycles reconnus** : `PRESCOLAIRE`, `PRIMAIRE`, `POST_PRIMAIRE`, `SECONDAIRE`.
**Nombres** : les formats « 1 234,5 » et les cellules texte sont convertis ; « ND », « – » et cellule vide deviennent des valeurs manquantes.

## Compléments optionnels

### Établissements (`etablissements`)

Fichier CSV, une ligne par établissement × année. Colonnes minimales : `id_etab, annee_debut, cycle, code_region, code_province, code_commune, nom_commune, statut, milieu, lon, lat, ferme, eff_t, nb_salles_t, nb_salles_bon_etat, nb_ens_t, eau, latrines, electricite, cantine`. Les autres variables du panel sont facultatives. Ce fichier active les niveaux commune et établissement, la carte des établissements, la densité et l'accessibilité.

### Contexte (`contexte`)

Fichier CSV `code_geo, annee_debut, taux_pauvrete, taux_urbanisation, indice_insecurite`. Il alimente l'économétrie et la vulnérabilité (priorisation). Sans lui, ces variables sont simplement absentes des choix proposés.

### Limites administratives (`geometries`)

- **GeoPackage** avec les couches `regions`, `provinces` (colonne `code_geo` = codes `PRVxxxx`) et, optionnellement, `communes` ; ou
- **tout fichier de limites provinciales** (GeoJSON, shapefile, gpkg, par exemple COD-AB de l'ancien découpage) : l'application rapproche automatiquement les provinces par leur **nom** (sans accents ni casse) et reconstitue les régions.

Sans géométrie, les modules fonctionnent ; seules les cartes affichent un message. En attendant les limites officielles, vous pouvez réutiliser `data/synthetique/geometries_synthetiques.gpkg` (formes approximatives).

## Points d'attention avec la base réelle

- Ne recopiez jamais les chiffres régionaux sur les provinces : ils vont dans `PANEL_REGION`.
- Les années sans donnée provinciale (par exemple le post-primaire 2023) apparaissent comme manquantes : les séries temporelles et les prévisions s'adaptent, mais les backtestings seront moins fiables.
- Vérifiez que `nb_etab_t` inclut ou non les établissements fermés, et renseignez `nb_etab_fermes` en cohérence (`nb_etab_fonctionnels = nb_etab_t − nb_etab_fermes`).
- Remplacez les normes de démonstration par les normes officielles (feuille `NORMES`, colonne `source` renseignée).
