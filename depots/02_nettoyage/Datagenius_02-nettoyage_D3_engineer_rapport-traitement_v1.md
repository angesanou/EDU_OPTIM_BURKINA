# Rapport de traitement des donnees - EDU-OPTIM Burkina

Fichier source : `Collecte_data_rempli_v6.xlsx` - traitement du 18/11/2026 17:45.

## Couverture observee par cycle

| cycle | periode |
|---|---|
| POST_PRIMAIRE | 2018 - 2024 |
| PRESCOLAIRE | 2015 - 2024 |
| PRIMAIRE | 2015 - 2024 |
| SECONDAIRE | 2018 - 2024 |

## Qualite des lignes apres traitement

| cycle | IMPUTE | MANQUANT | OK | SUSPECT | ESTIME |
|---|---|---|---|---|---|
| POST_PRIMAIRE | 269 | 135 | 42 | 4 | 0 |
| PRESCOLAIRE | 0 | 0 | 0 | 8 | 442 |
| PRIMAIRE | 184 | 0 | 176 | 0 | 90 |
| SECONDAIRE | 223 | 135 | 92 | 0 | 0 |

## Origine des cellules

| origine | cellules |
|---|---|
| OBS | 36801 |
| NA_VARIABLE | 36630 |
| NON_APPLICABLE | 13500 |
| NA_LIGNE | 12150 |
| NA_PERIODE | 7875 |
| EST | 6480 |
| IMP_RETRO | 900 |
| DED | 541 |
| IMP_RAPPORT | 300 |
| IMP_INTERP | 12 |
| IMP_CALAGE | 6 |
| NA_PONCTUEL | 4 |
| IMP_PONDERE | 1 |

## Controles echoues

| regle | gravite | decision | n |
|---|---|---|---|
| POP_INCOHERENTE_CYCLES | BLOQUANT | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales | 60 |
| DOMAINE_TAUX | BLOQUANT | valeur rejetee puis reimputee si possible | 1 |
| VARIATION_ATYPIQUE | ALERTE | conservee ; a verifier | 6 |
| VARIATION_POPULATION | ALERTE | conservee ; a verifier | 6 |
| AGREGATION_REGION | ALERTE | valeurs provinciales conservees | 2 |
| VARIATION_CONTEXTE | INFO | conservee : variation coherente avec le contexte | 538 |
| IPS_INCOHERENT | INFO | IPS remplace par taux F / taux G (publie dans valeur) | 424 |
| AGREGATION_REGION | INFO | valeurs provinciales conservees | 88 |
| RUPTURE_CONTEXTE | INFO | conservee : choc coherent avec le contexte | 74 |
| RUPTURE_SERIE_POPULATION | INFO | conservee : changement de projections (RGPH 2006 puis RGPH 2019) ; utiliser periode_population dans les modeles | 50 |
| RATIO_ATYPIQUE | INFO | conserve : valeur extreme mais plausible | 31 |
| IPS_TAUX_FAIBLES | INFO | conserve : IPS peu significatif (taux < seuil) | 14 |
| ETAB_FERMES_SUP_TOTAL | INFO | attendu : l'annuaire ne compte que les ecoles fonctionnelles (PARAM$etab_t_inclut_fermes = FALSE) | 12 |
| EFFECTIF_NUL | INFO | conserve : fermeture totale probable (insecurite) | 9 |
| VARIATION_POPULATION_CONTEXTE | INFO | conservee : revision des projections dans une zone de deplacements | 3 |

## Imputations et deductions

| origine_apres | methode | n |
|---|---|---|
| IMP_RETRO | retropolation : population de la premiere annee / (1 + croissance annuelle moyenne de la province)^ecart | 900 |
| DED | IPS recalcule = taux F / taux G (publie incoherent) | 424 |
| IMP_RAPPORT | taux recale sur pop_age_scolaire corrigee (x ancienne / nouvelle population) | 120 |
| DED | fonctionnels = total recense (annuaire) | 90 |
| IMP_RAPPORT | population = population 6-11 ans de la province x rapport median des autres annees | 60 |
| IMP_RAPPORT | taux recale sur pop_age_entree corrigee (x ancienne / nouvelle population) | 60 |
| IMP_RAPPORT | taux recale sur pop_age_fin corrigee (x ancienne / nouvelle population) | 60 |
| DED | effectif nul : aucun etablissement fonctionnel (annuaire = ecoles ouvertes) | 26 |
| IMP_INTERP | interpolation lineaire entre annees observees (trou <= 2 ans) | 18 |
| IMP_CALAGE | MILIEU_EFFECTIFS : composante imputee recalee sur le total observe | 6 |
| DED | TBA = nouveaux inscrits / population d'age d'entree x 100 | 1 |
| IMP_PONDERE | taux total = moyenne des taux G et F ponderee par les effectifs G et F | 1 |
| NEUTRALISE | valeur rejetee : taux hors plage | 1 |

## Agregation provinces / regions

| controle | ECART | OK |
|---|---|---|
| POST_PRIMAIRE|SECONDAIRE eff_t vs PPS_REGION eleves_tous_t | 88 | 3 |
| PRESCOLAIRE eff_t vs PRE_REGION eff_t | 2 | 128 |
| PRESCOLAIRE nb_etab_t vs PRE_REGION structures_t | 0 | 130 |
| PRESCOLAIRE nb_salles_t vs PRE_REGION salles_t | 0 | 130 |
| PRE_REGION salles_t : somme des regions vs national | 1 | 0 |
| PRE_REGION structures_t : somme des regions vs national | 1 | 0 |
| PRIMAIRE eff_f vs PRI_REGION eff_f | 0 | 130 |
| PRIMAIRE eff_t vs PRI_REGION eff_t | 0 | 130 |
| PRIMAIRE nb_ens_t vs PRI_REGION nb_ens_t | 0 | 26 |
| PRIMAIRE nb_etab_t vs PRI_REGION nb_etab_t | 0 | 130 |
| PRIMAIRE nb_salles_t vs PRI_REGION nb_salles_t | 0 | 26 |
| PRIMAIRE nouv_cp1_t vs PRI_REGION nouv_cp1_t | 0 | 130 |

## Alertes a examiner (40 premieres)

| gravite | regle | cycle | nom_province | annee_debut | variables | valeur | attendu | decision |
|---|---|---|---|---|---|---|---|---|
| BLOQUANT | DOMAINE_TAUX | PRIMAIRE | Gourma | 2019 | cep_t | 142.7 |  | valeur rejetee puis reimputee si possible |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Boulgou | 2024 | pop_age_scolaire | 57325 | 88186.84 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Koulpélogo | 2024 | pop_age_scolaire | 61329 | 43798.69 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Kouritenga | 2024 | pop_age_scolaire | 105006 | 57866.52 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Bam | 2024 | pop_age_scolaire | 31182 | 57935.25 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Namentenga | 2024 | pop_age_scolaire | 22249 | 61980.4 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Sanmatenga | 2024 | pop_age_scolaire | 35309 | 106124.06 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Sissili | 2024 | pop_age_scolaire | 56924 | 40819.97 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Ziro | 2024 | pop_age_scolaire | 79916 | 29134.9 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Bazèga | 2024 | pop_age_scolaire | 45200 | 31651.84 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Nahouri | 2024 | pop_age_scolaire | 40192 | 22581.66 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Boulgou | 2024 | pop_age_scolaire | 34751 | 55193.91 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Koulpélogo | 2024 | pop_age_scolaire | 37178 | 27428.27 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Kouritenga | 2024 | pop_age_scolaire | 63656 | 36238.06 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Bam | 2024 | pop_age_scolaire | 20211 | 36557.68 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Namentenga | 2024 | pop_age_scolaire | 14423 | 39111.78 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Sanmatenga | 2024 | pop_age_scolaire | 22889 | 66965.97 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Sissili | 2024 | pop_age_scolaire | 34301 | 26679.51 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Ziro | 2024 | pop_age_scolaire | 50539 | 19042.22 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Bazèga | 2024 | pop_age_scolaire | 28584 | 21199.86 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Nahouri | 2024 | pop_age_scolaire | 25419 | 15127.21 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Boulgou | 2024 | pop_age_entree | 15332 | 23474.48 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Koulpélogo | 2024 | pop_age_entree | 16402 | 11680.68 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Kouritenga | 2024 | pop_age_entree | 28084 | 15431.66 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Bam | 2024 | pop_age_entree | 8138 | 15393.07 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Namentenga | 2024 | pop_age_entree | 5806 | 16467.4 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Sanmatenga | 2024 | pop_age_entree | 9215 | 28195.77 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Sissili | 2024 | pop_age_entree | 15331 | 10616.41 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Ziro | 2024 | pop_age_entree | 20960 | 7577.01 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Bazèga | 2024 | pop_age_entree | 11855 | 8169.92 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | POST_PRIMAIRE | Nahouri | 2024 | pop_age_entree | 10542 | 5829.77 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Boulgou | 2024 | pop_age_entree | 12161 | 19399.24 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Koulpélogo | 2024 | pop_age_entree | 13010 | 9751.66 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Kouritenga | 2024 | pop_age_entree | 22277 | 12883.86 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Bam | 2024 | pop_age_entree | 7333 | 12577.93 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Namentenga | 2024 | pop_age_entree | 5233 | 13457.42 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Sanmatenga | 2024 | pop_age_entree | 8304 | 23041.58 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Sissili | 2024 | pop_age_entree | 12265 | 9536.12 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Ziro | 2024 | pop_age_entree | 18363 | 6805.98 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |
| BLOQUANT | POP_INCOHERENTE_CYCLES | SECONDAIRE | Bazèga | 2024 | pop_age_entree | 10386 | 7569.06 | population remplacee (population 6-11 ans x rapport median de la province) ; taux recales |

## Etapes

| etape | operation | n |
|---|---|---|
| 2 | lignes lues (4 cycles) | 1800 |
| 2 | cellules non numeriques ignorees | 0 |
| 2 | lignes ajoutees pour completer la grille | 0 |
| 3 | cellules estimees integrees | 6480 |
| 3 | cellules dont la valeur differait de ESTIMATIONS | 0 |
| 4 | cellules sans objet (non applicables) | 13500 |
| 4 | cellules NA_LIGNE | 12150 |
| 4 | cellules NA_VARIABLE | 36720 |
| 4 | cellules NA_PERIODE | 8775 |
| 4 | cellules NA_PONCTUEL | 49 |
| 7 | releves ESU lus | 346 |
| 7 | provinces touchees par des fermetures (ESU) | 26 |
| 7.1 | cellules deduites par identite comptable | 0 |
| 7.2 | cellules mises a 0 (lignes a effectif nul) | 26 |
| 8.0 | populations incoherentes avec le primaire corrigees | 60 |
| 8.1 | valeurs rejetees (hors domaine) | 1 |
| 8.2 | valeurs rejetees (partie > tout) | 0 |
| 8.4 | taux totaux rejetes (hors intervalle G-F) | 0 |
| 8.5 | IPS recalcules (publie incoherent) | 424 |
| 8.6 | pics isoles rejetes | 0 |
| 9.1 | cellules interpolees | 7 |
| 9.2 | cellules imputees par tendance regionale | 0 |
| 9.3 | cellules deduites par identite comptable | 0 |
| 9.4 | taux et IPS deduits de leurs composantes | 1 |
| 9.5 | taux totaux reconstruits (ponderation G/F) | 1 |
| 9.6 | cellules interpolees | 11 |
| 9.7 | taux et IPS deduits de leurs composantes | 0 |
| 9.7b | composantes imputees recalees sur le total | 6 |
| 9.8 | etablissements fonctionnels deduits | 90 |
| 9.9 | superficies completees (constante) | 0 |
| 9.10 | populations totales retropolees | 900 |
| 11 | controles d'agregation en ecart | 92 |
