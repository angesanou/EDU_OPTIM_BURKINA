# Méthodologie scientifique

Cette page décrit les méthodes appliquées par la plateforme aux données réelles de la base EXX_01 : 45 provinces, années scolaires 2015-2016 à 2024-2025, quatre cycles d'enseignement. Les mêmes méthodes s'appliquent à tout classeur de même format.

## 1. Unités d'analyse et sources

| Niveau | Unité | Origine |
|---|---|---|
| Province × cycle × année | 45 provinces (découpage en 13 régions), 4 cycles, 2015-2016 à 2024-2025 | feuille `PANEL_PROVINCE` de la base EXX_01 |
| Région, pays | agrégats des provinces | calculés : sommes, puis ratios recalculés |
| Contexte | pauvreté (EHCVM 2018 et 2021), urbanisation (RGPH 2019), insécurité (relevé ESU du 31/12/2021) | `data/reelles/contexte_province.csv` |
| Limites administratives | 45 provinces | geoBoundaries (Banque mondiale), licence CC BY 4.0 |

**Règle d'agrégation.** Les variables de comptage (effectifs, salles, enseignants) s'additionnent. Les ratios et les taux sont recalculés à partir des sommes et ne sont jamais moyennés. Les déficits sont calculés au niveau provincial puis additionnés : l'excédent d'une province ne compense pas le manque d'une autre.

## 2. Indicateurs et déficits

Notations : *E* effectif d'élèves, *S* salles, *S*<sub>b</sub> salles en bon état, *M* enseignants, *F* établissements fonctionnels, *P* population d'âge scolaire du cycle.

| Indicateur | Définition | Lecture |
|---|---|---|
| Élèves par salle utilisable | *E* / *S*<sub>b</sub> | pression sur les locaux |
| Part des salles en mauvais état | (*S* − *S*<sub>b</sub>) / *S* | état du patrimoine |
| Élèves par enseignant | *E* / *M* | charge d'encadrement |
| Couverture en eau, électricité, latrines, cantine | établissements équipés / *F* | conditions d'apprentissage |
| Taux brut de scolarisation | 100 × *E* / *P* | couverture de la demande |
| Taux de fonctionnalité | *F* / établissements recensés | continuité du service |
| Établissements pour 100 km² | 100 × établissements / superficie | proximité de l'offre |

Le déficit d'une ressource compare le besoin défini par la norme au stock existant. Pour les salles de classe :

$$Déficit<sub>salles</sub> = max ( 0 ; &lceil; <i>E</i> / taille maximale de classe &rceil; &minus; <i>S</i><sub>b</sub> )$$

Les enseignants, les tables-bancs et les latrines suivent la même règle avec leur norme respective. Pour l'eau, l'électricité et les cantines, le déficit est le nombre d'établissements fonctionnels non équipés.

**Normes officielles appliquées** (feuille `NORMES`, sources indiquées ligne par ligne) : 50 élèves par classe au primaire et 25 au préscolaire (MENA, normes EHA, 2022) ; 40 élèves par enseignant au primaire (PSEF 2017-2030) ; 100 élèves par cabine de latrine au primaire et 38 au préscolaire (MENA, 2022), 70 aux autres cycles (UNICEF, 2014) ; deux places par table-banc.

## 3. Indice multidimensionnel de déficit éducatif (IMDE)

### 3.1 Structure

| Dimension | Indicateurs | Levier d'action |
|---|---|---|
| Infrastructures | élèves par salle utilisable, part des salles en mauvais état, taux de déficit en salles | construire, réhabiliter |
| Ressources humaines | élèves par enseignant, part d'enseignants non qualifiés (si publiée) | recruter, affecter |
| Équipements | élèves par table-banc et par cabine (si publiés), part des établissements sans latrines | équiper |
| Conditions d'apprentissage | part des établissements sans eau, sans électricité, sans cantine ; élèves par groupe pédagogique | raccorder, nourrir |
| Accès et couverture | TBS, taux de fonctionnalité, établissements pour 100 km², part des élèves touchés par les fermetures | ouvrir, rouvrir |

Les indicateurs ont été retenus pour leur lien direct avec un levier de politique publique, leur disponibilité dans les annuaires et une redondance limitée.

### 3.2 Normalisation

Pour chaque cycle, les bornes min et max sont les percentiles 2 % et 98 % de l'indicateur sur toutes les provinces et toutes les années. Ces bornes sont fixes : une baisse du score traduit une amélioration réelle, et non un déplacement de la distribution. Les valeurs hors bornes sont ramenées dans l'intervalle [0 ; 1].

$$<i>s<sub>i</sub></i> = (<i>x<sub>i</sub></i> &minus; min) / (max &minus; min) &nbsp;&nbsp; ou &nbsp;&nbsp; <i>s<sub>i</sub></i> = (max &minus; <i>x<sub>i</sub></i>) / (max &minus; min) pour un indicateur favorable$$

### 3.3 Agrégation et classes

Le score de dimension est la moyenne des scores disponibles de la dimension, ce qui repondère un indicateur manquant au sein de sa dimension. L'indice est la moyenne pondérée des dimensions, à poids égaux par défaut, selon la convention de l'indice de développement humain et de l'indice de pauvreté multidimensionnelle. Les poids restent modifiables dans l'interface.

$$IMDE = 100 &times; &sum;<sub><i>d</i></sub> <i>w<sub>d</sub></i> <i>D<sub>d</sub></i> / &sum;<sub><i>d</i></sub> <i>w<sub>d</sub></i>$$

L'indice n'est calculé que si au moins trois dimensions sur cinq sont renseignées. Les classes sont à seuils fixes : favorable (moins de 25), modéré (25 à 37,5), préoccupant (37,5 à 50), sévère (50 à 62,5), critique (62,5 et plus).

### 3.4 Robustesse et sensibilité

Six variantes sont comparées : normalisation par bornes, par score centré réduit ou par rangs, agrégation arithmétique ou géométrique (non compensatoire), poids égaux, issus de l'analyse en composantes principales ou de l'entropie. La concordance des classements est mesurée par la corrélation de Spearman. La sensibilité aux poids repose sur 500 vecteurs tirés selon une loi de Dirichlet ; on retient pour chaque province l'intervalle de rang à 90 % et la probabilité d'appartenir au quart le plus déficitaire.

### 3.5 Limites

L'IMDE mesure des moyens et des conditions d'accueil, pas les acquis des élèves. Sa version principale est compensatoire. Ses valeurs dépendent des bornes retenues ; les rangs y sont en revanche peu sensibles.

## 4. Analyse spatiale

Le voisinage est défini par contiguïté (provinces partageant une frontière), avec des poids standardisés. L'indice global de Moran mesure si les provinces voisines se ressemblent ; il est testé par 999 permutations.

$$<i>I</i> = (<i>n</i> / <i>S</i><sub>0</sub>) &times; &sum;<sub><i>i</i></sub> &sum;<sub><i>j</i></sub> <i>w<sub>ij</sub></i> <i>z<sub>i</sub></i> <i>z<sub>j</sub></i> / &sum;<sub><i>i</i></sub> <i>z<sub>i</sub></i><sup>2</sup>$$

Les indicateurs locaux d'association spatiale (Anselin, 1995) classent ensuite chaque province, au seuil de 5 %, en **groupe de valeurs élevées** (la province et ses voisines ont des valeurs élevées), **groupe de valeurs faibles**, **valeur élevée isolée**, **valeur faible isolée** ou **pas de regroupement marqué**. Pour l'IMDE, un groupe de valeurs élevées signale une zone de concentration des déficits, où une intervention coordonnée entre provinces se justifie. Ces tests locaux ne sont pas corrigés pour les comparaisons multiples et doivent être lus comme exploratoires.

## 5. Économétrie

Le modèle de panel province × année relie, pour un cycle donné, une variable de déficit à ses facteurs associés :

$$<i>y<sub>it</sub></i> = &alpha;<sub><i>i</i></sub> + &beta;&prime; <i>x<sub>it</sub></i> + &epsilon;<sub><i>it</i></sub> &nbsp;&nbsp; (effets fixes) &nbsp;&nbsp;&nbsp; <i>y<sub>it</sub></i> = &alpha;<sub><i>i</i></sub> + &gamma;<sub><i>t</i></sub> + &beta;&prime; <i>x<sub>it</sub></i> + &epsilon;<sub><i>it</i></sub> &nbsp;&nbsp; (double effets fixes)$$

Variables expliquées : IMDE, élèves par salle, élèves par enseignant, taux de déficit en salles, TBS, part des établissements sans eau. Variables explicatives : pauvreté, urbanisation, part de déplacés internes, insécurité, densité, croissance des effectifs. Le choix du modèle s'appuie sur les tests de Fisher, de Hausman, de Breusch-Pagan, de Wooldridge et de Pesaran, et sur les facteurs d'inflation de la variance. L'inférence utilise des erreurs-types robustes regroupées par province et un bootstrap par grappes. Les coefficients sont des associations conditionnelles : ils ne sont jamais présentés comme des effets causaux.

## 6. Prévisions

Les effectifs sont projetés par six méthodes : tendance linéaire et log-linéaire, lissage de Holt amorti, modèles ETS et ARIMA, et un modèle démographique (population projetée multipliée par le TBS prolongé, plafonné à 125 % du maximum observé). Les méthodes sont départagées par validation sur les trois dernières années (erreur absolue moyenne, erreur quadratique, erreur relative, couverture des intervalles à 95 %).

$$Besoin<sub>horizon</sub> = &lceil; effectif projeté / norme &rceil; &minus; stock actuel$$

Avec une dizaine d'observations par série et une rupture des projections démographiques en 2021, les résultats sont des ordres de grandeur.

## 7. Priorisation (TOPSIS)

Sept critères sont combinés : déficit (IMDE, 30 %), population scolaire (15 %), vulnérabilité (pauvreté, déplacés, fermetures, 15 %), accessibilité, pression démographique, coût par élève bénéficiaire et équité (10 % chacun). Chaque critère est écrêté aux percentiles 5 % et 95 %, normalisé et pondéré. Chaque province est ensuite située entre la situation idéale *A*<sup>+</sup> et la plus défavorable *A*<sup>−</sup> :

$$<i>C<sub>i</sub></i> = <i>d<sub>i</sub></i><sup>&minus;</sup> / ( <i>d<sub>i</sub></i><sup>+</sup> + <i>d<sub>i</sub></i><sup>&minus;</sup> ) &nbsp;&isin; [0 ; 1], &nbsp; 1 = priorité maximale$$

La contribution de chaque critère au score justifie le rang. La stabilité est vérifiée par 300 perturbations des poids de plus ou moins 30 %. Les poids par défaut relèvent d'un choix politique et sont modifiables.

## 8. Optimisation

L'allocation du budget est un programme linéaire résolu avec lpSolve. On note *u* une province (pour un cycle), *r* une ressource, *x*<sub>ur</sub> la quantité allouée, *d*<sub>ur</sub> le déficit, *D*<sub>r</sub> le déficit total de la ressource, *c*<sub>r</sub> son coût unitaire et *B* le budget.

$$max &nbsp;&sum;<sub><i>u</i></sub> &sum;<sub><i>r</i></sub> <i>w<sub>u</sub></i> <i>v<sub>r</sub></i> <i>x<sub>ur</sub></i> / <i>D<sub>r</sub></i> + &lambda; <i>z</i> &sum;<sub><i>r</i></sub> <i>v<sub>r</sub></i>, &nbsp;&nbsp; avec <i>w<sub>u</sub></i> = 0,5 + IMDE<sub><i>u</i></sub> / 100$$

Contraintes : le coût total ne dépasse pas le budget ; la quantité allouée ne dépasse ni le déficit de la province ni la disponibilité de la ressource ; chaque province reçoit une couverture minimale *z* de ses besoins ; chaque ressource atteint un taux de couverture minimal commun, afin que l'optimum ne privilégie pas les seuls équipements peu coûteux ; les provinces critiques peuvent recevoir un besoin minimal garanti. La solution continue est arrondie puis complétée de façon gloutonne selon le rapport valeur sur coût ; elle est entière et réalisable. Les indicateurs et l'IMDE sont recalculés après intervention, avec les mêmes bornes.

Limites : les coûts unitaires des salles (12 millions de FCFA) et des forages (8 millions) sont documentés, les autres sont à confirmer ; ni l'entretien, ni les délais de construction, ni la capacité d'exécution ne sont modélisés ; le gain est mesuré sur des déficits normatifs.

## 9. Simulations

Chaque scénario projette les effectifs à l'horizon choisi, à stocks constants, puis peut redéployer une partie des enseignants excédentaires (problème de transport minimisant la distance) avant d'optimiser le budget du scénario. Les scénarios sont comparés selon le coût, l'IMDE moyen pondéré, l'indice de Gini de l'IMDE, le nombre de provinces sévères ou critiques et le nombre d'élèves bénéficiaires.

## 10. Adaptation aux données disponibles

Toutes les variables ne sont pas publiées au niveau provincial. La complétude est mesurée variable par variable dans l'onglet `COMPLETUDE_TABLEAU` de la base traitée.

| Cycle | Disponible par province | Non publié par province |
|---|---|---|
| Primaire | effectifs, taux, établissements, salles et leur état, enseignants, eau, électricité, latrines, cantines | enseignants qualifiés, cabines de latrines, tables-bancs |
| Préscolaire | effectifs, TBS, structures et salles (en partie estimées) | enseignants, état des salles, équipements |
| Post-primaire, secondaire | effectifs, nouveaux inscrits, TBA, TBS, TNS, TAP, indice de parité (2018-2024) | établissements, salles, enseignants, équipements |

La plateforme applique en conséquence six règles. Aucune valeur n'est inventée : une variable non publiée reste manquante. Un ratio agrégé ne compte que les élèves des unités dont le stock est connu. Les besoins et les déficits ne sont calculés que lorsque le stock est connu, donc au primaire ; l'optimisation et les besoins à l'horizon portent sur ce cycle. L'IMDE n'est calculé qu'avec trois dimensions au moins, soit au primaire ; pour les autres cycles, l'analyse porte sur l'accès et l'équité, et la priorisation neutralise le critère de déficit. Le taux de fonctionnalité n'est calculé que pour l'année où les fermetures sont connues. L'agrégat « Tous cycles » ne couvre que les années 2018 à 2024, où les quatre cycles sont présents.

## Références

Anselin L. (1995), *Local Indicators of Spatial Association – LISA*, Geographical Analysis 27(2). Hwang C.-L. et Yoon K. (1981), *Multiple Attribute Decision Making*, Springer. OCDE et JRC (2008), *Handbook on Constructing Composite Indicators*. Alkire S. et Foster J. (2011), *Counting and multidimensional poverty measurement*, Journal of Public Economics. Wooldridge J. M. (2010), *Econometric Analysis of Cross Section and Panel Data*, MIT Press. Hyndman R. J. et Athanasopoulos G. (2021), *Forecasting: Principles and Practice*, 3e édition.
