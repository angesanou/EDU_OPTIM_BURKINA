# Guide d'utilisation

## Principes de navigation

- **Barre latérale** : les 14 modules, regroupés par usage (Pilotage, Données, Analyse, Décision, Restitution, Administration).
- **En-tête** : deux filtres globaux, **Année** et **Cycle**, appliqués à toutes les pages ; « Tous cycles » additionne les quatre cycles. À droite, votre profil et votre périmètre.
- **Bandeau** en haut de page : il rappelle l'objet de la plateforme (ou, en mode démonstration, que les données sont synthétiques).
- Chaque carte de résultat peut être **agrandie** (icône en bas à droite). Les tableaux se **trient**, se **filtrent** et s'**exportent** (Copier, CSV, Excel).

## Parcours conseillé pour un décideur

1. **Accueil** : la situation en une page (indicateurs clés, alertes, synthèse automatique).
2. **Tableau de bord** : comparer les régions, suivre les tendances, voir où se concentrent les déficits.
3. **Indice IMDE** : identifier les territoires les plus déficitaires et la dimension en cause.
4. **Priorisation** : ajuster les poids des critères selon les orientations politiques.
5. **Optimisation** : saisir le budget, obtenir l'allocation optimale et l'IMDE après intervention.
6. **Simulations** : comparer plusieurs scénarios budgétaires et démographiques.
7. **Rapports** : télécharger le rapport exécutif PDF et les fiches territoriales.

## Parcours conseillé pour un analyste

**Données et sources** (qualité, manquants) → **Diagnostic** (ratios et déficits à tous les niveaux) → **Cartographie** (regroupements géographiques, accessibilité) → **Économétrie** (facteurs associés, tests, bootstrap) → **Prévisions** (choix de la méthode par backtesting).

## Lire les résultats avec prudence

- Une **corrélation** n'est pas une **causalité** : le module Économétrie le rappelle sur chaque résultat.
- Les **normes** sont les normes officielles (MENA 2022, PSEF 2017-2030) ; les **coûts unitaires** des salles et des forages sont documentés, les autres sont à confirmer. Les déficits en dépendent directement et restent modifiables.
- Les **limites** des 45 provinces proviennent de geoBoundaries (Banque mondiale) ; aucun fichier d'établissements géolocalisés n'est encore disponible.
- L'IMDE mesure des **conditions**, pas des **apprentissages**.

## Astuces

- Les calculs lourds (sensibilité, bootstrap, besoins de toutes les provinces, optimisation, simulations) se lancent par **bouton**.
- Le classeur Excel du module Rapports inclut automatiquement les résultats d'optimisation et de simulation déjà calculés dans la session.
- Profils régional et provincial : l'agrégat « national » affiché devient celui de votre périmètre.
