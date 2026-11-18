# Journal d'usage de l'IA : Équipe Datagenius, Défi 3 (EDU-OPTIM BURKINA)

## Objet

Ce journal consigne les usages des outils d'intelligence artificielle dans la réalisation du projet, les propositions retenues ou écartées, et les vérifications faites par l'équipe. Conformément au règlement du challenge, il accompagne la section « Usage des outils d'intelligence artificielle » de la documentation.

Outil utilisé : Claude (Anthropic), assistant conversationnel.

## Principe de responsabilité

La collecte des données, le choix des méthodes statistiques, des normes, des pondérations et des coûts unitaires, ainsi que toutes les décisions de traitement, relèvent exclusivement de l'équipe. L'outil n'a pas accès aux données réelles hors des échanges nécessaires à une tâche précise. Toute proposition de code, de texte ou de contrôle est relue, testée ou vérifiée par l'équipe avant d'être intégrée.

## Journal des usages

| Date | Demande de l'équipe | Usage de l'outil | Décision de l'équipe | Vérification |
|---|---|---|---|---|
| 18/11/2026 | Diagnostic du lancement de l'application sous Windows (échecs de téléchargement de paquets R, refus de connexion) | Lecture du lanceur et de `demarrer.R` ; hypothèse sur la cause (interruption du téléchargement, puis arrêt avant le démarrage du serveur) | Relancer l'installation, puis vérifier le démarrage | Démarrage confirmé par le journal d'exécution (serveur local lancé, 45 provinces chargées) |
| 18/11/2026 | Explication du blocage « contrôle d'application » sur `collapse.dll` | Explication du mécanisme de Windows et des options possibles | Aucun contournement de la sécurité du système ; hébergement en ligne envisagé pour la diffusion | À compléter par l'équipe |
| 18/11/2026 | Refonte de la documentation en mise en forme LaTeX, avec ajout des sections sur l'extraction des données et les méthodes statistiques | Rédaction et mise en forme du texte ; formules relues par rapport au code R | Retenues après relecture ; deux formulations corrigées (résolution du programme linéaire, tests de panel) | Formules confrontées aux scripts `R/21_imde`, `R/25_priorisation`, `R/26_optimisation` et `R/22_econometrie` |
| 18/11/2026 | Vérification du dépôt GitHub (structure, fichiers obligatoires, historique) | Lecture de l'arborescence et de l'historique ; signalement de points à traiter | Décision sur la publication des données réelles et sur la version finale de la documentation | À compléter par l'équipe |
| Dates antérieures | Écriture et débogage de modules de l'application, de tests unitaires et de scripts de traitement ; propositions de contrôles de cohérence et de règles d'imputation ; préparation des dossiers de dépôt | À compléter par l'équipe : pour chaque tâche, indiquer la date, la demande, la partie concernée et ce qui a été retenu | À compléter par l'équipe | À compléter par l'équipe : tests exécutés, relectures, comparaisons avec les sources |

## Vérifications générales de l'équipe

- Exécution complète de la chaîne (`run_all.R`) et des 25 tests unitaires avant chaque dépôt.
- Contrôle des totaux provinciaux contre les totaux régionaux publiés.
- Relecture des formules de la documentation par rapport au code.
- Test de l'application sur un poste vierge, en suivant le LISEZ-MOI : à compléter par l'équipe avant le dépôt.

## Éléments non délégués

Les données de la collecte, les normes officielles retenues, les pondérations de l'IMDE et de la priorisation, les coûts unitaires, les hypothèses de projection et les conclusions des résultats ont été établis par l'équipe, à partir des sources citées dans la documentation.
