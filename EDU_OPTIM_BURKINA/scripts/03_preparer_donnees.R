# ==========================================================================
# 03_preparer_donnees.R — Prépare (et met en cache) indicateurs et IMDE
# ==========================================================================
# Facultatif : l'application le fait au premier démarrage. Utile après un
# changement de données, pour vérifier que tout se calcule sans erreur.
#   source("scripts/03_preparer_donnees.R", encoding = "UTF-8")

suppressPackageStartupMessages(library(sf))
sf::sf_use_s2(FALSE)
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
t0 <- Sys.time()
D <- preparer_tout(lire_config(), forcer = TRUE)
message(sprintf("Préparation terminée en %.1f s : %d lignes province, %s communes, cycles : %s",
                as.numeric(Sys.time() - t0, units = "secs"), nrow(D$ind$province),
                if (is.null(D$ind$commune)) 0 else length(unique(D$ind$commune$code_geo)),
                paste(names(D$imde), collapse = ", ")))
im <- D$imde$PRIMAIRE; im <- im[im$annee_debut == max(im$annee_debut), ]
print(head(im[order(-im$imde), c("nom", "imde", "classe_imde")], 10))
