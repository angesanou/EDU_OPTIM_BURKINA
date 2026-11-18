# ==========================================================================
# 28_rapports.R — Rapports PDF, exports Excel / CSV, graphiques et cartes
# ==========================================================================
# Les PDF sont produits avec grid + ggplot2 (aucune dépendance à LaTeX,
# pandoc ou Internet) : ils fonctionnent sur tout poste où R est installé.

# --- Exports tabulaires --------------------------------------------------------
exporter_excel <- function(feuilles, fichier, mention, filtres = NULL) {
  wb <- openxlsx::createWorkbook(creator = "EDU-OPTIM BURKINA")
  st_titre <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#0B1A2E")
  st_ment <- openxlsx::createStyle(fontColour = "#D1495B", textDecoration = "bold")
  st_ent <- openxlsx::createStyle(fgFill = "#0B1A2E", fontColour = "#FFFFFF", textDecoration = "bold",
                                  halign = "left", border = "bottom", wrapText = TRUE)
  openxlsx::addWorksheet(wb, "LISEZ_MOI", tabColour = "#0E6E7E")
  infos <- c("EDU-OPTIM BURKINA : export de données", mention,
             paste("Généré le", format(Sys.time(), "%d/%m/%Y %H:%M")), "",
             if (!is.null(filtres)) paste(names(filtres), ":", unlist(filtres)),
             "", paste("Feuilles :", paste(names(feuilles), collapse = ", ")))
  openxlsx::writeData(wb, "LISEZ_MOI", infos, startRow = 1, colNames = FALSE)
  openxlsx::addStyle(wb, "LISEZ_MOI", st_titre, rows = 1, cols = 1)
  openxlsx::addStyle(wb, "LISEZ_MOI", st_ment, rows = 2, cols = 1)
  openxlsx::setColWidths(wb, "LISEZ_MOI", 1, 100)
  for (n in names(feuilles)) {
    df <- feuilles[[n]]
    if (is.null(df) || !nrow(df)) next
    nom <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "", n), 1, 31)
    openxlsx::addWorksheet(wb, nom)
    openxlsx::writeData(wb, nom, mention, startRow = 1)
    openxlsx::addStyle(wb, nom, st_ment, rows = 1, cols = 1)
    openxlsx::writeData(wb, nom, as.data.frame(df), startRow = 3, headerStyle = st_ent)
    openxlsx::freezePane(wb, nom, firstActiveRow = 4)
    openxlsx::setColWidths(wb, nom, seq_len(ncol(df)), "auto")
  }
  openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
  invisible(fichier)
}

exporter_csv <- function(df, fichier) {
  data.table::fwrite(df, fichier, sep = ";", dec = ",", bom = TRUE)
  invisible(fichier)
}

# --- Graphiques statiques (ggplot) --------------------------------------------
gg_classement_imde <- function(im, titre = "Classement IMDE", n = 20, mention = "") {
  x <- im[!is.na(im$imde), ]
  x <- head(x[order(-x$imde), ], n)
  x$nom <- factor(x$nom, levels = rev(x$nom))
  ggplot2::ggplot(x, ggplot2::aes(imde, nom, fill = classe_imde)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", imde)), hjust = -0.15, size = 3, colour = COULEURS$texte) +
    ggplot2::scale_fill_manual(values = setNames(PALETTE_DEFICIT, CLASSES_IMDE), drop = FALSE, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
    ggplot2::labs(title = titre, x = "IMDE (0 = aucun déficit, 100 = déficit maximal)", y = NULL, caption = mention) +
    theme_edu()
}

gg_carte <- function(couche, variable, titre, mention = "", classes = TRUE) {
  if (is.null(couche)) return(NULL)
  p <- ggplot2::ggplot(couche)
  p <- if (classes) {
    couche$classe <- classer_imde(couche[[variable]])
    p + ggplot2::geom_sf(data = couche, ggplot2::aes(fill = classe), colour = "white", linewidth = 0.25) +
      ggplot2::scale_fill_manual(values = setNames(PALETTE_DEFICIT, CLASSES_IMDE), drop = FALSE, na.value = "#E9EDF2", name = NULL)
  } else {
    p + ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), colour = "white", linewidth = 0.25) +
      ggplot2::scale_fill_gradientn(colours = PALETTE_DEFICIT, na.value = "#E9EDF2", name = NULL)
  }
  p + ggplot2::labs(title = titre, caption = paste0(mention, ". ", libelle_limites(court = TRUE))) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom",
                   plot.caption = ggplot2::element_text(colour = "#8A98AB", size = 7, hjust = 0))
}

gg_deficits_regions <- function(reg, mention = "") {
  x <- reg[, c("nom", "deficit_salles", "deficit_enseignants")]
  x <- rbind(data.frame(nom = x$nom, ressource = "Salles de classe", valeur = x$deficit_salles),
             data.frame(nom = x$nom, ressource = "Enseignants", valeur = x$deficit_enseignants))
  ggplot2::ggplot(x, ggplot2::aes(stats::reorder(nom, valeur), valeur, fill = ressource)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("Salles de classe" = COULEURS$petrole, "Enseignants" = COULEURS$orange), name = NULL) +
    ggplot2::labs(title = "Déficits par région", x = NULL, y = "Unités manquantes", caption = mention) + theme_edu()
}

# --- Mise en page PDF (grid) ---------------------------------------------------
.page_pdf <- function(titre, mention, numero, corps) {
  grid::grid.newpage()
  grid::grid.rect(x = 0.5, y = 1, height = grid::unit(20, "mm"), just = "top", gp = grid::gpar(fill = "#0B1A2E", col = NA))
  grid::grid.text("EDU-OPTIM BURKINA", x = grid::unit(12, "mm"), y = grid::unit(1, "npc") - grid::unit(8.5, "mm"),
                  just = "left", gp = grid::gpar(col = "white", fontsize = 13, fontface = "bold"))
  grid::grid.text(titre, x = grid::unit(12, "mm"), y = grid::unit(1, "npc") - grid::unit(14.5, "mm"),
                  just = "left", gp = grid::gpar(col = "#7EE3D8", fontsize = 8.5))
  grid::grid.rect(x = 0.5, y = grid::unit(1, "npc") - grid::unit(20, "mm"), height = grid::unit(5.5, "mm"), just = "top",
                  gp = grid::gpar(fill = "#3A1A24", col = NA))
  grid::grid.text(mention, y = grid::unit(1, "npc") - grid::unit(22.75, "mm"), gp = grid::gpar(col = "#FFD9DF", fontsize = 6.8, fontface = "bold"))
  grid::grid.text(sprintf("Généré le %s, page %d", format(Sys.time(), "%d/%m/%Y"), numero),
                  x = grid::unit(1, "npc") - grid::unit(12, "mm"), y = grid::unit(7, "mm"), just = "right",
                  gp = grid::gpar(fontsize = 7, col = "#8A98AB"))
  grid::pushViewport(grid::viewport(x = 0.5, y = grid::unit(13, "mm"), width = grid::unit(1, "npc") - grid::unit(24, "mm"),
                                    height = grid::unit(1, "npc") - grid::unit(43, "mm"), just = "bottom"))
  corps()
  grid::popViewport()
}

.texte <- function(lignes, y, taille = 9.5, gras = FALSE, couleur = "#142235", largeur = 105, interligne = 1.45) {
  lignes <- unlist(lapply(lignes, function(l) if (nchar(l) == 0) "" else strwrap(l, largeur)))
  for (l in lignes) {
    grid::grid.text(l, x = 0, y = grid::unit(y, "npc"), just = c("left", "top"),
                    gp = grid::gpar(fontsize = taille, fontface = if (gras) "bold" else "plain", col = couleur))
    y <- y - (taille * interligne) / 72 / 10.3
  }
  y
}

.tableau_pdf <- function(df, y, largeurs = NULL, taille = 8) {
  n <- ncol(df); largeurs <- largeurs %||% rep(1 / n, n)
  x0 <- c(0, cumsum(largeurs))[seq_len(n)]
  pas <- (taille * 1.7) / 72 / 10.3
  grid::grid.rect(x = 0, y = grid::unit(y, "npc"), width = 1, height = grid::unit(pas, "npc"), just = c("left", "top"),
                  gp = grid::gpar(fill = "#0B1A2E", col = NA))
  for (j in seq_len(n)) grid::grid.text(names(df)[j], x = x0[j] + 0.005, y = grid::unit(y - pas / 2, "npc"), just = "left",
                                        gp = grid::gpar(fontsize = taille, col = "white", fontface = "bold"))
  y <- y - pas
  for (i in seq_len(nrow(df))) {
    if (i %% 2 == 0) grid::grid.rect(x = 0, y = grid::unit(y, "npc"), width = 1, height = grid::unit(pas, "npc"),
                                     just = c("left", "top"), gp = grid::gpar(fill = "#F4F6F9", col = NA))
    for (j in seq_len(n)) grid::grid.text(as.character(df[i, j]), x = x0[j] + 0.005, y = grid::unit(y - pas / 2, "npc"),
                                          just = "left", gp = grid::gpar(fontsize = taille, col = "#142235"))
    y <- y - pas
  }
  y - pas / 2
}

.kpis_pdf <- function(kpis, y) {
  n <- length(kpis); w <- 1 / n
  for (i in seq_len(n)) {
    x <- (i - 1) * w
    grid::grid.roundrect(x = x + 0.004, y = grid::unit(y, "npc"), width = w - 0.008, height = grid::unit(0.075, "npc"),
                         just = c("left", "top"), r = grid::unit(2, "mm"), gp = grid::gpar(fill = "#F4F6F9", col = "#E4E9F0"))
    grid::grid.text(names(kpis)[i], x = x + 0.015, y = grid::unit(y - 0.017, "npc"), just = "left",
                    gp = grid::gpar(fontsize = 7.2, col = "#5A6B80"))
    grid::grid.text(kpis[[i]], x = x + 0.015, y = grid::unit(y - 0.05, "npc"), just = "left",
                    gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#0B1A2E"))
  }
  y - 0.09
}

.graphique <- function(p, y_haut, hauteur) {
  if (is.null(p)) return(invisible())
  print(p, vp = grid::viewport(x = 0, y = y_haut, width = 1, height = hauteur, just = c("left", "top")))
}

ouvrir_pdf <- function(fichier) {
  ok <- tryCatch({ grDevices::cairo_pdf(fichier, width = 8.27, height = 11.69, onefile = TRUE, family = "sans"); TRUE },
                 error = function(e) FALSE)
  if (!ok) grDevices::pdf(fichier, width = 8.27, height = 11.69, onefile = TRUE, encoding = "ISOLatin1")
}

#' Recommandations automatiques à partir des dimensions de l'IMDE.
recommandations <- function(ligne) {
  dims <- c("Infrastructures" = "Programmer la construction et la réhabilitation de salles de classe (déficit estimé : %s salles).",
            "Ressources humaines" = "Affecter ou recruter des enseignants qualifiés (déficit estimé : %s enseignants).",
            "Équipements" = "Doter les établissements en tables-bancs et en latrines (déficit : %s tables-bancs).",
            "Conditions d'apprentissage" = "Raccorder les établissements à l'eau et à l'électricité ; étendre les cantines (%s établissements sans eau).",
            "Accès et couverture" = "Renforcer l'offre de proximité et la continuité éducative (écoles fermées, élèves déplacés).")
  sc <- unlist(ligne[paste0("dim_", names(dims))])
  ordre <- order(-sc)
  val <- c(fmt_nombre(ligne$deficit_salles), fmt_nombre(ligne$deficit_enseignants), fmt_nombre(ligne$deficit_tables_bancs),
           fmt_nombre(ligne$deficit_eau), "")
  vapply(head(ordre, 3), function(i) sprintf("%s (score %s/100) : %s", names(dims)[i], fmt_nombre(sc[i], 0),
                                             if (grepl("%s", dims[[i]], fixed = TRUE)) sprintf(dims[[i]], val[i]) else dims[[i]]), character(1))
}

#' Rapport exécutif national (ou du périmètre) — 3 à 4 pages.
rapport_executif_pdf <- function(fichier, ctx) {
  ouvrir_pdf(fichier)
  on.exit(grDevices::dev.off(), add = TRUE)
  .page_pdf("Rapport exécutif : synthèse pour les décideurs", ctx$mention, 1, function() {
    y <- .texte(ctx$titre, 0.995, taille = 18, gras = TRUE)
    y <- .texte(ctx$sous_titre, y - 0.005, taille = 9.5, couleur = "#5A6B80")
    y <- .kpis_pdf(ctx$kpis, y - 0.02)
    y <- .texte("Messages clés", y - 0.01, taille = 12, gras = TRUE)
    y <- .texte(paste("•", ctx$messages), y - 0.008, taille = 9.5, largeur = 110)
    .graphique(ctx$graph_classement, y - 0.02, y - 0.03)
  })
  .page_pdf("Cartographie du déficit et priorités", ctx$mention, 2, function() {
    .graphique(ctx$carte, 1, 0.5)
    y <- .texte("Territoires prioritaires (TOPSIS)", 0.47, taille = 12, gras = TRUE)
    if (!is.null(ctx$priorites)) .tableau_pdf(ctx$priorites, y - 0.01, c(0.06, 0.26, 0.2, 0.12, 0.36))
  })
  .page_pdf("Déficits et besoins", ctx$mention, 3, function() {
    .graphique(ctx$graph_deficits, 1, 0.48)
    y <- .texte("Déficits estimés (normes de l'application)", 0.49, taille = 12, gras = TRUE)
    y <- .tableau_pdf(ctx$tableau_deficits, y - 0.01, c(0.45, 0.25, 0.3))
    if (!is.null(ctx$optim)) {
      y <- .texte("Allocation optimale proposée", y - 0.02, taille = 12, gras = TRUE)
      y <- .tableau_pdf(ctx$optim, y - 0.01, c(0.34, 0.16, 0.16, 0.16, 0.18))
    }
    .texte(c("", "Méthode : indicateurs calculés à partir de la base au format EXX_01 ; IMDE à 5 dimensions (min-max à bornes fixes, poids égaux) ;",
             "priorisation TOPSIS à 7 critères ; optimisation par programmation linéaire sous contrainte budgétaire.",
             "Les associations statistiques présentées ne constituent pas des preuves causales."), y - 0.02, taille = 8, couleur = "#5A6B80")
  })
  invisible(fichier)
}

#' Fiche de diagnostic territorial (une province).
fiche_territoriale_pdf <- function(fichier, f) {
  ouvrir_pdf(fichier)
  on.exit(grDevices::dev.off(), add = TRUE)
  .page_pdf(paste0("Fiche de diagnostic territorial : ", f$nom), f$mention, 1, function() {
    y <- .texte(f$nom, 0.995, taille = 20, gras = TRUE)
    y <- .texte(f$sous_titre, y - 0.004, taille = 9.5, couleur = "#5A6B80")
    y <- .kpis_pdf(f$kpis, y - 0.02)
    y <- .tableau_pdf(f$comparaison, y - 0.01, c(0.4, 0.2, 0.2, 0.2))
    y <- .texte("Recommandations (dimensions les plus déficitaires)", y - 0.015, taille = 12, gras = TRUE)
    y <- .texte(paste("•", f$recommandations), y - 0.008, taille = 9.2, largeur = 112)
    .graphique(f$graph_dimensions, y - 0.015, max(0.18, y - 0.03))
  })
  .page_pdf(paste0("Évolution : ", f$nom), f$mention, 2, function() {
    .graphique(f$graph_evolution, 1, 0.48)
    .graphique(f$graph_ratios, 0.5, 0.48)
  })
  invisible(fichier)
}
