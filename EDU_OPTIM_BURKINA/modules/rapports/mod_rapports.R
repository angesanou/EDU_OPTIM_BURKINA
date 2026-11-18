# ==========================================================================
# MODULE 12 — RAPPORTS ET EXPORTATIONS
# ==========================================================================

mod_rapports_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Production de livrables pour les décideurs : rapport exécutif PDF, fiche de diagnostic territorial, classeurs Excel, fichiers CSV, graphiques et cartes. Les filtres ci-dessous s'appliquent à tous les exports ; chaque fichier porte la mention de la nature des données."),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(4, 3, 5)),
          sliderInput(ns("periode"), "Période", min(ANNEES), max(ANNEES), c(min(ANNEES), max(ANNEES)), sep = "", step = 1),
          selectInput(ns("cycle"), "Cycle", c("Tous cycles" = "TOUS", setNames(names(CYCLES), CYCLES)), "PRIMAIRE", selectize = FALSE),
          selectizeInput(ns("territoire"), "Territoire", choices = NULL))),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, xl = 4),
      carte("Rapport exécutif (PDF)", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", "3 pages : indicateurs clés, messages, classement IMDE, carte, priorités TOPSIS, déficits et, si elle a été calculée, l'allocation optimale."),
            downloadButton(ns("dl_executif"), "Télécharger le rapport exécutif", class = "btn-primary")),
      carte("Fiche de diagnostic territorial (PDF)", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", "Pour la province choisie : profil, comparaison au niveau national, dimensions de l'IMDE, évolution et recommandations."),
            selectizeInput(ns("province_fiche"), NULL, choices = NULL, width = "100%"),
            downloadButton(ns("dl_fiche"), "Télécharger la fiche", class = "btn-primary")),
      carte("Classeur Excel", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", "Indicateurs, déficits, IMDE, priorités, allocation optimale et scénarios (selon les analyses réalisées)."),
            downloadButton(ns("dl_excel"), "Télécharger le classeur", class = "btn-primary")),
      carte("Fichier CSV", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", "Indicateurs par province et par année (séparateur « ; », décimales « , »)."),
            downloadButton(ns("dl_csv"), "Télécharger le CSV", class = "btn-light")),
      carte("Graphique (PNG)", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", "Classement IMDE haute résolution, prêt à insérer dans une présentation."),
            downloadButton(ns("dl_graph"), "Télécharger le graphique", class = "btn-light")),
      carte("Carte (PNG)", plein_ecran = FALSE,
            p(style = "font-size:14px;color:#5A6B80", paste0("Carte de l'IMDE par province. ", libelle_limites())),
            downloadButton(ns("dl_carte"), "Télécharger la carte", class = "btn-light"))),
    carte("Aperçu des messages clés du rapport exécutif", uiOutput(ns("apercu"))),
    uiOutput(ns("mention"))
  )
}

mod_rapports_server <- function(id, app) moduleServer(id, function(input, output, session) {

  observe({
    p <- app$D()$ind$province; prov <- unique(p[, c("code_geo", "nom", "code_region")])
    regs <- unique(prov$code_region)
    updateSelectizeInput(session, "territoire", server = TRUE,
                         choices = c("Périmètre complet" = "TOUT", setNames(paste0("R:", regs), paste("Région :", REGIONS[regs]))))
    updateSelectizeInput(session, "province_fiche", server = TRUE, choices = setNames(prov$code_geo, prov$nom))
  })

  # Données filtrées pour les exports
  filtre <- reactive({
    D <- app$D(); t <- input$territoire %||% "TOUT"
    provs <- unique(D$ind$province$code_geo)
    if (startsWith(t, "R:")) provs <- unique(D$ind$province$code_geo[D$ind$province$code_region == sub("R:", "", t)])
    list(provinces = provs, cycle = input$cycle, annees = seq(input$periode[1], input$periode[2]),
         libelle = if (t == "TOUT") libelle_perimetre(app$utilisateur(), DONNEES$d$geo) else REGIONS[[sub("R:", "", t)]])
  })

  contexte <- reactive({
    D <- app$D(); f <- filtre(); an <- max(f$annees); cy <- f$cycle; meta <- D$d$meta
    im <- D$imde[[cy]]; im <- im[im$annee_debut == an & im$code_geo %in% f$provinces, ]
    p <- panel_cycle(D, cy); p <- p[p$code_geo %in% f$provinces, ]
    n <- agreger(p[p$annee_debut == an, ], "national", D$d$normes); n <- n[1, ]
    reg <- agreger(p[p$annee_debut == an, ], "region", D$d$normes)
    pri <- tryCatch(prioriser(matrice_criteres(p, im, annee = an)), error = function(e) NULL)
    crit <- im[im$classe_imde %in% "Critique", ]
    messages <- c(
      sprintf("%s élèves (%s) dans %s établissements%s ; %s enseignants et %s salles de classe.",
              fmt_nombre(n$eff_t), tolower(libelle_cycle(cy)), fmt_nombre(n$nb_etab_t),
              if (is.na(n$nb_etab_fermes)) "" else paste0(", dont ", fmt_nombre(n$nb_etab_fermes), " fermés"),
              fmt_nombre(n$nb_ens_t), fmt_nombre(n$nb_salles_t)),
      sprintf("IMDE moyen pondéré : %s / 100 ; %d province(s) en situation critique%s.", fmt_nombre(moy_ponderee(im$imde, im$eff_t), 1),
              nrow(crit), if (nrow(crit)) paste0(" (", if (nrow(crit) > 4) "dont " else "", paste(head(crit$nom[order(-crit$imde)], 4), collapse = ", "), ")") else ""),
      {
        d <- c(salles = "salles de classe", enseignants = "enseignants", tables_bancs = "tables-bancs", eau = "établissements sans point d'eau")
        v <- vapply(paste0("deficit_", names(d)), function(k) n[[k]] %||% NA_real_, numeric(1))
        if (any(!is.na(v))) paste0("Déficits estimés : ", paste(fmt_nombre(v[!is.na(v)]), d[!is.na(v)], collapse = ", "),
                                   if (any(is.na(v))) paste0(" (non estimables faute de données : ", paste(d[is.na(v)], collapse = ", "), ")") else "", ".")
        else NA_character_
      },
      sprintf("Ratios moyens : %s élèves par salle utilisable et %s élèves par enseignant ; inégalité de l'IMDE entre provinces (Gini) : %s.",
              fmt_nombre(n$ratio_eleves_salle_utilisable, 1), fmt_nombre(n$ratio_eleves_ens, 1), fmt_nombre(gini(im$imde), 2)),
      if (!is.null(pri)) sprintf("Priorités TOPSIS : %s.", paste(head(pri$nom, 5), collapse = ", ")))
    o <- app$resultats$optimisation
    optim <- if (!is.null(o$opt$par_ressource)) with(o$opt$par_ressource, data.frame(
      Ressource = libelle, `Déficit` = fmt_nombre(deficit_initial), `Alloué` = fmt_nombre(alloue),
      Couverture = fmt_pct(couverture, 0), `Coût` = fmt_fcfa(cout), check.names = FALSE))
    if (!is.null(o$opt$cout_total)) messages <- c(messages, sprintf("Allocation optimale calculée : %s engagés, IMDE moyen ramené de %s à %s.",
                                                                    fmt_fcfa(o$opt$cout_total), fmt_nombre(moy_ponderee(o$avant$imde, o$avant$eff_t), 1),
                                                                    fmt_nombre(moy_ponderee(o$apres$imde, o$avant$eff_t), 1)))
    g <- D$d$geometries$provinces
    list(titre = sprintf("Situation éducative : %s", f$libelle),
         sous_titre = sprintf("%s, année scolaire %s, période analysée %s-%s", libelle_cycle(cy), annee_scolaire(an), min(f$annees), max(f$annees) + 1),
         mention = meta$mention,
         kpis = c("Élèves" = fmt_compact(n$eff_t), "Élèves / enseignant" = fmt_nombre(n$ratio_eleves_ens, 1),
                  "Élèves / salle" = fmt_nombre(n$ratio_eleves_salle_utilisable, 1), "IMDE moyen" = fmt_nombre(moy_ponderee(im$imde, im$eff_t), 1),
                  "Couverture eau" = fmt_pct(n$taux_eau, 0)),
         messages = messages[!is.na(messages)],
         graph_classement = gg_classement_imde(im, "Classement des provinces par IMDE", 15, meta$mention),
         carte = if (!is.null(g)) gg_carte(joindre_geo(g[g$code_geo %in% f$provinces, ], im[, c("code_geo", "imde")]), "imde",
                                           "IMDE par province", meta$mention),
         graph_deficits = gg_deficits_regions(reg, meta$mention),
         priorites = if (!is.null(pri)) { h <- head(pri, 12); data.frame(Rang = h$rang_national, Province = h$nom,
                                                                       `Région` = REGIONS[h$code_region], Score = fmt_nombre(h$score, 3),
                                                                       `Critère principal` = substr(h$critere_principal, 1, 34), check.names = FALSE) },
         tableau_deficits = data.frame(Ressource = DEFICITS$libelle,
                                       `Unités` = fmt_nombre(vapply(DEFICITS$code, function(v) n[[v]] %||% NA_real_, numeric(1))),
                                       `Coût indicatif` = fmt_fcfa(vapply(seq_len(nrow(DEFICITS)), function(i)
                                         (n[[DEFICITS$code[i]]] %||% NA) * COUTS_DEFAUT$cout_unitaire[COUTS_DEFAUT$ressource == DEFICITS$ressource[i]], numeric(1))),
                                       check.names = FALSE),
         optim = optim, im = im, p = p, pri = pri)
  })

  output$apercu <- renderUI({
    c <- contexte()
    tagList(h5(c$titre, style = "font-weight:750"), p(style = "color:#5A6B80", c$sous_titre),
            tags$ul(style = "font-size:13.5px;line-height:1.7", lapply(c$messages, tags$li)))
  })

  nom_fichier <- function(base, ext) sprintf("EDU-OPTIM_%s_%s.%s", base, format(Sys.time(), "%Y%m%d_%H%M"), ext)
  sauver_copie <- function(file, nom) try(file.copy(file, file.path("resultats/rapports", nom), overwrite = TRUE), silent = TRUE)

  output$dl_executif <- downloadHandler(
    filename = function() nom_fichier("rapport_executif", "pdf"),
    content = function(file) {
      withProgress(message = "Composition du rapport PDF…", value = 0.5, {
        tryCatch(rapport_executif_pdf(file, contexte()),
                 error = function(e) { showNotification(paste("Rapport :", conditionMessage(e)), type = "error"); stop(e) })
      })
      sauver_copie(file, nom_fichier("rapport_executif", "pdf"))
    })

  output$dl_fiche <- downloadHandler(
    filename = function() nom_fichier(paste0("fiche_", input$province_fiche), "pdf"),
    content = function(file) {
      D <- app$D(); cy <- input$cycle; code <- input$province_fiche; req(code)
      im <- D$imde[[cy]]; serie <- im[im$code_geo == code, ]; serie <- serie[order(serie$annee_debut), ]
      x <- serie[nrow(serie), ]
      nat <- D$ind$national; nat <- nat[as.character(nat$cycle) == cy & nat$annee_debut == x$annee_debut, ]
      dims <- names(DIMENSIONS_IMDE)
      dd <- data.frame(dimension = dims, score = unlist(x[paste0("dim_", dims)]))
      f <- list(
        nom = x$nom, mention = D$d$meta$mention,
        sous_titre = sprintf("Province de la région %s, %s, %s, rang IMDE %s sur %d", REGIONS[[x$code_region]], libelle_cycle(cy),
                             annee_scolaire(x$annee_debut), x$rang, sum(im$annee_debut == x$annee_debut)),
        kpis = c("IMDE" = fmt_nombre(x$imde, 1), "Classe" = as.character(x$classe_imde), "Élèves" = fmt_compact(x$eff_t),
                 "Élèves/salle" = fmt_nombre(x$ratio_eleves_salle_utilisable, 1), "Élèves/ens." = fmt_nombre(x$ratio_eleves_ens, 1)),
        comparaison = data.frame(Indicateur = c("Élèves par salle utilisable", "Élèves par enseignant", "TBS (%)", "Couverture en eau",
                                                "Couverture en électricité", "Taux de fonctionnalité"),
                                 Province = c(fmt_nombre(x$ratio_eleves_salle_utilisable, 1), fmt_nombre(x$ratio_eleves_ens, 1), fmt_nombre(x$tbs, 1),
                                              fmt_pct(x$taux_eau, 0), fmt_pct(x$taux_electricite, 0), fmt_pct(x$taux_fonctionnalite, 0)),
                                 `Référence` = c(fmt_nombre(nat$ratio_eleves_salle_utilisable, 1), fmt_nombre(nat$ratio_eleves_ens, 1), fmt_nombre(nat$tbs, 1),
                                                 fmt_pct(nat$taux_eau, 0), fmt_pct(nat$taux_electricite, 0), fmt_pct(nat$taux_fonctionnalite, 0)),
                                 `Écart` = c(fmt_nombre(x$ratio_eleves_salle_utilisable - nat$ratio_eleves_salle_utilisable, 1),
                                             fmt_nombre(x$ratio_eleves_ens - nat$ratio_eleves_ens, 1), fmt_nombre(x$tbs - nat$tbs, 1),
                                             fmt_nombre(100 * (x$taux_eau - nat$taux_eau), 1), fmt_nombre(100 * (x$taux_electricite - nat$taux_electricite), 1),
                                             fmt_nombre(100 * (x$taux_fonctionnalite - nat$taux_fonctionnalite), 1)), check.names = FALSE),
        recommandations = recommandations(x),
        graph_dimensions = ggplot2::ggplot(dd, ggplot2::aes(stats::reorder(dimension, score), score, fill = score)) +
          ggplot2::geom_col(width = 0.65) + ggplot2::coord_flip() +
          ggplot2::scale_fill_gradientn(colours = PALETTE_DEFICIT, limits = c(0, 100), guide = "none") +
          ggplot2::scale_y_continuous(limits = c(0, 100)) +
          ggplot2::labs(title = "Scores par dimension de l'IMDE", x = NULL, y = "Score de déficit (0-100)") + theme_edu(),
        graph_evolution = ggplot2::ggplot(serie, ggplot2::aes(annee_debut, eff_t)) +
          ggplot2::geom_line(colour = COULEURS$petrole, linewidth = 1.2) + ggplot2::geom_point(colour = COULEURS$petrole, size = 2) +
          ggplot2::scale_y_continuous(labels = function(v) fmt_nombre(v)) +
          ggplot2::scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = 2)) +
          ggplot2::labs(title = "Évolution des effectifs", x = NULL, y = "Élèves", caption = D$d$meta$mention) + theme_edu(),
        graph_ratios = ggplot2::ggplot(serie, ggplot2::aes(annee_debut)) +
          ggplot2::geom_line(ggplot2::aes(y = ratio_eleves_salle_utilisable, colour = "Élèves / salle utilisable"), linewidth = 1.1) +
          ggplot2::geom_line(ggplot2::aes(y = ratio_eleves_ens, colour = "Élèves / enseignant"), linewidth = 1.1) +
          ggplot2::geom_line(ggplot2::aes(y = imde, colour = "IMDE"), linewidth = 1.1, linetype = "dashed") +
          ggplot2::scale_colour_manual(values = c("Élèves / salle utilisable" = COULEURS$petrole, "Élèves / enseignant" = COULEURS$orange,
                                                  "IMDE" = COULEURS$rouge), name = NULL) +
          ggplot2::scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = 2)) +
          ggplot2::labs(title = "Ratios et IMDE", x = NULL, y = NULL) + theme_edu())
      withProgress(message = "Composition de la fiche…", value = 0.5, fiche_territoriale_pdf(file, f))
      sauver_copie(file, nom_fichier(paste0("fiche_", code), "pdf"))
    })

  output$dl_excel <- downloadHandler(
    filename = function() nom_fichier("export", "xlsx"),
    content = function(file) {
      c <- contexte(); f <- filtre(); D <- app$D()
      p <- c$p[c$p$annee_debut %in% f$annees, ]
      cols <- intersect(c("annee_debut", "code_region", "code_geo", "nom", "cycle", "eff_t", "nb_etab_t", "nb_etab_fermes", "nb_salles_t",
                          "nb_salles_bon_etat", "nb_ens_t", "ratio_eleves_salle_utilisable", "ratio_eleves_ens", "tbs", "taux_eau",
                          "taux_electricite", "taux_latrines", "taux_cantine", COLONNES_DEFICIT), names(p))
      im <- D$imde[[f$cycle]]; im <- im[im$annee_debut %in% f$annees & im$code_geo %in% f$provinces, ]
      fe <- list(Indicateurs = p[, cols],
                 IMDE = im[, c("annee_debut", "code_geo", "nom", "imde", "classe_imde", "rang", grep("^dim_", names(im), value = TRUE))],
                 Priorites = c$pri, Normes = D$d$normes)
      o <- app$resultats$optimisation
      if (!is.null(o$opt$allocation)) { fe$Optimisation_synthese <- o$opt$par_ressource; fe$Optimisation_detail <- o$opt$allocation }
      s <- app$resultats$simulations
      if (length(s)) fe$Scenarios <- do.call(rbind, lapply(s, `[[`, "resume"))
      exporter_excel(fe, file, D$d$meta$mention, list(`Périmètre` = f$libelle, Cycle = libelle_cycle(f$cycle),
                                                      `Période` = paste(min(f$annees), max(f$annees), sep = "–")))
      sauver_copie(file, nom_fichier("export", "xlsx"))
    })

  output$dl_csv <- downloadHandler(
    filename = function() nom_fichier("indicateurs", "csv"),
    content = function(file) {
      c <- contexte(); f <- filtre()
      p <- c$p[c$p$annee_debut %in% f$annees, ]
      p$mention <- app$D()$d$meta$mention
      exporter_csv(p[, setdiff(names(p), c("id_ligne", "qualite", "sources", "remarque"))], file)
    })

  output$dl_graph <- downloadHandler(
    filename = function() nom_fichier("classement_imde", "png"),
    content = function(file) ggplot2::ggsave(file, contexte()$graph_classement, width = 9, height = 7, dpi = 200, bg = "white"))

  output$dl_carte <- downloadHandler(
    filename = function() nom_fichier("carte_imde", "png"),
    content = function(file) {
      cc <- contexte()$carte
      if (is.null(cc)) stop("Aucune géométrie disponible.")
      ggplot2::ggsave(file, cc, width = 8, height = 7, dpi = 200, bg = "white")
    })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
