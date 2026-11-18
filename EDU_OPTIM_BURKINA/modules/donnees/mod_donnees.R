# ==========================================================================
# MODULE 3 — DONNÉES ET SOURCES
# ==========================================================================

mod_donnees_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Inventaire des jeux de données chargés, contrôles de qualité automatiques, valeurs manquantes, historique des traitements et dictionnaire des variables. L'application lit une base au format EXX_01 : la même interface sert aux données synthétiques et aux données réelles."),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Jeux de données", DTOutput(ns("t_jeux"))),
      carte("Remplacer par des données réelles", plein_ecran = FALSE,
            tags$ol(style = "font-size:14px;line-height:1.75;padding-left:18px;margin:0",
                    tags$li("Copier le classeur rempli au format EXX_01 dans ", tags$code("data/reelles/"), "."),
                    tags$li("Dans ", tags$code("config/config.yml"), " : ", tags$code('source_donnees: "reel"'), " et le chemin ", tags$code("base_exx"), "."),
                    tags$li("Optionnel : établissements (CSV), contexte (pauvreté…), limites administratives (.gpkg / .geojson)."),
                    tags$li("Relancer l'application : les contrôles ci-dessous s'appliquent automatiquement.")),
            uiOutput(ns("source_active")))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Contrôles de cohérence (panel province)", DTOutput(ns("t_controles"))),
      carte("Valeurs manquantes par variable", plotlyOutput(ns("g_manquants"), height = 420),
            sous_titre = "panel province, toutes années")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      carte("Contrôles des établissements", DTOutput(ns("t_etab"))),
      carte("Historique des traitements", DTOutput(ns("t_journal")))),
    carte("Explorer le panel province", DTOutput(ns("t_panel")), sous_titre = "année et cycle de l'en-tête"),
    carte("Dictionnaire des variables", DTOutput(ns("t_dico")), sous_titre = "feuille DICTIONNAIRE de la base"),
    uiOutput(ns("mention"))
  )
}

mod_donnees_server <- function(id, app) moduleServer(id, function(input, output, session) {

  jeux <- reactive({
    D <- app$D(); p <- D$ind$province; per <- paste(min(p$annee_debut), max(p$annee_debut), sep = "–")
    l <- list(profil_table(D$d$panel, "PANEL_PROVINCE (base EXX_01)", "Province × cycle × année", per))
    if (!is.null(D$d$panel_region)) l[[2]] <- profil_table(D$d$panel_region, "PANEL_REGION (base EXX_01)", "Région / national", per)
    if (!is.null(D$ind$etablissements)) l[[length(l) + 1]] <- profil_table(D$ind$etablissements, "Établissements (CSV)", "Établissement × année", per)
    if (!is.null(D$ind$commune)) l[[length(l) + 1]] <- profil_table(D$ind$commune, "Communes (agrégées)", "Commune × cycle × année", per)
    if (!is.null(D$d$contexte)) l[[length(l) + 1]] <- profil_table(D$d$contexte, "Contexte socio-économique", "Province × année", per)
    g <- D$d$geometries
    if (!is.null(g)) for (n in names(g)) if (!is.null(g[[n]])) l[[length(l) + 1]] <-
      data.frame(jeu = paste("Géométries :", n), niveau_geographique = n, observations = nrow(g[[n]]),
                 variables = ncol(g[[n]]), periode = "–", taux_manquants = NA)
    l[[length(l) + 1]] <- data.frame(jeu = "NORMES", niveau_geographique = "Paramètres", observations = nrow(D$d$normes),
                                     variables = ncol(D$d$normes), periode = "–", taux_manquants = mean(is.na(D$d$normes$valeur)))
    do.call(rbind, l)
  })

  output$kpis <- renderUI({
    j <- jeux(); q <- DONNEES$controles$panel; p <- app$D()$ind$province
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi("Jeux de données", nrow(j), icone = "database", sous_texte = "chargés et contrôlés"),
      kpi("Observations", fmt_compact(sum(j$observations)), icone = "table", ton = "violet", sous_texte = "toutes tables"),
      kpi("Contrôles conformes", sprintf("%d / %d", sum(q$statut == "Conforme"), nrow(q)), icone = "circle-check", ton = "vert",
          jauge = mean(q$statut == "Conforme"), couleur_jauge = COULEURS$vert),
      kpi("Taux de manquants", fmt_pct(mean(is.na(as.matrix(p[, intersect(VARIABLES_SAISIE, names(p))])))),
          icone = "circle-half-stroke", ton = "orange", sous_texte = "variables saisies du panel"))
  })

  output$source_active <- renderUI({
    m <- app$D()$d$meta
    alerte(if (EST_SYNTHETIQUE) "avert" else "ok",
           tags$b("Source active : "), if (EST_SYNTHETIQUE) "données synthétiques" else "données réelles", br(),
           tags$code(m$fichier), br(), sprintf("Chargée le %s", format(m$charge_le, "%d/%m/%Y %H:%M")))
  })

  output$t_jeux <- renderDT({
    j <- jeux()
    tableau(data.frame(`Jeu de données` = j$jeu, Niveau = j$niveau_geographique, Observations = fmt_nombre(j$observations),
                       Variables = j$variables, `Période` = j$periode, Manquants = fmt_pct(j$taux_manquants), check.names = FALSE),
            boutons = FALSE)
  })

  output$t_controles <- renderDT({
    q <- DONNEES$controles$panel
    tableau(data.frame(`Contrôle` = q$controle, Anomalies = q$anomalies,
                       Statut = sprintf('<span class="statut-%s">%s</span>', q$statut, q$statut), `Détail` = q$detail,
                       check.names = FALSE), escape = FALSE, page = 15)
  })

  output$g_manquants <- renderPlotly({
    m <- manquants_par_variable(app$D()$d$panel)
    m <- m[m$taux > 0, ]; m <- m[order(m$taux), ]
    exiger(nrow(m) > 0, "Aucune valeur manquante.")
    plot_ly(m, x = ~100 * taux, y = ~factor(variable, levels = variable), type = "bar", orientation = "h",
            marker = list(color = ifelse(m$taux > 0.2, COULEURS$rouge, ifelse(m$taux > 0.05, COULEURS$orange, COULEURS$petrole))),
            hovertemplate = "%{y}<br>%{x:.1f} % manquant<extra></extra>") |>
      plotly_edu(titre_x = "% de valeurs manquantes", legende = FALSE)
  })

  output$t_etab <- renderDT({
    q <- DONNEES$controles$etablissements
    exiger(!is.null(q), "Pas de fichier d'établissements pour cette source.")
    tableau(data.frame(`Contrôle` = q$controle, Anomalies = q$anomalies,
                       Statut = sprintf('<span class="statut-%s">%s</span>', q$statut, q$statut), `Détail` = q$detail,
                       check.names = FALSE), escape = FALSE, boutons = FALSE)
  })

  output$t_journal <- renderDT({
    j <- rbind(DONNEES$journal, JOURNAL$entrees)
    j <- j[!duplicated(j), ]
    tableau(j[rev(seq_len(nrow(j))), ], boutons = FALSE)
  })

  output$t_panel <- renderDT({
    f <- app$filtres(); p <- app$D()$d$panel
    p <- p[p$annee_debut == f$annee & (f$cycle == "TOUS" | as.character(p$cycle) == f$cycle), ]
    p <- p[p$code_geo %in% app$D()$ind$province$code_geo, ]
    cols <- intersect(c("id_ligne", "nom_region", "nom_province", "eff_t", "nb_etab_t", "nb_salles_t", "nb_salles_bon_etat",
                        "nb_ens_t", "nb_etab_eau", "nb_etab_electricite", "nb_tables_bancs", "pop_age_scolaire", "tbs_t", "qualite"), names(p))
    tableau(p[, cols], page = 10, filtre_colonnes = TRUE)
  })

  output$t_dico <- renderDT({
    d <- app$D()$d$dictionnaire
    exiger(!is.null(d), "Dictionnaire absent de la base.")
    cols <- intersect(c("variable", "libellé", "bloc", "type", "unité", "définition", "formule / origine"), names(d))
    tableau(d[!is.na(d$variable), cols], page = 10)
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
