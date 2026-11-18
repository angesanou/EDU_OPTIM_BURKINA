# ==========================================================================
# MODULE 13 — DOCUMENTATION ET MÉTHODOLOGIE
# ==========================================================================
# Affiche les fichiers Markdown du dossier documentation/ (rendus localement,
# sans Internet) et les catalogues générés à partir du code lui-même.

DOCS <- c("Guide d'utilisation" = "guide_utilisation.md",
          "Méthodologie scientifique" = "methodologie_scientifique.md",
          "Documentation technique" = "documentation_technique.md",
          "Remplacer par des données réelles" = "remplacer_donnees.md",
          "Installation et exécution" = "installation.md")

md_en_html <- function(fichier) {
  if (!file.exists(fichier)) return(p("Fichier introuvable : ", tags$code(fichier)))
  lignes <- readLines(fichier, encoding = "UTF-8", warn = FALSE)
  # Lignes « $$ ... $$ » : formules écrites en HTML (fichiers de l'application), affichées comme eq()
  formules <- character(0)
  for (k in grep("^\\$\\$.*\\$\\$\\s*$", lignes)) {
    formules <- c(formules, sub("^\\$\\$(.*)\\$\\$\\s*$", "\\1", lignes[k]))
    lignes[k] <- sprintf("EQFORMULE%03d", length(formules))
  }
  txt <- paste(lignes, collapse = "\n")
  html <- if (requireNamespace("commonmark", quietly = TRUE)) commonmark::markdown_html(txt, extensions = TRUE)
          else if (requireNamespace("markdown", quietly = TRUE)) markdown::markdownToHTML(text = txt, fragment.only = TRUE)
          else paste0("<pre>", htmltools::htmlEscape(txt), "</pre>")
  for (k in seq_along(formules))
    html <- sub(sprintf("<p>EQFORMULE%03d</p>", k), sprintf("<div class=\"eq\">%s</div>", formules[k]), html, fixed = TRUE)
  div(class = "doc-md", HTML(html))
}

mod_documentation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Définitions, formules, méthodes, hypothèses et limites. Les catalogues d'indicateurs ci-dessous sont lus directement dans le code : ils sont toujours à jour."),
    navset_tab(
      id = ns("onglets"),
      nav_panel("Guide", div(class = "pt-3", md_en_html(file.path("documentation", DOCS[[1]])))),
      nav_panel("Méthodologie", div(class = "pt-3", md_en_html(file.path("documentation", DOCS[[2]])))),
      nav_panel("Indicateurs et formules", div(class = "pt-3",
        carte("Catalogue des indicateurs (IMDE)", DTOutput(ns("t_ind"))),
        carte("Déficits", DTOutput(ns("t_def"))),
        carte("Normes utilisées", DTOutput(ns("t_normes")), pied = "Feuille NORMES de la base : normes officielles, avec la source de chaque valeur."),
        carte("Coûts unitaires utilisés", DTOutput(ns("t_couts"))))),
      nav_panel("Technique", div(class = "pt-3", md_en_html(file.path("documentation", DOCS[[3]])))),
      nav_panel("Données réelles", div(class = "pt-3", md_en_html(file.path("documentation", DOCS[[4]])))),
      nav_panel("Installation", div(class = "pt-3", md_en_html(file.path("documentation", DOCS[[5]])))),
      nav_panel("Versions", div(class = "pt-3", uiOutput(ns("versions"))))
    )
  )
}

mod_documentation_server <- function(id, app) moduleServer(id, function(input, output, session) {
  output$t_ind <- renderDT({
    c <- CATALOGUE_INDICATEURS
    tableau(data.frame(Code = c$code, `Libellé` = c$libelle, Dimension = c$dimension, Formule = c$formule,
                       Sens = ifelse(c$sens > 0, "↑ = déficit", "↑ = favorable"), `Unité` = c$unite, check.names = FALSE), page = 20)
  })
  output$t_def <- renderDT(tableau(data.frame(Code = DEFICITS$code, Ressource = DEFICITS$libelle, Formule = DEFICITS$formule), boutons = FALSE))
  output$t_normes <- renderDT({
    n <- app$D()$d$normes
    tableau(data.frame(`Paramètre` = n$code_parametre, Cycle = n$cycle, Valeur = n$valeur, Source = n$source, check.names = FALSE), page = 20)
  })
  output$t_couts <- renderDT(tableau(data.frame(Ressource = COUTS_DEFAUT$libelle, `Unité` = COUTS_DEFAUT$unite,
                                                `Coût unitaire (FCFA)` = fmt_nombre(COUTS_DEFAUT$cout_unitaire),
                                                Statut = COUTS_DEFAUT$statut %||% "", Source = COUTS_DEFAUT$source %||% "", check.names = FALSE), boutons = FALSE))
  output$versions <- renderUI({
    m <- app$D()$d$meta
    pk <- c("shiny", "bslib", "plotly", "leaflet", "DT", "sf", "spdep", "plm", "forecast", "lpSolve", "openxlsx")
    tagList(
      tags$table(class = "table table-sm", style = "max-width:760px;font-size:14px",
                 tags$tbody(
                   tags$tr(tags$th("Application"), tags$td(CONFIG$application$version)),
                   tags$tr(tags$th("Source des données"), tags$td(m$type, " : ", tags$code(m$fichier))),
                   tags$tr(tags$th("Version du fichier"), tags$td(format(m$date_fichier, "%d/%m/%Y %H:%M"))),
                   tags$tr(tags$th("Empreinte des données"), tags$td(tags$code(substr(m$empreinte, 1, 16)))),
                   tags$tr(tags$th("Préparation"), tags$td(VERSION_PREPARATION)),
                   tags$tr(tags$th("Graine aléatoire"), tags$td(CONFIG$graine)),
                   tags$tr(tags$th("R"), tags$td(R.version.string)),
                   tags$tr(tags$th("Packages"), tags$td(paste(sprintf("%s %s", pk, vapply(pk, function(x) as.character(utils::packageVersion(x)), "")), collapse = ", "))))))
  })
})
