# ==========================================================================
# ui.R — Structure de l'interface (barre latérale, en-tête, pages)
# ==========================================================================

theme_edu_optim <- bs_theme(
  version = 5,
  bg = "#F4F6F9", fg = "#142235",
  primary = "#0E6E7E", secondary = "#5A6B80", success = "#2E9E6B",
  info = "#1FB5A8", warning = "#E08A2E", danger = "#D1495B",
  base_font = font_collection("Segoe UI Variable Text", "Segoe UI", "system-ui", "-apple-system",
                              "Helvetica Neue", "Roboto", "Arial", "sans-serif"),
  heading_font = font_collection("Segoe UI Variable Display", "Segoe UI", "system-ui", "Arial", "sans-serif"),
  code_font = font_collection("Cascadia Mono", "Consolas", "Courier New", "monospace"),
  "font-size-base" = "0.875rem", "border-radius" = "0.6rem", "card-border-radius" = "14px",
  "input-border-color" = "#E4E9F0", "link-color" = "#0E6E7E"
)

navigation <- function() {
  tagList(lapply(unique(PAGES$section), function(s) {
    tagList(div(class = "nav-section", s),
            lapply(which(PAGES$section == s), function(i) {
              tags$a(class = paste("nav-lien", if (i == 1) "actif"), href = "#", role = "link", tabindex = "0",
                     `data-page` = PAGES$id[i], title = PAGES$titre[i], icon(PAGES$icone[i]), span(PAGES$court[i]))
            }))
  }))
}

pages <- lapply(seq_len(nrow(PAGES)), function(i) {
  nav_panel_hidden(PAGES$id[i], div(class = "page-entree", get(paste0("mod_", PAGES$id[i], "_ui"))(PAGES$id[i])))
})

ui <- page_sidebar(
  window_title = "EDU-OPTIM BURKINA",
  theme = theme_edu_optim,
  fillable = FALSE,
  sidebar = sidebar(
    width = 258, bg = "#0B1A2E", fg = "#C9D4E3",
    open = list(desktop = "open", mobile = "closed"),
    div(class = "logo",
        div(class = "logo-marque", "EO"),
        div(class = "logo-texte", div(class = "l1", HTML("EDU<span>-OPTIM</span>")), div(class = "l2", "BURKINA"))),
    navigation(),
    div(class = "sidebar-pied",
        tags$b("Plateforme d'aide à la décision"), br(),
        sprintf("Version %s, %s", CONFIG$application$version,
                if (EST_SYNTHETIQUE) "données synthétiques" else "données de collecte"))
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "css/edu-optim.css"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "images/favicon.svg"),
    tags$script(src = "js/edu-optim.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  div(class = paste("bandeau-synth", if (!EST_SYNTHETIQUE) "reel"), role = "note", DONNEES$d$meta$mention),
  div(class = "entete",
      div(div(class = "fil", textOutput("fil", inline = TRUE)), h1(textOutput("titre_page", inline = TRUE))),
      div(class = "outils",
          div(class = "filtre-global", tags$label(`for` = "annee", "Année"),
              selectInput("annee", NULL, choices = setNames(rev(ANNEES), annee_scolaire(rev(ANNEES))),
                          width = "128px", selectize = FALSE)),
          div(class = "filtre-global", tags$label(`for` = "cycle", "Cycle"),
              selectInput("cycle", NULL, choices = c("Tous cycles" = "TOUS", setNames(names(CYCLES), CYCLES)),
                          selected = "PRIMAIRE", width = "142px", selectize = FALSE)),
          uiOutput("puce_utilisateur", inline = TRUE))),
  div(class = "contenu", do.call(navset_hidden, c(pages, list(id = "pages"))))
)
