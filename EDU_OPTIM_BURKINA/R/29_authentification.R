# ==========================================================================
# 29_authentification.R — Comptes, profils et périmètres d'accès
# ==========================================================================
# Trois profils :
#   NATIONAL   : toutes les données ; peut gérer les comptes.
#   REGIONAL   : uniquement les provinces de sa région (périmètre = REGxx).
#   PROVINCIAL : uniquement sa province (périmètre = PRVxxxx).
# Les mots de passe ne sont JAMAIS stockés en clair : empreinte SHA-256 salée.
# Pour un déploiement réel (Shiny Server / Posit Connect), déléguer
# l'authentification au serveur (LDAP, SSO) et conserver ce module pour les
# AUTORISATIONS (périmètres).

FICHIER_COMPTES <- "config/utilisateurs.csv"
PROFILS <- c(NATIONAL = "National", REGIONAL = "Régional", PROVINCIAL = "Provincial")

hacher <- function(mdp, sel) digest::digest(paste0(sel, "::", mdp), algo = "sha256", serialize = FALSE)

lire_comptes <- function(fichier = FICHIER_COMPTES) {
  if (!file.exists(fichier)) return(data.frame(identifiant = character(), nom_complet = character(), profil = character(),
                                               perimetre = character(), mot_de_passe_hache = character(), sel = character()))
  utils::read.csv(fichier, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
}

ajouter_compte <- function(identifiant, nom_complet, profil, perimetre, mdp, fichier = FICHIER_COMPTES) {
  stopifnot(profil %in% names(PROFILS), nchar(mdp) >= 8, grepl("^[a-z0-9._-]{3,}$", identifiant))
  c <- lire_comptes(fichier)
  if (identifiant %in% c$identifiant) stop("Cet identifiant existe déjà.")
  sel <- paste(sample(c(letters, LETTERS, 0:9), 16, TRUE), collapse = "")
  c <- rbind(c, data.frame(identifiant = identifiant, nom_complet = nom_complet, profil = profil,
                           perimetre = perimetre, mot_de_passe_hache = hacher(mdp, sel), sel = sel))
  utils::write.csv(c, fichier, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(c)
}

#' Comptes de DÉMONSTRATION (créés si le fichier est vide). Voir README.
initialiser_comptes_demo <- function(fichier = FICHIER_COMPTES) {
  if (nrow(lire_comptes(fichier))) return(invisible(FALSE))
  set.seed(NULL)
  ajouter_compte("national", "Direction nationale (démo)", "NATIONAL", "NAT", "Demo-National-2026", fichier)
  ajouter_compte("centre-nord", "Direction régionale du Centre-Nord (démo)", "REGIONAL", "REG05", "Demo-Region-2026", fichier)
  ajouter_compte("sahel", "Direction régionale du Sahel (démo)", "REGIONAL", "REG12", "Demo-Region-2026", fichier)
  ajouter_compte("soum", "Direction provinciale du Soum (démo)", "PROVINCIAL", "PRV1203", "Demo-Province-2026", fichier)
  invisible(TRUE)
}

verifier_identifiants <- function(identifiant, mdp, comptes = lire_comptes()) {
  u <- comptes[comptes$identifiant == trimws(tolower(identifiant)), ]
  if (nrow(u) != 1) return(NULL)
  if (!identical(hacher(mdp, u$sel), u$mot_de_passe_hache)) return(NULL)
  as.list(u[, c("identifiant", "nom_complet", "profil", "perimetre")])
}

#' Codes des provinces autorisées (NULL = tout le pays).
provinces_autorisees <- function(u, geo) {
  if (is.null(u) || u$profil == "NATIONAL") return(NULL)
  prov <- geo[geo$niveau_geo == "PROVINCE", ]
  if (u$profil == "REGIONAL") return(prov$code_geo[prov$code_parent == u$perimetre])
  u$perimetre
}

libelle_perimetre <- function(u, geo) {
  if (is.null(u) || u$profil == "NATIONAL") return("Burkina Faso (national)")
  geo$nom_geo[match(u$perimetre, geo$code_geo)]
}

UTILISATEUR_ANONYME <- list(identifiant = "invite", nom_complet = "Accès libre (authentification désactivée)",
                            profil = "NATIONAL", perimetre = "NAT")

#' Fenêtre de connexion (modale non fermable).
modal_connexion <- function() {
  shiny::modalDialog(
    size = "l", easyClose = FALSE, footer = NULL,
    div(class = "ecran-connexion",
        div(class = "gauche",
            div(div(class = "logo", style = "padding:0 0 18px",
                    div(class = "logo-marque", "EO"),
                    div(class = "logo-texte", div(class = "l1", HTML("EDU<span>-OPTIM</span>")), div(class = "l2", "BURKINA"))),
                h3("Piloter l'éducation par la donnée"),
                p("Diagnostic territorial, indice de déficit, prévisions, priorisation et allocation optimale des ressources éducatives.")),
            p(style = "font-size:12.5px;color:#9FB8CC",
              if (exists("DONNEES") && !is.null(DONNEES$d$meta$mention)) toupper(DONNEES$d$meta$mention)
              else "DONNÉES SYNTHÉTIQUES, ENVIRONNEMENT DE DÉMONSTRATION, AUCUNE STATISTIQUE OFFICIELLE")),
        div(class = "droite",
            h4("Connexion", style = "font-weight:750"),
            p(style = "color:#5A6B80;font-size:13px", "Votre profil détermine les territoires accessibles."),
            shiny::textInput("connexion_id", "Identifiant", placeholder = "ex. national", width = "100%"),
            shiny::passwordInput("connexion_mdp", "Mot de passe", width = "100%"),
            shiny::uiOutput("connexion_msg"),
            shiny::actionButton("connexion_valider", "Se connecter", class = "btn-primary w-100", icon = shiny::icon("arrow-right-to-bracket")),
            div(class = "comptes-demo",
                tags$b("Comptes de démonstration"), br(),
                "National : ", tags$code("national"), " / ", tags$code("Demo-National-2026"), br(),
                "Régional : ", tags$code("centre-nord"), " ou ", tags$code("sahel"), " / ", tags$code("Demo-Region-2026"), br(),
                "Provincial : ", tags$code("soum"), " / ", tags$code("Demo-Province-2026")),
            tags$script(HTML("$(document).on('keyup', '#connexion_mdp, #connexion_id', function(e){ if(e.key==='Enter'){ $('#connexion_valider').click(); } });"))))
  )
}
