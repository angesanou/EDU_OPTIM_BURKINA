# ==========================================================================
# 01_generer_donnees_synthetiques.R
# --------------------------------------------------------------------------
# Génère un système éducatif ENTIÈREMENT SYNTHÉTIQUE, cohérent et
# reproductible (graine fixe), puis l'écrit au format EXX_01.
#
#   DONNÉES SYNTHÉTIQUES — ENVIRONNEMENT DE DÉMONSTRATION —
#   AUCUNE STATISTIQUE OFFICIELLE
#
# Logique « du bas vers le haut » :
#   1. Profils territoriaux (pauvreté, développement, insécurité) par province.
#   2. Géométries APPROXIMATIVES (Voronoï autour de chefs-lieux approchés).
#   3. Démographie par province et par âge scolaire, 2015-2024.
#   4. Établissements (création, statut, milieu, taille, fermetures).
#   5. Panel établissement × année (effectifs, salles, enseignants, équipements).
#   6. Agrégation province / région / national -> classeur EXX_01.
#
# Exécution (depuis la racine du projet, dans RStudio) :
#   source("scripts/01_generer_donnees_synthetiques.R", encoding = "UTF-8")
# Durée : 1 à 2 minutes. Résultats dans data/synthetique/.
# ==========================================================================

suppressPackageStartupMessages({ library(sf); library(openxlsx) })
for (f in c("R/00_config.R", "R/01_utils.R", "R/10_chargement.R")) source(f, encoding = "UTF-8")
cfg <- lire_config()
set.seed(cfg$graine)
sf::sf_use_s2(FALSE)
ANNEES_GEN <- 2015:2024
SORTIE <- "data/synthetique"
dir.create(SORTIE, showWarnings = FALSE, recursive = TRUE)
MODELE <- "data/modele/EXX_01_modele_structure.xlsx"
if (!file.exists(MODELE)) stop("Modèle EXX_01 introuvable : ", MODELE,
                               "\nCopiez EXX_01-collecte_D3_architect_structure-base-donnees_v1.xlsx sous ce nom.")
message("1/6 Profils territoriaux")

# --------------------------------------------------------------------------
# 1. Référentiel géographique (repris du modèle EXX_01 : codes PRV/REG)
# --------------------------------------------------------------------------
geo <- lire_feuille(MODELE, "DIM_GEO", "code_geo")
prov <- geo[geo$niveau_geo == "PROVINCE", c("code_geo", "nom_geo", "code_parent")]
names(prov) <- c("code_province", "province", "code_region")

# Chefs-lieux APPROCHÉS (longitude, latitude) et population 2015 FICTIVE (milliers).
# Servent seulement à dessiner des polygones de Voronoï approximatifs et à
# donner des ordres de grandeur ; ce ne sont ni des limites ni des chiffres réels.
coords <- utils::read.table(header = TRUE, sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE, text = "
province;lon;lat;pop_2015_k
Balé;-2.93;11.75;250
Banwa;-4.08;12.18;330
Kossi;-3.86;12.73;330
Mouhoun;-3.46;12.46;360
Nayala;-2.89;12.76;200
Sourou;-3.07;13.07;260
Comoé;-4.76;10.63;480
Léraba;-5.17;10.67;150
Kadiogo;-1.53;12.37;2600
Boulgou;-0.37;11.78;650
Koulpélogo;0.05;11.50;310
Kouritenga;-0.35;12.18;400
Bam;-1.53;13.33;340
Namentenga;-0.57;12.66;400
Sanmatenga;-1.08;13.09;720
Boulkiemdé;-2.25;12.20;600
Sanguié;-2.75;12.20;370
Sissili;-2.10;11.10;260
Ziro;-1.77;11.55;220
Bazèga;-1.33;12.00;290
Nahouri;-1.15;11.17;190
Zoundwéogo;-0.95;11.62;300
Gnagna;0.14;12.98;520
Gourma;0.36;12.06;400
Komondjari;0.70;12.70;100
Kompienga;0.70;11.25;110
Tapoa;1.79;12.07;420
Houet;-4.30;11.18;1300
Kénédougou;-5.00;11.40;330
Tuy;-3.52;11.50;270
Loroum;-2.07;13.77;180
Passoré;-2.26;12.96;400
Yatenga;-2.42;13.58;690
Zondoma;-2.60;13.20;210
Ganzourgou;-0.62;12.25;420
Kourwéogo;-1.89;12.66;170
Oubritenga;-1.30;12.62;300
Oudalan;-0.24;14.44;260
Séno;0.20;13.95;330
Soum;-1.63;14.10;430
Yagha;0.85;13.35;200
Bougouriba;-3.25;10.96;130
Ioba;-3.00;11.25;240
Noumbiel;-2.92;9.88;90
Poni;-3.30;10.30;330")
prov <- merge(prov, coords, by = "province", sort = FALSE)
if (nrow(prov) != 45) stop("Rapprochement des 45 provinces incomplet (", nrow(prov), ") : vérifiez l'encodage UTF-8.")
prov <- prov[order(prov$code_province), ]

# Profils régionaux SYNTHÉTIQUES : pauvreté, développement des infrastructures,
# début et intensité d'une crise sécuritaire simulée. Ils créent volontairement
# des situations contrastées (déficits élevés, modérés, favorables).
profil_reg <- utils::read.table(header = TRUE, sep = ";", text = "
code_region;pauvrete;developpement;debut_crise;intensite
REG01;0.45;0.45;2019;0.20
REG02;0.35;0.55;2021;0.04
REG03;0.10;0.92;0;0.00
REG04;0.40;0.50;2019;0.08
REG05;0.50;0.40;2019;0.38
REG06;0.33;0.62;2021;0.03
REG07;0.37;0.56;0;0.00
REG08;0.56;0.28;2018;0.45
REG09;0.28;0.72;2021;0.03
REG10;0.50;0.42;2018;0.30
REG11;0.38;0.58;0;0.00
REG12;0.66;0.18;2016;0.62
REG13;0.46;0.40;2021;0.10")
prov <- merge(prov, profil_reg, by = "code_region")
prov <- prov[order(prov$code_province), ]
n_p <- nrow(prov)
prov$pauvrete <- pmin(0.85, pmax(0.05, prov$pauvrete + stats::rnorm(n_p, 0, 0.04)))
prov$developpement <- pmin(0.97, pmax(0.08, prov$developpement + stats::rnorm(n_p, 0, 0.05)))
choc <- c(Soum = 0.25, Oudalan = 0.10, Yagha = 0.12, Komondjari = 0.25, Tapoa = 0.15,
          Loroum = 0.30, Kossi = 0.20, Sourou = 0.20, Bam = 0.08, Namentenga = 0.10,
          Koulpélogo = 0.10, Gnagna = 0.10)
prov$intensite <- pmin(0.9, prov$intensite + ifelse(prov$province %in% names(choc), choc[prov$province], 0) +
                         ifelse(prov$intensite > 0, stats::runif(n_p, -0.03, 0.03), 0))
prov$intensite[prov$intensite < 0] <- 0
prov$urbanisation <- with(prov, pmin(0.95, 0.06 + 0.25 * developpement + stats::rnorm(n_p, 0, 0.02)))
prov$urbanisation[prov$province == "Kadiogo"] <- 0.88
prov$urbanisation[prov$province == "Houet"] <- 0.52
prov$urbanisation[prov$province %in% c("Boulkiemdé", "Comoé", "Yatenga")] <- c(0.24, 0.26, 0.20)

# --------------------------------------------------------------------------
# 2. Géométries approximatives (UTM 30N pour les calculs)
# --------------------------------------------------------------------------
message("2/6 Géométries approximatives")
contour_xy <- matrix(c(
  -5.52,10.44, -5.47,11.10, -5.25,11.40, -4.80,11.95, -4.42,12.40, -4.25,13.20,
  -3.95,13.48, -3.45,13.68, -2.95,13.62, -2.65,14.10, -2.05,14.20, -1.35,14.72,
  -0.75,15.08, -0.25,15.07, 0.24,14.92, 0.24,14.45, 0.40,14.00, 0.97,13.35,
  1.02,12.85, 2.17,12.62, 2.40,12.25, 2.10,11.80, 1.45,11.45, 0.90,10.99,
  0.00,11.00, -0.44,11.10, -1.10,10.97, -2.30,10.99, -2.80,10.90, -2.70,10.35,
  -2.75,9.60, -3.10,9.85, -3.60,9.65, -4.30,9.62, -4.75,9.80, -5.00,10.15,
  -5.52,10.44), ncol = 2, byrow = TRUE)
contour <- st_sfc(st_polygon(list(contour_xy)), crs = 4326)
contour_m <- st_transform(contour, 32630)

voronoi_dans <- function(points_m, cadre_m) {
  v <- st_collection_extract(st_voronoi(st_union(points_m), envelope = st_geometry(cadre_m)[[1]]))
  v <- st_intersection(st_make_valid(st_sf(geometry = v)), st_geometry(cadre_m))
  idx <- sapply(st_intersects(points_m, v), function(i) i[1])
  st_geometry(v)[idx]
}
pts_prov <- st_transform(st_as_sf(prov, coords = c("lon", "lat"), crs = 4326), 32630)
geom_prov_m <- voronoi_dans(pts_prov, contour_m)
provinces_sf <- st_sf(code_geo = prov$code_province, nom = prov$province,
                      code_region = prov$code_region, geometry = geom_prov_m)
provinces_sf$superficie_km2 <- as.numeric(st_area(provinces_sf)) / 1e6
prov$superficie_km2 <- provinces_sf$superficie_km2

# Communes synthétiques : nombre lié à la population (≈ 350 au total)
prov$n_communes <- pmax(3, round(350 * prov$pop_2015_k / sum(prov$pop_2015_k) * 0.55 + 4))
prov$n_communes[prov$province == "Kadiogo"] <- 7
communes_l <- vector("list", n_p)
for (i in seq_len(n_p)) {
  poly <- provinces_sf[i, ]
  k <- prov$n_communes[i]
  germes <- st_sample(poly, k, type = "random", exact = TRUE)
  germes[1] <- st_geometry(pts_prov[i, ])   # la commune 01 contient le chef-lieu approché
  g <- voronoi_dans(st_sf(geometry = germes), poly)
  codes <- sprintf("COM%s%02d", substr(prov$code_province[i], 4, 7), seq_len(k))
  communes_l[[i]] <- st_sf(code_geo = codes,
                           nom = sprintf("%s – commune %02d (synth.)", prov$province[i], seq_len(k)),
                           code_province = prov$code_province[i], code_region = prov$code_region[i],
                           chef_lieu = seq_len(k) == 1, geometry = g)
}
communes_sf <- do.call(rbind, communes_l)
communes_sf$superficie_km2 <- as.numeric(st_area(communes_sf)) / 1e6
regions_sf <- do.call(rbind, lapply(split(provinces_sf, provinces_sf$code_region), function(x)
  st_sf(code_geo = x$code_region[1], nom = REGIONS[[x$code_region[1]]], geometry = st_union(x))))
for (nm in c("regions_sf", "provinces_sf", "communes_sf")) {
  obj <- get(nm); obj$approximatif <- TRUE
  obj$avertissement <- "Géométrie synthétique approximative — ne représente pas les limites officielles"
  assign(nm, obj)
}

# --------------------------------------------------------------------------
# 3. Démographie synthétique (population totale et âges scolaires)
# --------------------------------------------------------------------------
message("3/6 Démographie")
part_age <- c(PRESCOLAIRE = 0.098, PRIMAIRE = 0.172, POST_PRIMAIRE = 0.104, SECONDAIRE = 0.068)
duree <- c(PRESCOLAIRE = 3, PRIMAIRE = 6, POST_PRIMAIRE = 4, SECONDAIRE = 3)
demo <- expand.grid(code_province = prov$code_province, annee_debut = ANNEES_GEN, stringsAsFactors = FALSE)
demo <- merge(demo, prov[, c("code_province", "province", "pop_2015_k", "debut_crise", "intensite",
                             "pauvrete", "urbanisation", "developpement")], by = "code_province")
croiss <- 0.027 + 0.008 * demo$pauvrete
demo$pop_totale <- round(demo$pop_2015_k * 1000 * (1 + croiss)^(demo$annee_debut - 2015))
# Déplacements internes simulés : départs des zones en crise, accueil ailleurs
demo$anciennete_crise <- ifelse(demo$debut_crise > 0 & demo$annee_debut >= demo$debut_crise,
                                demo$annee_debut - demo$debut_crise + 1, 0)
demo$part_partis <- pmin(0.35, demo$intensite * 0.06 * demo$anciennete_crise)
accueil <- c(Sanmatenga = 0.30, Soum = 0.10, Séno = 0.12, Bam = 0.08, Kadiogo = 0.10,
             Gourma = 0.08, Yatenga = 0.07, Namentenga = 0.05, Loroum = 0.03, Gnagna = 0.04,
             Houet = 0.03)
departs <- stats::aggregate(cbind(partis = pop_totale * part_partis) ~ annee_debut, demo, sum)
demo$nb_pdi <- round(departs$partis[match(demo$annee_debut, departs$annee_debut)] *
                       ifelse(demo$province %in% names(accueil), accueil[demo$province], 0) *
                       (demo$annee_debut >= 2018) * stats::runif(nrow(demo), 0.9, 1.1))
demo$pop_totale <- round(demo$pop_totale * (1 - demo$part_partis * 0.6) + demo$nb_pdi)
demo$nb_pdi_age_scolaire <- round(demo$nb_pdi * 0.52)
demo$nb_pdi[demo$annee_debut < 2018] <- NA
demo$nb_pdi_age_scolaire[demo$annee_debut < 2018] <- NA

# --------------------------------------------------------------------------
# 4. Établissements (une ligne par établissement)
# --------------------------------------------------------------------------
message("4/6 Établissements")
tbs_base <- c(PRESCOLAIRE = 0.045, PRIMAIRE = 0.92, POST_PRIMAIRE = 0.52, SECONDAIRE = 0.20)
taille_moy <- c(PRESCOLAIRE = 70, PRIMAIRE = 205, POST_PRIMAIRE = 340, SECONDAIRE = 360)
pc <- expand.grid(code_province = prov$code_province, cycle = names(CYCLES), stringsAsFactors = FALSE)
pc <- merge(pc, prov, by = "code_province")
pc$tbs_2015 <- tbs_base[pc$cycle] * (1.25 - 0.9 * pc$pauvrete + 0.35 * pc$developpement) *
  ifelse(pc$cycle == "PRESCOLAIRE", 0.5 + 1.6 * pc$urbanisation, 1)
pc$tbs_2015 <- pmin(ifelse(pc$cycle == "PRIMAIRE", 1.08, 0.85), pc$tbs_2015)
pc$tendance <- c(PRESCOLAIRE = 0.06, PRIMAIRE = 0.005, POST_PRIMAIRE = 0.03, SECONDAIRE = 0.045)[pc$cycle]
pop_pc <- merge(demo[, c("code_province", "annee_debut", "pop_totale", "anciennete_crise")],
                pc[, c("code_province", "cycle", "tbs_2015", "tendance", "intensite")], by = "code_province")
pop_pc$pop_age_scolaire <- round(pop_pc$pop_totale * part_age[pop_pc$cycle])
pop_pc$tbs_potentiel <- pop_pc$tbs_2015 * (1 + pop_pc$tendance)^(pop_pc$annee_debut - 2015)
pop_pc$eff_potentiel <- pop_pc$pop_age_scolaire * pop_pc$tbs_potentiel

fin <- pop_pc[pop_pc$annee_debut == max(ANNEES_GEN), ]
fin$n_etab <- pmax(3, round(fin$eff_potentiel / taille_moy[fin$cycle] * ifelse(fin$cycle == "PRIMAIRE", 1, 1.05)))
etab <- fin[rep(seq_len(nrow(fin)), fin$n_etab), c("code_province", "cycle")]
etab <- merge(etab, prov[, c("code_province", "code_region", "urbanisation", "developpement",
                             "pauvrete", "intensite", "debut_crise", "n_communes")], by = "code_province")
etab <- etab[order(etab$code_province, etab$cycle), ]
n_e <- nrow(etab)
etab$id_etab <- sprintf("ETS%06d", seq_len(n_e))
p_urb <- pmin(0.95, etab$urbanisation * c(PRESCOLAIRE = 2.2, PRIMAIRE = 0.85, POST_PRIMAIRE = 1.4,
                                           SECONDAIRE = 1.9)[etab$cycle])
etab$milieu <- ifelse(stats::runif(n_e) < p_urb, "URBAIN", "RURAL")
num_com <- ifelse(etab$milieu == "URBAIN", 1L, vapply(etab$n_communes, function(k) sample.int(k, 1), integer(1)))
etab$code_commune <- sprintf("COM%s%02d", substr(etab$code_province, 4, 7), num_com)
u <- stats::runif(n_e)
p_prive <- ifelse(etab$milieu == "URBAIN", 0.45, 0.08) *
  c(PRESCOLAIRE = 1.3, PRIMAIRE = 0.75, POST_PRIMAIRE = 1.2, SECONDAIRE = 1.5)[etab$cycle]
p_comm <- ifelse(etab$cycle == "PRESCOLAIRE" & etab$milieu == "RURAL", 0.35, 0.03)
etab$statut <- ifelse(u < p_prive, "PRIVE", ifelse(u < p_prive + p_comm, "COMMUNAUTAIRE", "PUBLIC"))
etab$annee_creation <- ifelse(stats::runif(n_e) < c(PRESCOLAIRE = 0.45, PRIMAIRE = 0.78, POST_PRIMAIRE = 0.62,
                                                     SECONDAIRE = 0.50)[etab$cycle],
                              sample(1960:2014, n_e, replace = TRUE), sample(2015:2024, n_e, replace = TRUE))
etab$poids <- stats::rlnorm(n_e, 0, 0.45) * ifelse(etab$milieu == "URBAIN", 1.6, 1)
p_ferm <- etab$intensite * ifelse(etab$milieu == "RURAL", 1.15, 0.35)
etab$annee_fermeture <- ifelse(etab$debut_crise > 0 & stats::runif(n_e) < p_ferm,
                               etab$debut_crise + floor(stats::rexp(n_e, 0.45)), NA)
etab$annee_fermeture[!is.na(etab$annee_fermeture) & etab$annee_fermeture > 2024] <- NA
etab$annee_reouverture <- ifelse(!is.na(etab$annee_fermeture) & stats::runif(n_e) < 0.18,
                                 pmax(etab$annee_fermeture + 2, 2023), NA)
taille_cls_norme <- c(PRESCOLAIRE = 35, PRIMAIRE = 60, POST_PRIMAIRE = 70, SECONDAIRE = 60)
etab$taille_classe <- taille_cls_norme[etab$cycle] * (0.82 + 0.55 * etab$pauvrete) * stats::runif(n_e, 0.85, 1.25)
etab$ratio_ens <- c(PRESCOLAIRE = 29, PRIMAIRE = 54, POST_PRIMAIRE = 60, SECONDAIRE = 48)[etab$cycle] *
  (0.68 + 0.9 * etab$pauvrete) * stats::runif(n_e, 0.85, 1.2)
etab$u_eau <- stats::runif(n_e); etab$u_lat <- stats::runif(n_e); etab$u_elec <- stats::runif(n_e); etab$u_cant <- stats::runif(n_e)
etab$q_etat <- stats::rbeta(n_e, 2 + 8 * etab$developpement, 2)
etab$abris <- ifelse(stats::runif(n_e) < (0.35 * etab$pauvrete * (etab$milieu == "RURAL")), stats::runif(n_e, 0.1, 0.5), 0)
etab$couv_tb <- pmin(1.1, stats::runif(n_e, 0.45, 0.85) + 0.3 * etab$developpement)
etab$part_f <- c(PRESCOLAIRE = 0.495, PRIMAIRE = 0.485, POST_PRIMAIRE = 0.50, SECONDAIRE = 0.46)[etab$cycle] -
  0.06 * pmax(0, etab$pauvrete - 0.4) + stats::rnorm(n_e, 0, 0.015)
# Coordonnées : tirées dans la commune (resserrées autour du centre en ville)
com_idx <- match(etab$code_commune, communes_sf$code_geo)
nb_par_com <- tabulate(com_idx, nbins = nrow(communes_sf))
xy <- matrix(NA_real_, n_e, 2)
for (j in which(nb_par_com > 0))
  xy[which(com_idx == j), ] <- st_coordinates(st_sample(communes_sf[j, ], nb_par_com[j], type = "random", exact = TRUE))
centre_cl <- st_coordinates(st_centroid(st_geometry(communes_sf)))[com_idx, ]
urb <- etab$milieu == "URBAIN"
xy[urb, ] <- centre_cl[urb, ] + (xy[urb, ] - centre_cl[urb, ]) * 0.15
lonlat <- st_coordinates(st_transform(st_as_sf(as.data.frame(xy), coords = 1:2, crs = 32630), 4326))
etab$lon <- round(lonlat[, 1], 5); etab$lat <- round(lonlat[, 2], 5)

# --------------------------------------------------------------------------
# 5. Panel établissement × année
# --------------------------------------------------------------------------
message("5/6 Panel établissement x année (", n_e, " établissements)")
pa <- etab[rep(seq_len(n_e), each = length(ANNEES_GEN)), ]
pa$annee_debut <- rep(ANNEES_GEN, n_e)
pa <- pa[pa$annee_debut >= pa$annee_creation, ]
pa$ferme <- !is.na(pa$annee_fermeture) & pa$annee_debut >= pa$annee_fermeture &
  (is.na(pa$annee_reouverture) | pa$annee_debut < pa$annee_reouverture)
cle <- paste(pa$code_province, pa$cycle, pa$annee_debut)
pot <- pop_pc$eff_potentiel[match(cle, paste(pop_pc$code_province, pop_pc$cycle, pop_pc$annee_debut))]
sw <- stats::ave(pa$poids, cle, FUN = sum)
pa$eff_pot <- pot * pa$poids / sw
# Élèves des établissements fermés : 55 % se réinscrivent ailleurs, 45 % déscolarisés
aff <- stats::ave(ifelse(pa$ferme, pa$eff_pot, 0), cle, FUN = sum)
sw_ouv <- stats::ave(ifelse(pa$ferme, 0, pa$poids), cle, FUN = sum)
pa$eleves_affectes <- ifelse(pa$ferme, round(pa$eff_pot), 0)
pa$eff_t <- ifelse(pa$ferme, 0,
                   round((pa$eff_pot + 0.55 * aff * pa$poids / pmax(sw_ouv, 1e-9)) * stats::rlnorm(nrow(pa), 0, 0.06)))
pa$eff_t <- pmax(pa$eff_t, ifelse(pa$ferme, 0, 8))
pa$eff_f <- round(pa$eff_t * pmin(0.6, pmax(0.35, pa$part_f + 0.002 * (pa$annee_debut - 2015))))
pa$eff_g <- pa$eff_t - pa$eff_f
duree_v <- duree[as.character(pa$cycle)]
pa$nouv_t <- round(pa$eff_t / duree_v * stats::runif(nrow(pa), 1.05, 1.22))
pa$nouv_f <- round(pa$nouv_t * pa$eff_f / pmax(pa$eff_t, 1)); pa$nouv_g <- pa$nouv_t - pa$nouv_f
pa$eff_cm2 <- round(pa$eff_t / duree_v * stats::runif(nrow(pa), 0.72, 0.92))
pa$redoublants_cm2 <- round(pa$eff_cm2 * stats::runif(nrow(pa), 0.06, 0.20))
# Salles : la capacité suit l'effectif avec retard ; elle ne diminue jamais
salles_brut <- ifelse(pa$ferme, NA, pmax(c(PRESCOLAIRE = 1, PRIMAIRE = 3, POST_PRIMAIRE = 4,
                                            SECONDAIRE = 4)[as.character(pa$cycle)],
                                          ceiling(pa$eff_t / pa$taille_classe)))
o <- order(pa$id_etab, pa$annee_debut)
pa <- pa[o, ]; salles_brut <- salles_brut[o]
pa$nb_salles_t <- stats::ave(ifelse(is.na(salles_brut), 0, salles_brut), pa$id_etab, FUN = cummax)
pa$nb_salles_t[pa$ferme] <- 0
pa$nb_salles_abris <- round(pa$nb_salles_t * pa$abris)
vieill <- pmax(0.4, pa$q_etat - 0.012 * (pa$annee_debut - 2015))
pa$nb_salles_bon_etat <- pmin(pa$nb_salles_t - pa$nb_salles_abris, round(pa$nb_salles_t * vieill))
pa$nb_groupes_pedago <- ifelse(pa$ferme, 0, pmax(pa$nb_salles_t, ceiling(pa$eff_t / 95)))
pa$nb_ens_t <- ifelse(pa$ferme, 0, pmax(1, round(pa$eff_t / pa$ratio_ens)))
part_ens_f <- c(PRESCOLAIRE = 0.85, PRIMAIRE = 0.47, POST_PRIMAIRE = 0.25, SECONDAIRE = 0.18)[as.character(pa$cycle)]
pa$nb_ens_f <- round(pa$nb_ens_t * pmin(0.95, part_ens_f * stats::runif(nrow(pa), 0.8, 1.2)))
pa$nb_ens_g <- pa$nb_ens_t - pa$nb_ens_f
pa$nb_ens_qualifies <- round(pa$nb_ens_t * pmin(1, (0.55 + 0.4 * pa$developpement) * stats::runif(nrow(pa), 0.85, 1.1)))
# Équipements : adoption progressive (seuil latent fixe, probabilité croissante)
t <- pa$annee_debut - 2015
rur <- pa$milieu == "RURAL"
p_eau  <- ifelse(rur, 0.25 + 0.45 * pa$developpement, 0.85) + 0.012 * t
p_lat  <- ifelse(rur, 0.40 + 0.40 * pa$developpement, 0.92) + 0.015 * t
p_elec <- ifelse(rur, 0.05 + 0.30 * pa$developpement, 0.75) + 0.018 * t +
  ifelse(pa$cycle %in% c("POST_PRIMAIRE", "SECONDAIRE"), 0.15, 0)
p_cant <- ifelse(pa$statut == "PUBLIC" & pa$cycle %in% c("PRESCOLAIRE", "PRIMAIRE"),
                 0.45 + 0.35 * pa$pauvrete, 0.10) + 0.005 * t
pa$eau <- !pa$ferme & pa$u_eau < p_eau
pa$latrines <- !pa$ferme & pa$u_lat < p_lat
pa$electricite <- !pa$ferme & pa$u_elec < p_elec
pa$cantine <- !pa$ferme & pa$u_cant < p_cant
pa$nb_latrines <- ifelse(pa$latrines, pmax(2, round(pa$eff_t / stats::runif(nrow(pa), 35, 90))), 0)
pa$nb_tables_bancs <- ifelse(pa$ferme, 0, round(pa$eff_t / 2 * pa$couv_tb))
pa$nb_tables_bancs[pa$cycle == "PRESCOLAIRE"] <- NA  # mobilier différent au préscolaire
# Valeurs manquantes CONTRÔLÉES au niveau établissement (questionnaires incomplets)
pa$nb_tables_bancs[sample(nrow(pa), round(0.015 * nrow(pa)))] <- NA
pa$nb_ens_qualifies[sample(nrow(pa), round(0.02 * nrow(pa)))] <- NA

etab_annuel <- data.frame(
  id_etab = pa$id_etab, annee_debut = pa$annee_debut, annee_scolaire = annee_scolaire(pa$annee_debut),
  cycle = pa$cycle, code_region = pa$code_region, code_province = pa$code_province,
  code_commune = pa$code_commune,
  nom_commune = communes_sf$nom[match(pa$code_commune, communes_sf$code_geo)],
  statut = pa$statut, milieu = pa$milieu, annee_creation = pa$annee_creation,
  lon = pa$lon, lat = pa$lat, ferme = as.integer(pa$ferme),
  eff_g = pa$eff_g, eff_f = pa$eff_f, eff_t = pa$eff_t, nouv_cp1_g = pa$nouv_g, nouv_cp1_f = pa$nouv_f,
  nouv_cp1_t = pa$nouv_t, eff_cm2_t = pa$eff_cm2, redoublants_cm2_t = pa$redoublants_cm2,
  nb_salles_t = pa$nb_salles_t, nb_salles_bon_etat = pa$nb_salles_bon_etat,
  nb_salles_abris = pa$nb_salles_abris, nb_groupes_pedago = pa$nb_groupes_pedago,
  nb_ens_g = pa$nb_ens_g, nb_ens_f = pa$nb_ens_f, nb_ens_t = pa$nb_ens_t,
  nb_ens_qualifies = pa$nb_ens_qualifies,
  eau = as.integer(pa$eau), latrines = as.integer(pa$latrines),
  electricite = as.integer(pa$electricite), cantine = as.integer(pa$cantine),
  nb_latrines = pa$nb_latrines, nb_tables_bancs = pa$nb_tables_bancs,
  eleves_affectes = pa$eleves_affectes, stringsAsFactors = FALSE)
# 4 doublons volontaires (tester le contrôle de doublons ; retirés au nettoyage)
etab_annuel <- rbind(etab_annuel, etab_annuel[sample(nrow(etab_annuel), 4), ])

# --------------------------------------------------------------------------
# 6. Agrégation province -> classeur EXX_01
# --------------------------------------------------------------------------
message("6/6 Agrégation et écriture du classeur EXX_01")
ea <- etab_annuel[!duplicated(etab_annuel[, c("id_etab", "annee_debut")]), ]
cle_a <- paste(ea$cycle, ea$code_province, ea$annee_debut, sep = "|")
agg <- data.frame(cle = sort(unique(cle_a)), stringsAsFactors = FALSE)
pieces <- do.call(rbind, strsplit(agg$cle, "|", fixed = TRUE))
agg$cycle <- pieces[, 1]; agg$code_geo <- pieces[, 2]; agg$annee_debut <- as.integer(pieces[, 3])
somme_par <- function(v) as.numeric(tapply(v, cle_a, function(z) if (all(is.na(z))) NA else sum(z, na.rm = TRUE))[agg$cle])
for (v in c("eff_g", "eff_f", "eff_t", "nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "eff_cm2_t",
            "redoublants_cm2_t", "nb_salles_t", "nb_salles_bon_etat", "nb_salles_abris",
            "nb_groupes_pedago", "nb_ens_g", "nb_ens_f", "nb_ens_t", "nb_ens_qualifies",
            "nb_latrines", "nb_tables_bancs"))
  agg[[v]] <- somme_par(ea[[v]])
agg$eff_public_t <- somme_par(ea$eff_t * (ea$statut == "PUBLIC"))
agg$eff_prive_t <- somme_par(ea$eff_t * (ea$statut == "PRIVE"))
agg$eff_communautaire_t <- somme_par(ea$eff_t * (ea$statut == "COMMUNAUTAIRE"))
agg$eff_urbain_t <- somme_par(ea$eff_t * (ea$milieu == "URBAIN"))
agg$eff_rural_t <- somme_par(ea$eff_t * (ea$milieu == "RURAL"))
agg$nb_etab_public <- somme_par(ea$statut == "PUBLIC")
agg$nb_etab_prive <- somme_par(ea$statut == "PRIVE")
agg$nb_etab_communautaire <- somme_par(ea$statut == "COMMUNAUTAIRE")
agg$nb_etab_t <- somme_par(rep(1, nrow(ea)))
agg$nb_etab_fermes <- somme_par(ea$ferme)
agg$nb_etab_fonctionnels <- agg$nb_etab_t - agg$nb_etab_fermes
agg$nb_eleves_affectes <- somme_par(ea$eleves_affectes)
agg$nb_ens_public <- somme_par(ea$nb_ens_t * (ea$statut == "PUBLIC"))
agg$nb_etab_eau <- somme_par(ea$eau); agg$nb_etab_latrines <- somme_par(ea$latrines)
agg$nb_etab_electricite <- somme_par(ea$electricite); agg$nb_etab_cantine <- somme_par(ea$cantine)
# Grille complète cycle × province × année (une combinaison sans établissement
# devient une ligne « MANQUANT » au lieu de disparaître)
grille <- expand.grid(cycle = names(CYCLES), code_geo = prov$code_province, annee_debut = ANNEES_GEN,
                      stringsAsFactors = FALSE)
agg <- merge(grille, agg, by = c("cycle", "code_geo", "annee_debut"), all.x = TRUE)
agg$cle <- NULL
# Démographie
k_d <- match(paste(agg$code_geo, agg$annee_debut), paste(demo$code_province, demo$annee_debut))
agg$pop_totale <- demo$pop_totale[k_d]
agg$nb_pdi <- demo$nb_pdi[k_d]; agg$nb_pdi_age_scolaire <- demo$nb_pdi_age_scolaire[k_d]
agg$pop_age_scolaire <- round(agg$pop_totale * part_age[agg$cycle])
agg$pop_age_entree <- round(agg$pop_age_scolaire / duree[agg$cycle] * 1.06)
agg$pop_age_fin <- round(agg$pop_age_scolaire / duree[agg$cycle] * 0.95)
agg$superficie_km2 <- round(prov$superficie_km2[match(agg$code_geo, prov$code_province)], 1)
# Taux (en %), calculés de façon cohérente avec les effectifs
agg$tbs_t <- round(100 * agg$eff_t / agg$pop_age_scolaire, 1)
agg$tbs_g <- round(100 * agg$eff_g / (agg$pop_age_scolaire * 0.505), 1)
agg$tbs_f <- round(100 * agg$eff_f / (agg$pop_age_scolaire * 0.495), 1)
agg$tba_t <- round(100 * agg$nouv_cp1_t / agg$pop_age_entree, 1)
agg$tba_g <- round(100 * agg$nouv_cp1_g / (agg$pop_age_entree * 0.505), 1)
agg$tba_f <- round(100 * agg$nouv_cp1_f / (agg$pop_age_entree * 0.495), 1)
achev <- pmax(0, agg$eff_cm2_t - agg$redoublants_cm2_t)
agg$tap_t <- round(100 * achev / agg$pop_age_fin, 1)
agg$tap_f <- round(agg$tap_t * (agg$eff_f / pmax(agg$eff_t, 1)) / 0.495, 1)
agg$tap_g <- round(agg$tap_t * (agg$eff_g / pmax(agg$eff_t, 1)) / 0.505, 1)
pauv <- prov$pauvrete[match(agg$code_geo, prov$code_province)]
agg$cep_t <- ifelse(agg$cycle == "PRIMAIRE" & !is.na(agg$eff_t),
                    round(pmin(95, 82 - 35 * pauv + stats::rnorm(nrow(agg), 0, 3)), 1), NA)
agg$cep_g <- ifelse(is.na(agg$cep_t), NA, round(agg$cep_t + stats::runif(nrow(agg), 0, 3), 1))
agg$cep_f <- ifelse(is.na(agg$cep_t), NA, round(agg$cep_t - stats::runif(nrow(agg), 0, 3), 1))
agg$ips_tba <- round(agg$tba_f / agg$tba_g, 3); agg$ips_tbs <- round(agg$tbs_f / agg$tbs_g, 3)
agg$ips_tap <- round(agg$tap_f / agg$tap_g, 3)
# Trous CONTRÔLÉS au niveau province (tester le traitement des manquants)
trous <- list(nb_tables_bancs = 0.04, nb_latrines = 0.03, nb_ens_qualifies = 0.05,
              nb_etab_electricite = 0.02, nb_salles_abris = 0.06)
for (v in names(trous)) agg[[v]][sample(nrow(agg), round(trous[[v]] * nrow(agg)))] <- NA
pre_old <- agg$cycle == "PRESCOLAIRE" & agg$annee_debut <= 2016
for (v in c("nb_etab_eau", "nb_etab_latrines", "nb_etab_electricite", "nb_etab_cantine", "nb_latrines"))
  agg[[v]][pre_old] <- NA
agg$qualite <- ifelse(is.na(agg$eff_t), "MANQUANT", ifelse(pre_old, "SUSPECT", "OK"))
agg$sources <- "SYNTH"
agg$remarque <- ifelse(is.na(agg$eff_t), "Synthétique : aucun établissement simulé",
                       ifelse(pre_old, "Synthétique : équipements non renseignés (trou volontaire)",
                              "DONNÉES SYNTHÉTIQUES — aucune statistique officielle"))

# Ordre des lignes = ordre du modèle : cycle, province (DIM_GEO), année
agg$cycle <- factor(agg$cycle, levels = names(CYCLES))
agg <- agg[order(agg$cycle, agg$code_geo, agg$annee_debut), ]
agg$niveau_geo <- "PROVINCE"
agg$nom_province <- prov$province[match(agg$code_geo, prov$code_province)]
agg$nom_region <- unname(REGIONS[prov$code_region[match(agg$code_geo, prov$code_province)]])
agg$id_ligne <- paste(PREFIXE_CYCLE[as.character(agg$cycle)], agg$code_geo, agg$annee_debut, sep = "_")
agg$annee_scolaire <- annee_scolaire(agg$annee_debut)

# Panel région et national : sommes des provinces, taux recalculés
agreger_panel <- function(df, cle_geo) {
  add <- c("eff_g", "eff_f", "eff_t", "eff_public_t", "eff_prive_t", "eff_communautaire_t",
           "eff_urbain_t", "eff_rural_t", "nouv_cp1_g", "nouv_cp1_f", "nouv_cp1_t", "redoublants_cm2_t",
           "eff_cm2_t", "nb_etab_public", "nb_etab_prive", "nb_etab_communautaire", "nb_etab_t",
           "nb_salles_t", "nb_salles_bon_etat", "nb_salles_abris", "nb_groupes_pedago", "nb_ens_g",
           "nb_ens_f", "nb_ens_t", "nb_ens_public", "nb_ens_qualifies", "nb_etab_eau", "nb_etab_latrines",
           "nb_etab_electricite", "nb_etab_cantine", "nb_latrines", "nb_tables_bancs", "pop_totale",
           "pop_age_scolaire", "pop_age_entree", "pop_age_fin", "superficie_km2", "nb_etab_fermes",
           "nb_etab_fonctionnels", "nb_eleves_affectes", "nb_pdi", "nb_pdi_age_scolaire")
  g <- paste(df$cycle, cle_geo, df$annee_debut, sep = "|")
  out <- data.frame(cle = unique(g), stringsAsFactors = FALSE)
  pz <- do.call(rbind, strsplit(out$cle, "|", fixed = TRUE))
  out$cycle <- factor(pz[, 1], levels = names(CYCLES)); out$code_geo <- pz[, 2]
  out$annee_debut <- as.integer(pz[, 3])
  for (v in add) out[[v]] <- as.numeric(tapply(df[[v]], g, function(z) if (all(is.na(z))) NA else sum(z, na.rm = TRUE))[out$cle])
  out$tbs_t <- round(100 * out$eff_t / out$pop_age_scolaire, 1)
  out$tbs_g <- round(100 * out$eff_g / (out$pop_age_scolaire * 0.505), 1)
  out$tbs_f <- round(100 * out$eff_f / (out$pop_age_scolaire * 0.495), 1)
  out$tba_t <- round(100 * out$nouv_cp1_t / out$pop_age_entree, 1)
  out$tba_g <- round(100 * out$nouv_cp1_g / (out$pop_age_entree * 0.505), 1)
  out$tba_f <- round(100 * out$nouv_cp1_f / (out$pop_age_entree * 0.495), 1)
  out$tap_t <- round(100 * pmax(0, out$eff_cm2_t - out$redoublants_cm2_t) / out$pop_age_fin, 1)
  out$tap_g <- out$tap_t; out$tap_f <- out$tap_t
  w <- tapply(df$eff_t * df$cep_t, g, sum, na.rm = TRUE) / tapply(df$eff_t * !is.na(df$cep_t), g, sum, na.rm = TRUE)
  out$cep_t <- round(as.numeric(w[out$cle]), 1); out$cep_t[!is.finite(out$cep_t)] <- NA
  out$cep_g <- out$cep_t; out$cep_f <- out$cep_t
  out$ips_tba <- round(out$tba_f / out$tba_g, 3); out$ips_tbs <- round(out$tbs_f / out$tbs_g, 3)
  out$ips_tap <- NA
  out
}
p_tmp <- agg; p_tmp$code_region <- prov$code_region[match(agg$code_geo, prov$code_province)]
reg <- agreger_panel(p_tmp, p_tmp$code_region)
reg$niveau_geo <- "REGION"; reg$nom_region <- unname(REGIONS[reg$code_geo]); reg$nom_province <- NA
nat <- agreger_panel(p_tmp, rep("NAT", nrow(p_tmp)))
nat$niveau_geo <- "NATIONAL"; nat$nom_region <- "Burkina Faso"; nat$nom_province <- NA
pr <- rbind(nat[, names(reg)], reg)
pr <- pr[order(pr$cycle, pr$code_geo != "NAT", pr$code_geo, pr$annee_debut), ]
pr$id_ligne <- paste(PREFIXE_CYCLE[as.character(pr$cycle)], pr$code_geo, pr$annee_debut, sep = "_")
pr$annee_scolaire <- annee_scolaire(pr$annee_debut)
pr$qualite <- "OK"; pr$sources <- "SYNTH"; pr$remarque <- "DONNÉES SYNTHÉTIQUES — agrégat des provinces"

# --- Écriture dans une COPIE du modèle EXX_01 (formules recalées) ----------
ecrire_panel <- function(wb, feuille, df) {
  entetes <- as.character(unlist(openxlsx::read.xlsx(MODELE, sheet = feuille, rows = 3, colNames = FALSE)))
  openxlsx::deleteData(wb, sheet = feuille, cols = seq_along(entetes), rows = 4:2500, gridExpand = TRUE)
  for (j in seq_along(entetes)) {
    v <- entetes[j]
    if (v %in% names(df)) {
      x <- df[[v]]; if (is.factor(x)) x <- as.character(x)
      openxlsx::writeData(wb, feuille, x, startCol = j, startRow = 4, colNames = FALSE)
    }
  }
  invisible(NULL)
}
wb <- openxlsx::loadWorkbook(MODELE)
ecrire_panel(wb, "PANEL_PROVINCE", agg)
ecrire_panel(wb, "PANEL_REGION", pr)
# Formules de contrôle (colonnes calculées BN:CI) recalées pour chaque ligne
formules_controle <- function(r) c(
  BN = sprintf('IF(OR(J%1$d="",K%1$d=""),"",IF(N(K%1$d)=0,"",J%1$d/K%1$d))', r),
  BO = sprintf('IF(OR(K%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",K%1$d/AN%1$d))', r),
  BP = sprintf('IF(OR(K%1$d="",AO%1$d=""),"",IF(N(AO%1$d)=0,"",K%1$d/AO%1$d))', r),
  BQ = sprintf('IF(OR(K%1$d="",AR%1$d=""),"",IF(N(AR%1$d)=0,"",K%1$d/AR%1$d))', r),
  BR = sprintf('IF(OR(K%1$d="",AU%1$d=""),"",IF(N(AU%1$d)=0,"",K%1$d/AU%1$d))', r),
  BS = sprintf('IF(OR(AW%1$d="",AU%1$d=""),"",IF(N(AU%1$d)=0,"",AW%1$d/AU%1$d))', r),
  BT = sprintf('IF(OR(AX%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",AX%1$d/AN%1$d))', r),
  BU = sprintf('IF(OR(AY%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",AY%1$d/AN%1$d))', r),
  BV = sprintf('IF(OR(AZ%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",AZ%1$d/AN%1$d))', r),
  BW = sprintf('IF(OR(BA%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",BA%1$d/AN%1$d))', r),
  BX = sprintf('IF(OR(K%1$d="",BB%1$d=""),"",IF(N(BB%1$d)=0,"",K%1$d/BB%1$d))', r),
  BY = sprintf('IF(OR(K%1$d="",BC%1$d=""),"",IF(N(BC%1$d)=0,"",K%1$d/BC%1$d))', r),
  BZ = sprintf('IF(OR(BJ%1$d="",AN%1$d=""),"",IF(N(AN%1$d)=0,"",BJ%1$d/AN%1$d))', r),
  CA = sprintf('IF(OR(AN%1$d="",BD%1$d=""),"",IF(N(BD%1$d)=0,"",AN%1$d/BD%1$d*10000))', r),
  CB = sprintf('IF(OR(AN%1$d="",BH%1$d=""),"",IF(N(BH%1$d)=0,"",AN%1$d/BH%1$d*100))', r),
  CC = sprintf('IF(OR(BE%1$d="",AO%1$d=""),"",IF(N(AO%1$d)=0,"",BE%1$d/AO%1$d))', r),
  CD = sprintf('IF(OR(K%1$d="",BE%1$d=""),"",IF(N(BE%1$d)=0,"",K%1$d/BE%1$d*100))', r),
  CE = sprintf('IF(OR(S%1$d="",BF%1$d=""),"",IF(N(BF%1$d)=0,"",S%1$d/BF%1$d*100))', r),
  CF = sprintf('IF(OR(Z%1$d="",Y%1$d=""),"",IF(N(Y%1$d)=0,"",Z%1$d/Y%1$d))', r),
  CG = sprintf('IF(OR(I%1$d="",J%1$d="",K%1$d=""),"",I%1$d+J%1$d-K%1$d)', r),
  CH = sprintf('IF(OR(AK%1$d="",AL%1$d="",AM%1$d="",AN%1$d=""),"",AK%1$d+AL%1$d+AM%1$d-AN%1$d)', r),
  CI = sprintf('IF(OR(AA%1$d="",CD%1$d=""),"",AA%1$d-CD%1$d)', r))
for (f in c("PANEL_PROVINCE", "PANEL_REGION")) {
  n <- if (f == "PANEL_PROVINCE") nrow(agg) else nrow(pr)
  lignes <- 4:(n + 3)
  fm <- t(vapply(lignes, formules_controle, character(22)))
  for (j in seq_len(ncol(fm))) {
    x <- fm[, j]; class(x) <- c(class(x), "formula")
    openxlsx::writeData(wb, f, x, startCol = openxlsx::col2int(colnames(fm)[j]), startRow = 4, colNames = FALSE)
  }
}
# Normes de démonstration (clairement étiquetées)
nr <- lire_feuille(MODELE, "NORMES", "code_parametre")
nr <- nr[!is.na(nr$code_parametre), ]
val_demo <- c(TAILLE_CLASSE_MAX = NA, RATIO_ELEVES_ENSEIGNANT = NA, PLACES_TABLE_BANC = 2,
              ELEVES_PAR_LATRINE = 50, SUPERFICIE_SALLE = 63, DISTANCE_MAX_ECOLE = 3,
              AGE_ENTREE = 6, DUREE_CYCLE = 6, COUT_SALLE = 12e6, COUT_ENSEIGNANT = 2.4e6, HORIZON = 2030)
nr$valeur <- unname(val_demo[nr$code_parametre])
k1 <- nr$code_parametre == "TAILLE_CLASSE_MAX"; nr$valeur[k1] <- c(PRESCOLAIRE = 35, PRIMAIRE = 60)[nr$cycle[k1]]
k2 <- nr$code_parametre == "RATIO_ELEVES_ENSEIGNANT"; nr$valeur[k2] <- c(PRESCOLAIRE = 30, PRIMAIRE = 55)[nr$cycle[k2]]
extra <- data.frame(code_parametre = rep(c("TAILLE_CLASSE_MAX", "RATIO_ELEVES_ENSEIGNANT"), each = 2),
                    lib = rep(c("Nombre maximal d'élèves par classe", "Élèves par enseignant (norme)"), each = 2),
                    cycle = rep(c("POST_PRIMAIRE", "SECONDAIRE"), 2), valeur = c(70, 60, 60, 50),
                    stringsAsFactors = FALSE)
normes_out <- rbind(data.frame(code_parametre = nr$code_parametre, lib = nr[[2]], cycle = nr$cycle, valeur = nr$valeur,
                               stringsAsFactors = FALSE), extra)
normes_out$unite <- ""; normes_out$source <- "VALEUR DE DÉMONSTRATION (synthétique) — à remplacer par la norme officielle"
normes_out$date_source <- format(Sys.Date()); normes_out$statut <- "Démo"
openxlsx::writeData(wb, "NORMES", normes_out, startRow = 5, colNames = FALSE)
# Avertissement en tête du LISEZ_MOI + feuille de traçabilité
openxlsx::writeData(wb, "LISEZ_MOI", "DONNÉES SYNTHÉTIQUES — ENVIRONNEMENT DE DÉMONSTRATION — AUCUNE STATISTIQUE OFFICIELLE",
                    startCol = 1, startRow = 2)
if ("SYNTHETIQUE_INFO" %in% names(wb)) openxlsx::removeWorksheet(wb, "SYNTHETIQUE_INFO")
openxlsx::addWorksheet(wb, "SYNTHETIQUE_INFO", tabColour = "#D1495B")
openxlsx::writeData(wb, "SYNTHETIQUE_INFO", data.frame(
  rubrique = c("Avertissement", "Script", "Graine", "Date de génération", "Établissements",
               "Lignes établissement x année", "Communes", "Provinces x cycles x années", "Géométries"),
  valeur = c("DONNÉES SYNTHÉTIQUES — ENVIRONNEMENT DE DÉMONSTRATION — AUCUNE STATISTIQUE OFFICIELLE",
             "scripts/01_generer_donnees_synthetiques.R", cfg$graine, format(Sys.time()),
             n_e, nrow(etab_annuel), nrow(communes_sf), nrow(agg),
             "Voronoï autour de chefs-lieux approchés : limites NON officielles")))
openxlsx::saveWorkbook(wb, file.path(SORTIE, "EXX_01_base_synthetique.xlsx"), overwrite = TRUE)

# --- Autres fichiers -------------------------------------------------------
data.table::fwrite(etab_annuel, file.path(SORTIE, "etablissements.csv"), bom = TRUE)
ctx <- demo[, c("code_province", "annee_debut", "pauvrete", "urbanisation", "intensite", "anciennete_crise")]
names(ctx) <- c("code_geo", "annee_debut", "taux_pauvrete", "taux_urbanisation", "indice_insecurite", "annees_crise")
ctx$taux_pauvrete <- round(pmin(0.9, ctx$taux_pauvrete * (1 - 0.01 * (ctx$annee_debut - 2015)) +
                                  0.08 * ctx$indice_insecurite * (ctx$annees_crise > 0) + stats::rnorm(nrow(ctx), 0, 0.01)), 3)
ctx$taux_urbanisation <- round(pmin(0.95, ctx$taux_urbanisation * (1 + 0.02 * (ctx$annee_debut - 2015))), 3)
ctx$indice_insecurite <- round(ifelse(ctx$annees_crise > 0, pmin(1, ctx$indice_insecurite * (0.6 + 0.12 * ctx$annees_crise)), 0), 3)
ctx$annees_crise <- NULL
data.table::fwrite(ctx[order(ctx$code_geo, ctx$annee_debut), ], file.path(SORTIE, "contexte_province.csv"), bom = TRUE)
gp <- file.path(SORTIE, "geometries_synthetiques.gpkg")
if (file.exists(gp)) unlink(gp)
st_write(st_transform(regions_sf, 4326), gp, layer = "regions", quiet = TRUE)
st_write(st_transform(provinces_sf, 4326), gp, layer = "provinces", quiet = TRUE, append = TRUE)
st_write(st_transform(communes_sf, 4326), gp, layer = "communes", quiet = TRUE, append = TRUE)
unlink(list.files("data/prepare", full.names = TRUE))   # invalide les caches
message(sprintf("Terminé : %d établissements, %d lignes établissement x année, %d lignes province, %d communes.",
                n_e, nrow(etab_annuel), nrow(agg), nrow(communes_sf)))
