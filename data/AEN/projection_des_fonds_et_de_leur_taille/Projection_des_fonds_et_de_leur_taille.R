setwd("C:/Users/raiku/Documents/Stage/Projection des fonds et de leur taille")
#Romain Margaron

library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(tidyr)
library(grid)
library(stringi)
library(packcircles)
library(ggiraph)
library(stringr)
library(ggrepel)

# Nous creer une visualisation d'un inventaire du nombre et type de
# photographies présentent au  travers l'ensemble des fonds
# des Archives de l'Etat de Neuchâtel

#Nous allons commencer ici une distribution sur le temps des fonds photographiques par rapport à leur colorométrie
#et leurs ma

df <- read_excel("DF_inventaire_photos_AEN.xlsx")



# ===== Visualisation 4: Circle packing des fonds (Option A) =====
# Chaque cercle = un fonds, taille = volume total (somme des colonnes nbre_*),
# couleur = type_fonds (public, privé, rassemblement fictif).
# Version statique (JPEG) + version interactive (HTML, clic = permalien)

# 4.1 Agrégation par fonds
df_bubbles <- df %>%
  mutate(across(starts_with("nbre_"), ~ suppressWarnings(as.numeric(.x)))) %>%
  group_by(fonds) %>%
  summarise(
    vol_total = sum(c_across(starts_with("nbre_")), na.rm = TRUE),
    type_fonds = dplyr::first(na.omit(as.character(type_fonds))),
    permalien  = dplyr::first(na.omit(as.character(permalien))),
    .groups = "drop"
  ) %>%
  filter(!is.na(fonds), vol_total > 0) %>%
  mutate(
    type_fonds = factor(type_fonds, levels = c("privé", "public", "rassemblement thématique", "Privé", "Public", "Rassemblement thématique"))
  )

# 4.2 Disposition des cercles (non chevauchants)
# Définition des seuils de taille pour les cercles
size_breaks <- c(20, 50, 100, 500, 1000, 2000, 5000, 10000, 15000)

# Création des classes de taille avec ajustement des tailles
df_bubbles <- df_bubbles %>%
  mutate(
    # Ajustement non-linéaire des tailles
    adjusted_size = case_when(
      vol_total > 5000 ~ vol_total * 0.9,    # Réduction des très grands cercles
      vol_total < 100 ~ vol_total * 1.3,     # Augmentation plus marquée des très petits cercles
      vol_total == 100 ~ vol_total * 1.25,   # Augmentation pour 100
      vol_total == 500 ~ vol_total * 1.2,    # Augmentation pour 500
      vol_total == 1000 ~ vol_total * 1.15,  # Augmentation pour 1000
      TRUE ~ vol_total * 1.1                 # Légère augmentation pour les autres tailles
    ),
    
    # Application des classes de taille sur les tailles ajustées
    vol_size = findInterval(adjusted_size, size_breaks, left.open = TRUE) %>%
      {size_breaks[. + 1] %>% replace_na(last(size_breaks))}
  )

# Calcul du layout des cercles
layout_cp <- packcircles::circleProgressiveLayout(df_bubbles$vol_size, sizetype = "area")

# Préparation des données pour le tracé
df_bubbles <- bind_cols(
  df_bubbles, 
  layout_cp %>% dplyr::rename(cx = x, cy = y, r = radius)
)

# Création des sommets pour les polygones des cercles
vertices <- packcircles::circleLayoutVertices(layout_cp, npoints = 60, idcol = NULL) %>%
  dplyr::mutate(id = as.integer(id))

# Joindre les attributs aux sommets
vertices_full <- vertices %>%
  left_join(df_bubbles %>% mutate(id = row_number()), by = "id") %>%
  # Champs préparés pour ggiraph: tooltip sans apostrophes non échappées,
  # et JS onclick qui ouvre le permalien
  mutate(
    # Tooltip garanti sans apostrophes (on les retire)
    tooltip_safe = paste0(
      stringr::str_replace_all(fonds, "'", ""),
      "\nTotal: ", scales::comma(vol_total)
    ),
    # Identifiant d'élément interactif sans apostrophes
    data_id_safe = stringr::str_replace_all(fonds, "'", ""),
    # URL: encoder entièrement pour un href/JS sûr
    permalink = as.character(dplyr::coalesce(permalien, "")) %>%
      utils::URLencode(reserved = TRUE),
    # Indicateur de présence de lien
    has_link = permalink != "",
    # JS onclick sans aucune apostrophe, uniquement des guillemets doubles échappés
    onclick_js = ifelse(
      has_link,
      paste0("window.open(\\\"", permalink, "\\\",\\\"_blank\\\")"),
      NA_character_
    ),
    # Facultatif: indiquer dans le tooltip s'il n'y a pas de lien
    tooltip_safe = ifelse(has_link, tooltip_safe, paste0(tooltip_safe, "\n(pas de permalien)"))
  )

# Palette des types de fonds (harmonise minuscules/majuscules)
# RColorBrewer Set2 (douce et lisible)
pal3 <- RColorBrewer::brewer.pal(3, "Set2")
pal_fonds <- c(
  "privé" = pal3[1], "Privé" = pal3[1],
  "public" = pal3[2], "Public" = pal3[2],
  "rassemblement thématique" = pal3[3], "Rassemblement thématique" = pal3[3]
)

# Nouveaux seuils pour les tailles de cercles
size_breaks <- c(10, 20, 50, 100, 500, 1000, 3000, 5000, 10000)
# Création d'une échelle personnalisée pour le mapping des tailles
size_mapping <- c(
  "10" = 1, "20" = 2, "50" = 3, 
  "100" = 4, "500" = 5, "1000" = 6, 
  "3000" = 7, "5000" = 8, "10000" = 9
)

# Création du dataframe pour la légende avec les nouvelles tailles
df_size_leg <- tibble::tibble(size_val = size_breaks)

# 4.3 Rendu statique
p_circlepack <- ggplot(vertices_full) +
  geom_polygon(aes(x, y, group = id, fill = type_fonds), colour = "white", linewidth = 0.3) +
  # Couche factice (invisible) pour générer une légende de tailles
  geom_point(
    data = df_size_leg,
    aes(x = 0, y = 0, size = size_val),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  coord_equal() +
  scale_fill_manual(values = pal_fonds, 
                   na.value = "#B0B0B0", 
                   name = "Type de fonds") +
  # Ajustement de l'espacement des légendes
  theme(legend.spacing.y = unit(5, "pt")) +
  scale_size_area(name = "Taille des fonds",
                   breaks = size_breaks,
                   labels = scales::comma(size_breaks),
                   max_size = 20,
                   guide = guide_legend(
                     title.position = "top",
                     title.theme = element_text(face = "bold", size = 12),
                     override.aes = list(alpha = 1, colour = "#555555")
                   )
  ) +
  # Mise en forme du graphique
  theme_void(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.margin = margin(15, 15, 15, 25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 5, l = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10, l = 5)),
    plot.caption = element_text(size = 9, colour = "#555555"),
    
    # Style de la légende
    legend.position = "right",
    legend.justification = "left",  # Alignement à gauche
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.7, "lines"),
    legend.spacing.y = unit(5, "pt"),
    legend.title = element_text(size = 10, face = "bold", hjust = 0, margin = margin(b = 5, l = 0)),
    legend.text = element_text(size = 9, hjust = 0),
    legend.box = "vertical"
  ) +
  labs(
    title = "Projection des fonds photographiques selon leur volume et leur statut",
    subtitle = "Chaque cercle représente le volume photographique conservé dans un fonds.",
    caption = "Données : Inventaire des fonds photographiques — Archives de l'État de Neuchâtel"
  ) +
  guides(
    fill = guide_legend(order = 1, title.theme = element_text(face = "bold")),
    size = guide_legend(order = 2, title.theme = element_text(face = "bold"),
                        override.aes = list(alpha = 1, colour = "#555555"))
  )

# Libellés intégrés dans les cercles (pas de fond, pas de contour), sans troncature
df_labels <- df_bubbles %>%
  mutate(
    # Application du nouveau mapping des tailles
    size_mapped = as.numeric(as.character(cut(vol_total, 
                                            breaks = c(0, size_breaks[-1], Inf), 
                                            labels = size_breaks, 
                                            right = TRUE))),
    # Ajustement des rayons pour correspondre aux nouvelles tailles
    r = sqrt(size_mapped / max(size_breaks, na.rm = TRUE)) * 1.2,  # Augmentation de l'échelle des rayons
    label = fonds,
    # Taille du texte en fonction du rayon (plus grand cercle => texte plus grand)
    size_pt = pmin(8.0, pmax(3.6, 3.0 + 6.0 * (r / max(r, na.rm = TRUE))))
  ) %>%
  filter(vol_total >= 80)  # Afficher les labels à partir de 80 unités

p_circlepack <- p_circlepack +
  geom_text(
    data = df_labels,
    aes(x = cx, y = cy, label = label),
    inherit.aes = FALSE,
    size = df_labels$size_pt / ggplot2::.pt,
    colour = "#0b0b0b",
    fontface = "bold",
    lineheight = 0.95
  )

# 4.4 Sauvegarde du rendu statique (SVG)
ggsave("Projection_fonds_circle_packing.svg",
       plot = p_circlepack,
       width = 10,   # Largeur en pouces
       height = 8,   # Hauteur en pouces
       dpi = 300,    # Haute résolution
       bg = "white"  # Fond blanc
)

# 4.5 Sauvegarde du rendu statique (JPEG - pour compatibilité)
ggsave("Projection_fonds_circle_packing.jpeg",
       plot = p_circlepack,
       width = 10,   # Largeur en pouces
       height = 8,   # Hauteur en pouces
       dpi = 300,    # Haute résolution
       bg = "white"  # Fond blanc
)
