setwd("C:/Users/raiku/Documents/Stage/Viusalisation distribution sur le temps")
#Romain Margaron

library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(grid)
library(stringi)

# Nous creer une visualisation d'un inventaire du nombre et type de
# photographies présentent au  travers l'ensemble des fonds
# des Archives de l'Etat de Neuchâtel

#Nous allons commencer ici une distribution sur le temps des fonds photographiques par rapport à leur colorométrie
#et leurs ma

df <- read_excel("DF_inventaire_photos_AEN.xlsx")


# ===== Préparation minimale: point médian (1880–1995) + palette + échelle Y =====
df_mid <- df %>%
  mutate(
    date_min = as.numeric(date_min),
    date_max = as.numeric(date_max),
    year     = floor((date_min + date_max)/2)
  ) %>%
  filter(!is.na(year), year >= 1880, year <= 1995)

## Palette: ColorBrewer Set2 (pastel contrastée) – qualitative, 7 catégories

# Échelle Y: graduations "pretty" et libellés complets
y_breaks_fun <- scales::breaks_extended(n = 6)
y_labels_fun <- scales::label_number(
  accuracy = 1,
  big.mark = " "
)

# ===== Variante test: tranches de 10 ans =====
breaks_10 <- seq(1880, 2000, by = 10)
labels_10 <- paste0(breaks_10[-length(breaks_10)], "–", breaks_10[-1] - 1)

df_supports_10 <- df_mid %>%
  mutate(bin_10y = cut(year, breaks = breaks_10, right = FALSE, labels = labels_10)) %>%
  mutate(
    nbre_photographie      = suppressWarnings(as.numeric(nbre_photographie)),
    nbre_carte_postale     = suppressWarnings(as.numeric(nbre_carte_postale)),
    nbre_negatif           = suppressWarnings(as.numeric(nbre_negatif)),
    nbre_planches_contact  = suppressWarnings(as.numeric(nbre_planches_contact)),
    nbre_diapositive       = suppressWarnings(as.numeric(nbre_diapositive)),
    nbre_plaque_de_verre   = suppressWarnings(as.numeric(nbre_plaque_de_verre)),
    nbre_daguerrotype      = suppressWarnings(as.numeric(nbre_daguerrotype))
  ) %>%
  select(bin_10y, nbre_photographie, nbre_carte_postale, nbre_negatif,
         nbre_planches_contact, nbre_diapositive, nbre_plaque_de_verre, nbre_daguerrotype) %>%
  pivot_longer(cols = starts_with("nbre_"), names_to = "support_col", values_to = "n") %>%
  mutate(n = replace_na(n, 0)) %>%
  mutate(Support = case_when(
    support_col == "nbre_photographie"      ~ "Tirage positif sur papier",
    support_col == "nbre_carte_postale"     ~ "Carte postale photographique",
    support_col == "nbre_negatif"           ~ "Négatif souple (film)",
    support_col == "nbre_planches_contact"  ~ "Planche-contact",
    support_col == "nbre_diapositive"       ~ "Diapositive",
    support_col == "nbre_plaque_de_verre"   ~ "Négatif sur plaque de verre",
    support_col == "nbre_daguerrotype"      ~ "Daguerréotype",
    TRUE ~ NA_character_
  )) %>%
  # Ordre: Positifs d'abord, puis Négatifs & assimilés (contrôle empilement et légende)
  mutate(Support = factor(
    Support,
    levels = c(
      "Tirage positif sur papier", "Carte postale photographique",
      "Planche-contact", "Négatif souple (film)", "Négatif sur plaque de verre", "Daguerréotype", "Diapositive"
    )
  )) %>%
  filter(!is.na(bin_10y), !is.na(Support)) %>%
  group_by(bin_10y, Support) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(bin_10y = factor(bin_10y, levels = labels_10)) %>%
  group_by(bin_10y) %>%
  mutate(
    total = sum(n, na.rm = TRUE),
    prop = ifelse(total > 0, n / total, 0),
    label_ok = prop >= 0.20,
    label_txt = scales::label_number(accuracy = 1, big.mark = " ")(n)
  ) %>%
  ungroup()

# Étendue Y: inclure exactement le maximum total par période et forcer une graduation au sommet
y_max_10 <- df_supports_10 %>%
  group_by(bin_10y) %>% summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
  summarise(max_total = max(total, na.rm = TRUE)) %>% pull(max_total)
y_breaks_10 <- y_breaks_fun(c(0, y_max_10))
if (length(y_breaks_10) == 0 || max(y_breaks_10) < y_max_10) {
  y_breaks_10 <- sort(unique(c(y_breaks_10, y_max_10)))
}

# Totaux par décennie (pour affichage au-dessus des barres)
df_totals_10 <- df_supports_10 %>%
  group_by(bin_10y) %>%
  summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_total = scales::label_number(accuracy = 1, big.mark = " ")(total))

# Palette Set2 avec inversion de couleurs entre "Planche-contact" et "Négatif souple (film)"
pal_set2 <- RColorBrewer::brewer.pal(7, "Set2")
# Ordre des catégories tel que défini dans les breaks plus bas
cats_support <- c(
  "Tirage positif sur papier", "Carte postale photographique",
  "Planche-contact", "Négatif souple (film)", "Négatif sur plaque de verre", "Daguerréotype", "Diapositive"
)
# Attribution par défaut (positionnelle)
pal_support <- setNames(pal_set2[seq_along(cats_support)], cats_support)
# Échanger les couleurs des deux catégories demandées
pc_col <- pal_support[["Planche-contact"]]
neg_col <- pal_support[["Négatif souple (film)"]]
pal_support[["Planche-contact"]] <- neg_col
pal_support[["Négatif souple (film)"]] <- pc_col

p_support_bar_10_mid <- ggplot(df_supports_10, aes(x = bin_10y, y = n, fill = Support)) +
  geom_col(width = 0.9, color = "white", linewidth = 0.2) +
  geom_text(
    data = df_totals_10,
    aes(x = bin_10y, y = total, label = label_total),
    inherit.aes = FALSE,
    vjust = -0.4,
    size = 3.2
  ) +
  scale_fill_manual(
    name = "Support",
    values = pal_support,
    breaks = cats_support
  ) +
  scale_y_continuous(
    breaks = y_breaks_10,
    labels = y_labels_fun,
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title    = "Types de support dans les fonds photographiques (1880–1995)",
    subtitle = "Point médian par fonds, regroupé par tranche décennales (10 ans). Chaque fonds est positionné sur sa date médiane de production, faute de dates précises pour chaque image.",
    x        = "Période (tranches de 10 ans)",
    y        = "Nombre de photos par support",
    caption  = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption  = element_text(size = 8, hjust = 1, colour = "grey30", lineheight = 0.95, margin = margin(t = 4)),
    plot.caption.position = "plot",
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12))
  )

#SAVE JPEG
ggsave("distribution_types_supports_1880_1995_10ans_median.jpeg",
       p_support_bar_10_mid, width = 14, height = 7, dpi = 300, bg = "white")

#SAVE SVG
ggsave("distribution_types_supports_1880_1995_10ans_median.svg",
       p_support_bar_10_mid, width = 14, height = 7, dpi = 300, bg = "white")


# ===== Visualisation 2: Frise chronologique par fonds (1850–2005), couleur = combinaisons de colorimétries =====

# Préparation: bornage des dates et calcul du volume par ligne
df_fonds_base <- df %>%
  mutate(
    fonds = stringr::str_squish(as.character(fonds)),
    date_min = suppressWarnings(as.numeric(date_min)),
    date_max = suppressWarnings(as.numeric(date_max))
  ) %>%
  # ne pas exclure si un seul des deux est présent
  mutate(
    date_min_eff = dplyr::coalesce(date_min, date_max),
    date_max_eff = dplyr::coalesce(date_max, date_min)
  ) %>%
  # bornage après imputation
  mutate(
    date_min_b = pmax(1850, pmin(2005, date_min_eff)),
    date_max_b = pmax(1850, pmin(2005, date_max_eff)),
    year_mid   = floor((date_min_b + date_max_b) / 2)
  )

# Colorimétrie simplifiée à 3 classes (normalisation des accents et espaces)
df_fonds_base <- df_fonds_base %>%
  mutate(
    color_raw0 = tolower(coalesce(as.character(colorimetrie), "")),
    # enlever les accents (ex: "sépia" -> "sepia"), compresser espaces
    color_raw  = stringi::stri_trans_general(stringr::str_squish(color_raw0), "Latin-ASCII")
  ) %>%
  mutate(colorimetrie_3 = case_when(
    str_detect(color_raw, "sepia") ~ "Sépia",
    str_detect(color_raw, "noir|noire|blanc|blanche|n&b|\bnb\b|b/w|\bbw\b") ~ "Noir et blanc",
    str_detect(color_raw, "couleur|couleurs|color|chrom") ~ "Couleur",
    str_detect(color_raw, "negatif|negatives|negatif(s)?|n[eé]gatif") ~ "Négatif",
    TRUE ~ NA_character_
  ))

# Volume par ligne = somme des colonnes nbre_* (vectorisé, sûr numériquement)
df_fonds_base <- df_fonds_base %>%
  mutate(across(starts_with("nbre_"), ~ suppressWarnings(as.numeric(.x))))

# Utiliser rowSums pour garantir un vecteur numérique (évite les colonnes liste et les erreurs de type)
nbre_cols <- df_fonds_base %>% select(starts_with("nbre_"))
df_fonds_base$volume_ligne <- if (ncol(nbre_cols) > 0) rowSums(nbre_cols, na.rm = TRUE) else 0

# Agrégation par fonds (présence regex par fonds, SANS pondération au niveau ligne)
df_fonds_agg <- df_fonds_base %>%
  group_by(fonds) %>%
  summarise(
    start = suppressWarnings(min(date_min_b, na.rm = TRUE)),
    end   = suppressWarnings(max(date_max_b, na.rm = TRUE)),
    year_mid   = suppressWarnings(floor(median(year_mid, na.rm = TRUE))),
    vol_total  = sum(volume_ligne, na.rm = TRUE),
    has_nb  = any(str_detect(color_raw, "noir|noire|blanc|blanche|n&b|\bnb\b|b/w|\bbw\b"), na.rm = TRUE),
    has_sep = any(str_detect(color_raw, "sepia|s[eé]pia"), na.rm = TRUE),
    has_col = any(str_detect(color_raw, "couleur|couleurs|color|chrom"), na.rm = TRUE),
    sum_photographie      = sum(suppressWarnings(as.numeric(nbre_photographie)), na.rm = TRUE),
    sum_carte_postale     = sum(suppressWarnings(as.numeric(nbre_carte_postale)), na.rm = TRUE),
    sum_negatif           = sum(suppressWarnings(as.numeric(nbre_negatif)), na.rm = TRUE),
    sum_planches_contact  = sum(suppressWarnings(as.numeric(nbre_planches_contact)), na.rm = TRUE),
    sum_diapositive       = sum(suppressWarnings(as.numeric(nbre_diapositive)), na.rm = TRUE),
    sum_plaque_de_verre   = sum(suppressWarnings(as.numeric(nbre_plaque_de_verre)), na.rm = TRUE),
    sum_daguerrotype      = sum(suppressWarnings(as.numeric(nbre_daguerrotype)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    neg_units = sum_negatif + sum_plaque_de_verre + sum_daguerrotype,
    other_units = sum_photographie + sum_carte_postale + sum_diapositive + sum_planches_contact,
    has_neg_excl = (neg_units > 0) & (other_units == 0)
  ) %>%
  # Déterminer la combinaison une seule fois ici
  mutate(
    combo = case_when(
      has_neg_excl ~ "Négatif uniquement",
      has_nb & !has_sep & !has_col ~ "Noir et blanc uniquement",
      !has_nb & has_sep & !has_col ~ "Sépia uniquement",
      !has_nb & !has_sep & has_col ~ "Couleur uniquement",
      has_nb & has_sep & !has_col ~ "Noir et blanc, Sépia",
      has_nb & !has_sep & has_col ~ "Noir et blanc, Couleur",
      # Sépia, Couleur a été retiré (pas présent dans tes données)
      has_nb & has_sep & has_col ~ "Noir et blanc, Sépia, Couleur",
      TRUE ~ "Non renseigné"
    )
  ) %>%
  mutate(combo = stringr::str_squish(combo)) %>%
  # bornes finales et suppression des fonds sans intervalle
  mutate(
    start = pmax(1850, pmin(2005, start)),
    end   = pmax(1850, pmin(2005, end))
  ) %>%
  filter(!is.infinite(start), !is.infinite(end), !is.na(start), !is.na(end), end >= start)

# Inclure tous les fonds et ordonner par médiane
df_fonds_top <- df_fonds_agg %>%
  arrange(fonds) %>%
  mutate(
    # ordre alphabétique de haut en bas
    fonds_ord = factor(fonds, levels = rev(sort(unique(fonds))))
  )

levels_combo <- c(
  "Négatif uniquement",
  "Noir et blanc uniquement",
  "Sépia uniquement",
  "Couleur uniquement",
  "Noir et blanc, Sépia",
  "Noir et blanc, Couleur",
  "Noir et blanc, Sépia, Couleur",
  "Non renseigné"
)
df_fonds_top <- df_fonds_top %>% mutate(combo = factor(combo, levels = levels_combo))
# Palette manuelle (8 catégories, teintes distinctes et lisibles)
pal_combo <- c(
  "Négatif uniquement"              = "#7F7F7F",  # gris
  "Noir et blanc uniquement"        = "#90be6d",  # vert
  "Sépia uniquement"                = "#ffd670",  # jaune pastel
  "Couleur uniquement"              = "#70d6ff",  # bleu pastel
  "Noir et blanc, Sépia"            = "#f3722c",  # orange soutenu
  "Noir et blanc, Couleur"          = "#9d4edd",  # violet/bleu soutenu
  "Noir et blanc, Sépia, Couleur"   = "#dd1d20",  # rouge
  "Non renseigné"                   = "#B0B0B0"   # gris clair
)

## (Diagnostics retirés à la demande)

p_frise_fonds <- ggplot(df_fonds_top) +
  geom_segment(aes(x = start, xend = end, y = fonds_ord, yend = fonds_ord, color = combo), linewidth = 2) +
  scale_color_manual(name = "Colorimétrie présente :", values = pal_combo, na.translate = FALSE, breaks = levels_combo, drop = TRUE) +
  scale_x_continuous(limits = c(1850, 2005), breaks = seq(1850, 2005, by = 15)) +
  labs(
    title = "Distribution chronologique des fonds photographiques et de leur colorimétrie",
    subtitle = "Chaque segment représente l'intervalle de dates couvert par un fonds ; la couleur indique la ou les colorimétries identifiées.",
    x = "Année",
    y = "Fonds",
    caption = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, colour = "grey30"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0),
    legend.text = element_text(size = 9),
    legend.key.width = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 2)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title.position = "top", title.hjust = 0, label.hjust = 0, override.aes = list(linewidth = 3))) +
  coord_cartesian(xlim = c(1850, 2005), clip = "off")

#SAVE JPEG
ggsave("frise_fonds_1850_2005_combinaisons_colorimetries.jpeg", p_frise_fonds, width = 18, height = 10, dpi = 300, bg = "white")
#SAVE SVG
ggsave("frise_fonds_1850_2005_combinaisons_colorimetries.svg", p_frise_fonds, width = 18, height = 10, dpi = 300, bg = "white")

# ===== Visualisation 3: Frise par fonds SANS colorimétrie (segments neutres, sans légende) =====

p_frise_fonds_simple <- ggplot(df_fonds_top) +
  geom_segment(aes(x = start, xend = end, y = fonds_ord, yend = fonds_ord),
               linewidth = 2.4, color = "#413f3f") +
  scale_x_continuous(limits = c(1850, 2005), breaks = seq(1850, 2005, by = 10)) +
  labs(
    title = "Répartition dans le temps des fonds photographiques",
    subtitle = "Chaque segment représente l'intervalle de dates couvert par un fonds.",
    x = "Année",
    y = "Fonds",
    caption = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, colour = "grey30"),
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 2)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    panel.grid.major = element_line(colour = "#E0E0E0"),  # ~ grey88
    panel.grid.minor = element_line(colour = "#EBEBEB")   # ~ grey92
  ) +
  coord_cartesian(xlim = c(1850, 2005), clip = "off")
#SAVE JPEG
ggsave("frise_fonds_1850_2005_simple.jpeg", p_frise_fonds_simple, width = 18, height = 10, dpi = 300, bg = "white")
#SAVE SVG
ggsave("frise_fonds_1850_2005_simple.svg", p_frise_fonds_simple, width = 18, height = 10, dpi = 300, bg = "white")

