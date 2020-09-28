library(tidyverse)
library(ggtext)
library(ragg)

ggplot(
  tibble(x = c("Area burnt", "Area covered with smoke"), y = c(63000, 5500000)),
  aes(x, y)
  ) + 
  geom_col(aes(fill = x), width = .8) + 
  geom_text(
    aes(label = glue::glue("{format(y, big.mark = ',')} km²"), color = x), 
    nudge_y = 150000,
    size = 5,
    fontface = "bold",
    family = "Oswald"
  ) +
  theme_minimal(base_size = 16, base_family = "Oswald") + 
  scale_color_manual(values = c("#c82626", "#d9c09e"), guide = "none") + 
  scale_fill_manual(values = c("#c82626", "#d9c09e"), guide = "none") + 
  scale_x_discrete( expand = c(.22, .22)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma_format(), limits = c(0, 5900000)) + 
  labs(x = NULL, y = NULL, title = "<span style='color:#9d1e1e'>Burnt land</span> and <span style='color:#c49c67'>plume of smoke</span> caused<br>by the Australian bushfires in 2019/20", subtitle = "(as of 6<sup>th</sup> of January 2020)") + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(color= "grey50"),
    plot.title = element_markdown(lineheight = 1.2, hjust = .5, face = "bold", margin = margin(20, 0, 3, 0)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(hjust = .5, margin = margin(0, 0, 10, 0))
  ) + 
  ggsave("australian_bars.pdf", width = 6, height = 9, device = cairo_pdf)



df <-
  tibble(
    movie = c("Star Wars", "Jumanji", "Pitch Perfect", "Greatest Showman", "Ferdinand"),
    gross = c(68, 38, 20, 10, 9)
  )

g <- ggplot(df, aes(fct_reorder(movie, -gross), gross)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_family = "Overpass") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
        plot.title.position = "plot")


g +
  geom_col(fill = "#28a87d", width = .9) +
  theme(axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Weekend gross (million USD)") +
  ggsave("bars_bad.pdf", width = 7, height = 6, device = cairo_pdf)


g +
  geom_col(fill = "#28a87d", width = .8) +
  theme(axis.text.x = element_text(size = 10, color = "black")) +
  labs(x = NULL, y = NULL, title = "Weekend gross in million USD of popular blockbusters") +
  ggsave("bars_good.pdf", width = 7, height = 5, device = cairo_pdf)


ggplot(df, aes(fct_reorder(movie, gross), gross)) +
  geom_col(fill = "#28a87d", width = .8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  theme_minimal(base_family = "Overpass") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
        plot.title.position = "plot",
        axis.text.y = element_text(color = "black", size = 10)) +
  labs(x = NULL, y = NULL, title = "Weekend gross in million USD of popular blockbusters") +
  coord_flip() +
  ggsave("bars_best.pdf", width = 7, height = 5, device = cairo_pdf)


ggplot(df, aes(fct_reorder(movie, gross), gross)) +
  geom_col(fill = "#28a87d", width = .8) +
  geom_text(
    aes(label = glue::glue("${gross}M")),
    family = "Overpass",
    fontface = "bold",
    color = "grey40",
    nudge_y = 1,
    hjust = 0,
    size = 3
  ) +
  geom_hline(yintercept = 0, size = .7,  color = "grey80") +
  scale_x_discrete(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 78)) +
  theme_minimal(base_family = "Overpass") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
        plot.title.position = "plot",
        axis.text.y = element_text(color = "black", size = 10),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, title = "Weekend gross in million USD of popular blockbusters") +
  coord_flip() +
  ggsave("bars_best_labs.pdf", width = 7, height = 5, device = cairo_pdf)


ggplot(df, aes(fct_reorder(movie, gross), gross, fill = movie == "Star Wars")) +
  geom_col(width = .8) +
  geom_text(
    aes(label = glue::glue("${gross}M")),
    family = "Overpass",
    fontface = "bold",
    color = "grey40",
    nudge_y = 1,
    hjust = 0,
    size = 3
  ) +
  ggtext::geom_richtext(
    data = tibble(x = factor("Jumanji", levels = c("Ferdinand", "Greatest Showman", "Pitch Perfect", "Jumanji", "Star Wars")), y = 63, label = "<b style='color:#28a87d;'>Star Wars</b> had<br>by far the **highest**<br>**weekend gross**<br>of the five movies"),
    aes(x, y, label = label),
    inherit.aes = F,
    family = "Overpass",
    size = 3.3,
    vjust = .35,
    label.color = NA
  ) +
  geom_hline(yintercept = 0, size = .7,  color = "grey80") +
  scale_x_discrete(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 78)) +
  scale_fill_manual(values = c("grey40", "#28a87d"), guide = "none") +
  theme_minimal(base_family = "Overpass") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
        plot.title.position = "plot",
        axis.text.y = element_text(color = "black", size = 10),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, title = "Weekend gross in million USD of popular blockbusters") +
  coord_flip() +
  ggsave("bars_best_labs_anno.pdf", width = 7, height = 5, device = cairo_pdf)
