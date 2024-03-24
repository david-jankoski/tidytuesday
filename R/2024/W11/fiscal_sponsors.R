library("tidytuesdayR")
library("scales")
library("ggtext")
library("nationalparkcolors")
library("tidyverse")

tuesdata <- tidytuesdayR::tt_load(2024, week = 11)

fiscal_sponsor_directory <- tuesdata$fiscal_sponsor_directory

educational_projects_sponsors <-
  fiscal_sponsor_directory |>
  select(
    name,
    year_fiscal_sponsor,
    n_sponsored,
    project_types,
    eligibility_criteria,
    services
  ) |>
  mutate(
    is_project_educational = str_detect(project_types, "[Ed]ucation|[Dd]ata|[Ss]cience"),
    year = {
      # "1987" -> "19870101" -> 1987-01-01
      year_fiscal_sponsor |>
        as.character() |>
        str_c("0101") |>
        ymd()
    }
  )


# educational breakdown
educational_projects_sponsors |>
  count(is_project_educational) |>
  mutate(perc = n / sum(n))

educational_projects_sponsors <-
  educational_projects_sponsors |>
  filter(is_project_educational)


# https://github.com/katiejolly/nationalparkcolors
line_colour       <- park_palette("Saguaro")[1]
annotation_colour <- park_palette("Saguaro")[6]
top_point_col     <- park_palette("Everglades")[4]


# educational per year
educational_projects_sponsors |>
  group_by(year) |>
  summarise(n_sponsored = sum(n_sponsored)) |>
  mutate(
    pointcol = ifelse(year == as_date("2013-01-01"), top_point_col, line_colour)
  ) |>
  ggplot(aes(x = year, y = n_sponsored)) +
  geom_point(aes(colour = I(pointcol))) +
  geom_line(aes(group = 1), colour = line_colour) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, 200),
    labels = label_comma(),
    minor_breaks = NULL
  ) +
  annotate(
    "text",
    x = ymd(19850101),
    y = 600,
    label = "Past records \nhappened around 1975",
    hjust = 0,
    fontface = "bold",
    lineheight = 0.8,
    colour = annotation_colour
  ) +
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = data.frame(
      x    = ymd(19840601),
      xend = ymd(19780130),
      y    = 600,
      yend = 545
    ),
    curvature = -0.01,
    colour = annotation_colour
  ) +
  annotate(
    "text",
    x = ymd(20000101),
    y = 970,
    label = "The record was in 2013 with 956 projects",
    hjust = 1.02,
    fontface = "bold",
    colour = annotation_colour
  ) +
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = data.frame(
      x    = ymd(20000101),
      xend = ymd(20120330),
      y    = 970,
      yend = 970
    ),
    curvature = -0.01,
    colour = annotation_colour
  ) +
  labs(
    title = "Number of sponsored projects related to Education, Data or Science - by year",
    subtitle = "A projects counts as related if it contains one of the keywords in it's project types entries",
    caption = "Tidy Tuesday 2024 W11 David Jankoski",
    x = NULL,
    y = "# projects"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),
    plot.title    = element_markdown(colour = annotation_colour, face = "bold"),
    plot.subtitle = element_markdown(colour = annotation_colour, margin = margin(b = 8)),
    plot.caption  = element_markdown(colour = annotation_colour, margin = margin(t = 6))
  )


# n criteria for educational projects per year ---------------
educational_projects_sponsors |>
  select(name, year_fiscal_sponsor, n_sponsored, eligibility_criteria) |>
  mutate(
    criteria = str_split(eligibility_criteria, "\\|"),
    n_criteria = map_int(criteria, length)
  ) |>
  select(year_fiscal_sponsor, n_criteria) |>
  group_by(year_fiscal_sponsor) |>
  summarise(n_criteria = sum(n_criteria)) |>
  ggplot(aes(x = year_fiscal_sponsor, y = n_criteria)) +
  geom_point() +
  geom_line(aes(group = 1))

# table with top criteria per project -------------
library("gt")

educational_projects_sponsors |>
  select(name, year_fiscal_sponsor, project_types, eligibility_criteria) |>
  mutate(
    across(
      project_types:eligibility_criteria,
      ~ str_split(.x, "\\|")
    )
  ) |>
  unnest_longer(col = eligibility_criteria) |>
  unnest_longer(col = project_types) |>
  count(project_types, eligibility_criteria, sort = T) |>
  group_by(project_types) |>
  slice_max(order_by = n, n = 10, with_ties = F) |>
  filter(project_types %in% c("Education", "Arts and culture")) |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = project_types
    )
  )


# matrix / tiles between projects and criteria --------------
educational_projects_sponsors |>
  select(name, year_fiscal_sponsor, project_types, eligibility_criteria) |>
  mutate(
    across(
      project_types:eligibility_criteria,
      ~ str_split(.x, "\\|")
    )
  ) |>
  unnest_longer(col = eligibility_criteria) |>
  unnest_longer(col = project_types) |>
  count(project_types, eligibility_criteria, sort = T) |>
  slice_head(n = 100) |>
  mutate(
    project_types = str_to_title(project_types),
    eligibility_criteria = str_sub(eligibility_criteria, end = 30) |> str_c("...")
  ) |>
  ggplot(aes(x = eligibility_criteria, y = project_types)) +
  geom_tile(aes(fill = n)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)
  )



# graph between projects and criteria ---------------
library("tidygraph")
library("ggraph")

educational_projects_sponsors |>
  select(name, year_fiscal_sponsor, project_types, eligibility_criteria) |>
  mutate(
    across(
      project_types:eligibility_criteria,
      ~ str_split(.x, "\\|")
    )
  ) |>
  unnest_longer(col = eligibility_criteria) |>
  unnest_longer(col = project_types) |>
  count(project_types, eligibility_criteria, sort = T) |>
  filter(
    project_types %in% c("Education", "Arts and culture")
  ) |>
  group_by(project_types) |>
  slice_max(order_by = n, n = 10) |>
  ungroup() |>
  as_tbl_graph(directed = T) |>
  ggraph() +
  geom_node_point() +
  geom_node_label(aes(label = name)) +
  coord_flip() +
  geom_edge_link(arrow = grid::arrow())
# geom_edge_link(aes(edge_width = log10(n), arrow = grid::arrow())







# project types sponsored by year ?
projects_by_year <-
  educational_projects_sponsors |>
  select(name, year_fiscal_sponsor, project_types, n_sponsored) |>
  mutate(
    project_types = str_split(project_types, "\\|")
  ) |>
  unnest_longer(col = project_types) |>
  group_by(project_types, year_fiscal_sponsor) |>
  summarise(n_sponsored = sum(n_sponsored)) |>
  ungroup() |>
  mutate(
    year = {
      year_fiscal_sponsor |>
        as.character() |>
        str_c("0101") |>
        ymd()
    },
    project_types = str_trim(project_types)
  ) |>
  select(-year_fiscal_sponsor) |>
  relocate(year, .before = n_sponsored) |>
  add_count(project_types, name = "n_sponsored_total")


projects_by_year |>
  mutate(project_types = {
    fct_reorder(project_types, n_sponsored_total) |>
      fct_lump_n(n = 7)
  }) |>
  ggplot(aes(x = year, y = n_sponsored, colour = project_types)) +
  geom_point() +
  geom_line(aes(group = project_types)) +
  facet_wrap(~ project_types, scales = "free_y") +
  theme(
    legend.position = "none"
  )
