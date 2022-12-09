################################################################################
## Convert a histogram to a  bar chart using ggplot_build function    ###
################################################################################

################################################################################
## Extract data from a ggplot                https://tinyurl.com/xr6v6wpw    ###
################################################################################
gg_viz                  <- tb_sc %>%
  filter(!is.na(GRAD_DEBT_MDN)) %>%
  ggplot() +
  aes(x = GRAD_DEBT_MDN, colour = name) +
  geom_histogram(bins = 10L, fill = "#112446", binwidth = 5000) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Debt Amount",
    title = "Median Graduate Debt",
    color = "Group"
  ) +
  scale_x_continuous(
    labels = scales::number_format(scale = 1, prefix = "$")
  ) +
  theme_gray()

gg_data                 <- ggplot_build(gg_viz)

dt_gg_data              <-as.data.table(gg_data[[1]])
################################################################################
# Row Names into Column                         https://tinyurl.com/2hewsbxd ###
################################################################################
dt_peer_group           <- tibble::rownames_to_column(peer.type, "row_names")
dt_peer_group$row_names <- as.integer(dt_peer_group$row_names)
names(dt_peer_group)[1] <- "group"
# ------------------------------------------------------------------------------
setkey(dt_peer_group, group)
setkey(dt_gg_data, group)
# ------------------------------------------------------------------------------
dt_sc.summary <- dt_gg_data[dt_peer_group]
# ------------------------------------------------------------------------------
p.1.1.3.1 <- dt_sc.summary %>%
  ggplot() +
  aes(x = x, y = y, fill = name) +
  geom_bar(stat = "identity") +
  labs(
    x = "Debt Amount",
    title = "Median Graduate Debt",
    color = "Group"
  ) +
  scale_x_continuous(
    labels = scales::number_format(scale = 1, prefix = "$")
  ) +
  scale_fill_manual(values = c("#000000", "#9EA2A2", "#BA0C2F")) +
  theme_gray()
# ------------------------------------------------------------------------------
p.1.1.3.1 <- transition_states(x, wrap = FALSE) +
    shadow_mark()