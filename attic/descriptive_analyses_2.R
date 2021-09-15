# field goal percentage

p <- ggplot(box, aes(x = order, y = fg_pct, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Field Goal Percentage") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away"))

ggsave(here("figs", "fg_pct.jpg"))

# three-point field goal percentage

p <- ggplot(box, aes(x = order, y = fg3_pct, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Three-point Field Goal Percentage") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away"))

ggsave(here("figs", "fg3_pct.jpg"))

# field goal attempts

p <- ggplot(box, aes(x = order, y = fga, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Field Goal Attempts") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away"))

ggsave(here("figs", "fta.jpg"))

# three-point field goal attempts

p <- ggplot(box, aes(x = order, y = fg3a, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Three-point Field Goal Attempts") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away"))

ggsave(here("figs", "fg3a.jpg"))

# free throws

p <- ggplot(box, aes(x = order, y = ft, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Free Throws") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away"))

ggsave(here("figs", "ft.jpg"))

# personal fouls

p <- ggplot(box, aes(x = order, y = pf, fill = venue))

p + geom_boxplot() +
  labs(x = "Season",
       y = "Personal Fouls") +
  guides(fill=guide_legend(title="Venue"))  +
  scale_fill_discrete(labels = c("Home", "Away")) +
  scale_y_continuous(breaks=seq(6,42,4))

max(box$pf)
min(box$pf)

ggsave(here("figs", "pf.jpg"))