## ---------------------------------------------------------##
##    Plot les evolutions des indicateurs                   ##
## ---------------------------------------------------------##

# ---------------------------------------------------------
# Densité des indicateurs
# ---------------------------------------------------------
df_long <- df_final %>%
  pivot_longer(
    cols = -TIME_PERIOD,
    names_to = "indicator",
    values_to = "value"
  )

ggplot(df_long, aes(x = value)) +
  geom_density() +
  facet_wrap(~ indicator, scales = "free") +
  theme_minimal() +
  labs(title = "Densité des indicateurs",
       x = "Valeur",
       y = "Densité")

# ---------------------------------------------------------
# Evolution temporelle des indicateurs 
# ---------------------------------------------------------
df_long <- df_final %>%
  pivot_longer(
    cols = -TIME_PERIOD,
    names_to = "indicator",
    values_to = "value"
  )

df_long <- df_long %>%
  mutate(year = substr(TIME_PERIOD, 1, 4))

ggplot(df_long, aes(x = TIME_PERIOD, y = value, group = indicator)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_x_discrete(
    breaks = df_long$TIME_PERIOD[df_long$year != dplyr::lag(df_long$year)] # un label par année
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(
    title = "Évolution des indicateurs dans le temps",
    x = "",
    y = ""
  )

df_per <- df_final %>% 
  select(TIME_PERIOD, marche_surrevaluation_PER) %>% 
  mutate(
    TIME_PERIOD = factor(TIME_PERIOD, levels = unique(TIME_PERIOD))  
  )

ggplot(df_per, aes(x = TIME_PERIOD, 
                   y = marche_surrevaluation_PER, 
                   group = 1)) +             
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(
    title = "Évolution PER dans le temps",
    x = "",
    y = ""
  )


