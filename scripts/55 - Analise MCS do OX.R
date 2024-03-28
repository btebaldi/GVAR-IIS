rm(list = ls())

library(ggplot2)

tbl.results <- readRDS("tbl.results.RDS")

tbl.results %>% 
  filter(!is.na(Critical)) %>% summary()


tbl.results %>% 
  filter(!is.na(Critical)) %>% 
  # filter(MCS_pValue >= 0.9) %>% 
  ggplot() +
  geom_bar(aes(x = Model), fill = "blue", alpha=0.5) + 
  facet_wrap(~Combustivel, ncol = 1) + 
  labs(title = "Model confidence set",
       subtitle = "Model frequency on MCS by fuel type")


ggsave(filename = "./graficos/MCS - Gafico1.png",
       plot = last_plot(),
       units = "in",
       width = 8, height = 6,
       dpi = 100)
