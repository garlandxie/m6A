# libraries --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(here)
library(janitor)

# import -----------------------------------------------------------------------

m6AR <- read.csv(here("data", "m6A.csv"), header = TRUE)

# check packaging --------------------------------------------------------------

str(m6AR)
head(m6AR, n = 5)
tail(m6AR, n = 5)

# clean ------------------------------------------------------------------------

m6AR_tidy <- m6AR %>%
  janitor::clean_names() %>%
  select(
    date, 
    ploidy, 
    treatment, 
    pot, 
    replicate, 
    x_of_m6a_in_total_rna
    ) %>%
  mutate(
    date = factor(
      date,
      levels = c("Control","Treatment","Recovery"),
      order = TRUE
      ),
    treatment = as_factor(treatment),
    pot = as_factor(pot), 
    replicate = as_factor(replicate)
  )
  
# sanity check
str(m6AR_tidy)

# summarize - prep for bar graph
m6AR_summ <- m6AR_tidy %>%
  group_by(date, ploidy, treatment) %>%
  summarize(
    rep      = n(),
    mean_rna = mean(x_of_m6a_in_total_rna, na.rm = TRUE), 
    std_rna  = sd(x_of_m6a_in_total_rna, na.rm = TRUE)
  )

# anohter sanity check
str(m6AR_summ)

# plot -------------------------------------------------------------------------

bar_plot <- m6AR_summ %>%
  ggplot(aes(x = treatment, y = mean_rna, fill = ploidy)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    aes(
      ymin = mean_rna - std_rna, 
      ymax = mean_rna + std_rna
      ),
    position = position_dodge()
    ) + 
  facet_wrap(~date) + 
  scale_fill_discrete(name = "Ploidy") + 
  labs(x = "Treatment", y = "Mean RNA %")

# save to disk -----------------------------------------------------------------

ggsave(
  filename = here("figures", "bar_plot_rna.png"),
  plot = bar_plot, 
  device = "png",
  unit = "in",
  width = 5, 
  height = 5
)
