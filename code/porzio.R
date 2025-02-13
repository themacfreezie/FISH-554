library(here)
library(plotrix)
library(readxl)
library(tidyverse)

here::i_am("code/porzio.R")

survey <- read_excel(here("data", "dat_porzio", "Porzio et al 2011 kite diagrams.xlsx"), sheet = "Data clean3")

# kiteChart(survey)

DF <- as.data.frame(survey) %>% mutate(species = rownames(survey)) %>%
  pivot_longer(-species, names_to = "X_var", values_to = "values") %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  mutate(X_var = factor(X_var, levels = unique(X_var))) %>%
  mutate(NewY = as.numeric(species)*2) %>%
  mutate(normval = values / max(values))  %>%
  mutate(NewX = as.numeric(X_var))  

species <- as.data.frame(survey$species)
species <- species %>% mutate(species = row_number())

DF <- merge(DF, species, by=c("species"))

DF[[1]] <- NULL
colnames(DF)[6] <- "species"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

DF$group <- substrRight(DF$species, 2)
DF$group <- substr(DF$group, 1, 1)

# ponzio <- ggplot(DF, aes(x = NewX, y= species, fill = group))+
#   geom_ribbon(aes(ymin = NewY-normval, ymax = NewY+normval))+
#   scale_y_continuous(breaks = unique(DF$NewY), labels = levels(DF$species)) +
#   scale_x_continuous(breaks = unique(DF$NewX), labels = levels(DF$X_var), name = "") +
#   theme(legend.position="bottom") +  theme(legend.title=element_blank()) 
# ponzio
# 
# ponzio2 <- ggplot(DF, aes(x = X_var, y = species, fill = species)) +
#   theme(legend.position="none") +
#   labs(x = "") +
#   scale_x_discrete(breaks = c("A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1", "I1"))
# 
# ponzio3 <- ggplot(data=DF, aes(x=NewX, y=values)) + 
#   geom_area(color=NA, fill=DF$group) + 
#   facet_wrap(~species, labeller = label_wrap_gen(width=20)) +  #wrap with width 20
#   theme(strip.background = element_rect(fill=NA, color=NA),
#         panel.background = element_rect(fill=NA, color=group),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.spacing.x=unit(0, "lines"),
#         panel.spacing.y=unit(0, "lines")) + 
#   labs( x = "",
#         y= "Catch (million metric tons)")
# ponzio3
# 
# ggplot() +
#   geom_point(data=DF, aes(x=X_var, y=species, fill = group))
#   facet_wrap(~species, labeller = label_wrap_gen(width=20)) +  #wrap with width 20

ggplot(DF, aes(x = NewX, fill = group, color=species)) +
  geom_ribbon(aes(ymin = NewY-normval, ymax = NewY+normval)) +
  scale_y_continuous(breaks=unique(DF$NewY), labels = levels(DF$species)) +
  scale_x_continuous(breaks=unique(DF$NewX), labels = levels(DF$X_var), name = "") +
  theme(legend.position="none")

species_names <- as.vector(c(unique(DF$species)))

porzio <- ggplot(DF, aes(x = NewX, y = species)) +
  geom_ribbon(aes(ymin = NewY-normval, ymax = NewY+normval, fill = group)) +
  scale_y_discrete(breaks=unique(DF$NewY), labels = levels(DF$species), name = "") +
  scale_x_continuous(breaks=unique(DF$NewX), labels = levels(DF$X_var), name = "") +
  theme(legend.position="none") +
  labs(title = "Distribution of most abundant macroalgal species",
      subtitle = "Sampled across 27 sites")
porzio

ggsave(here("output", "ponzio.png"), plot=porzio, device="png", dpi=300, width = 15, height = 20)
