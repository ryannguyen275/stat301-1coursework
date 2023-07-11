## L10


### Exercise 1
is.factor(gss_cat$marital)

levels(gss_cat$rincome)

income_lvl <- c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999", 
                "$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                "$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more",
                "Don't know", "No answer", "Not applicable", "Refused")

# overwrite variable with new levels
gss_clean <- gss_cat %>% 
  mutate(rincome = factor(rincome,
                          levels = income_lvl))

gss_cat$rincome <- factor(gss_cat$rincome,
                          levels = income_lvl)

# geom_bar since it's a list and we don't have the count
ggplot(gss_clean, aes(x = rincome))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Exercise 5

gss_party <- gss_cat %>%
  mutate(political = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>% 
  group_by(year) %>% 
  mutate(count = n()) %>% 
  group_by(year, political) %>% 
  mutate(prop = n()/count)

gss_party %>% 
  ggplot(aes(year, prop, color = political)) +
  geom_line() + 
  scale_x_continuous(breaks = c(2000:2014)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
levels(gss_cat$rincome)

### Exercise 6

gss_income <- gss_cat %>%
  mutate(incomes = fct_collapse(rincome,
                                other = c("No answer", "Don't know", "Refused", "Not applicable"),
                                lower = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
                                middle = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999"),
                                high = c("$10000 - 14999","$15000 - 19999", "$20000 - 24999", "$25000 or more")))

gss_income