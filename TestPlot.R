Molecules <- c("L-Glutamine","L-Glutamine","L-Glutamine","L-Glutamine","L-Glutamine","L-Glutamine","D-Mannose","D-Mannose","D-Mannose","D-Mannose","D-Mannose","D-Mannose")
Samples <- c("HepG2 1 13","HepG2 1 13","HepG2 1 13","HepG2 1 13","HepG2 1 ctrl", "HepG2 1 ctrl", "HepG2 1 13","HepG2 1 13","HepG2 1 13","HepG2 1 13","HepG2 1 ctrl", "HepG2 1 ctrl")
Values <- c(10,20,30,560,134,467,103,123,579,122,681,457)

df <- data.frame(Molecules, Samples, Values)

# Wanneer y as.factor(Values) is klopt de schaal op de y-as niet meer maar plot hij de bars wel goed
# Als y geen factor is kloppen de bars niet meer.
p <- ggplot(df, aes(x = Samples, y=Values, fill = Samples, group = factor(Values)))+
  geom_bar(position = position_dodge(), stat = "identity", colour="black", size = 0.2, width = 0.2) +
  facet_wrap(~Molecules) + 
  scale_y_continuous(c(0,1000))


print(p)