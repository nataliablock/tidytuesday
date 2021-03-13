### TidyTuesday, March 8th 2021
## Author: Natalia Block


#packages
pacman::p_load(tidyverse, tidytuesdayR, waffle, showtext, extrafont,hrbrthemes)

##Adding fonts
font_add_google(name="Fjalla One", family="fjalla")
font_add_google(name="Roboto", family="roboto")
showtext_auto()

#loading data
ttuesday<-tidytuesdayR::tt_load('2021-03-09')

movies<-ttuesday$movies


#Directors with highest number of movies tested
movies%>%
  group_by(director)%>%
  count()%>%
  arrange(desc(n))%>%
  top_n(10)%>%
  drop_na()


#Waffle chart
movies%>%
  select(director, binary)%>%
  filter(director %in% c("Steven Spielberg", "Martin Scorsese","Robert Zemeckis",
                         "Ridley Scott","Sam Raimi","Steven Soderbergh", "Tim Burton",
                         "Joel Schumacher", "Woody Allen", "Clint Eastwood"))%>%
  group_by(director)%>%
  count(binary)%>%
  ggplot(aes(fill=binary, values=n))+
  geom_waffle(color="white", size=0.25, n_rows=2)+
  facet_wrap(~director, nrow=2)+
  labs(title= "Directors & The Bechdel Test",
      caption = "Data: FiveThirtyEight\nVisualization: Natalia Block (Twitter @nataliamblock | Github nataliablock)",
       subtitle = "The Bechdel test evaluates gender bias in movies. A movie passes the test if it satisfies\n the three following criteria: there are at least two named women in the picture, they have\n a conversation with each other and that conversation is not about a male character.\n\nThe graph presents whether created by the top 10 analyzed directors in the dataset passed\n the test. Tim Burton and Woddy Allen were the only directors who passed the test in most\n of their movies.")+
  scale_fill_manual(name = NULL,values = c("#CC0033", "#3C989E"), labels=c("FAILED", "PASSED")) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(plot.background = element_rect(fill="#f3f3f3ff", color="#f3f3f3ff"),
        plot.title = element_text(family="fjalla", hjust=0.5, size=35),
        plot.subtitle = element_text(family="roboto",hjust = 0.5, size=15),
        plot.caption=element_text(family="roboto", size= 12),
        strip.text=element_text(family="roboto", size=13),
        legend.text=element_text(family="roboto", size=15),
        legend.position="top")

