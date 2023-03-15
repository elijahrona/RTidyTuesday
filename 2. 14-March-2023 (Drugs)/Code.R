library(tidyverse)
library(skimr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2023-03-14')
drugs <- tuesdata$drugs
rm(tuesdata)

skimmed_drugs <- skim(drugs)

sum(length(unique(drugs$active_substance)))

human_active_substance <- drugs %>%
  filter(category == "human")
human_active_substance <- human_active_substance$active_substance

veterinary_active_substance <- drugs %>%
  filter(category == "veterinary")
veterinary_active_substance <- veterinary_active_substance$active_substance

mutual_active_substance <- intersect(human_active_substance,veterinary_active_substance)
mutual_active_substance

mutual_df <- drugs %>%
  filter(active_substance %in% mutual_active_substance)

mutual_df %>%
  group_by(category) %>%
  summarise(count = length(category))

skimmed_mutual_df <- skim(mutual_df)

mutual_df2 <- mutual_df[c("active_substance","category","pharmacotherapeutic_group",
                          "medicine_name","marketing_authorisation_date",
                          "date_of_refusal_of_marketing_authorisation")]

sum(length(unique(mutual_df2$active_substance)))

mutual_df2 %>% 
  group_by(active_substance) %>% 
  summarise(human = filter(category == "human")%>%paste(unique(medicine_name), collapse = ', '),
            species = filter(category == "veterinary")%>%paste(unique(medicine_name), collapse = ', '))

human <- mutual_df2 %>%
  filter(category == "human") %>%
  group_by(`Name of Drug` = medicine_name) %>%
  summarise(`Drug Target` = paste(unique(pharmacotherapeutic_group)[1], collapse = ' & '),
            `Active Substance` = paste(unique(active_substance)[1], collapse = ' & '),
            `Authorization Date` = unique(marketing_authorisation_date)[1],
            `Rejection Date` = unique(date_of_refusal_of_marketing_authorisation)[1]) %>%
  mutate(Year = ifelse(is.na(`Authorization Date`)==TRUE,year(`Rejection Date`),year(`Authorization Date`))) %>%
  mutate(Remark = factor(ifelse(is.na(`Authorization Date`)==TRUE, "Rejected","Authorized"))) %>%
  mutate(Category = "human")

human <- human[!duplicated(human[3]),]

human$`Name of Drug`[human$`Name of Drug` == "Kinzalmono (previously Telmisartan Boehringer Ingelheim Pharma KG)"] <- "Kinzalmono"
human[7,2] <- "Antiepileptics"

veterinary <- mutual_df2 %>%
  filter(category == "veterinary") %>%
  group_by(`Name of Drug` = medicine_name) %>%
  summarise(`Drug Target` = paste(unique(pharmacotherapeutic_group)[1], collapse = ' & '),
            `Active Substance` = paste(unique(active_substance)[1], collapse = ' & '),
            `Authorization Date` = unique(marketing_authorisation_date)[1],
            `Rejection Date` = unique(date_of_refusal_of_marketing_authorisation)[1]) %>%
  mutate(Year = ifelse(is.na(`Authorization Date`)==TRUE,year(`Rejection Date`),year(`Authorization Date`))) %>%
  mutate(Remark = factor(ifelse(is.na(`Authorization Date`)==TRUE, "Rejected","Authorized")))%>%
  mutate(Category = "veterinary")

veterinary <- veterinary[!duplicated(veterinary[3]),]

veterinary[6,2] <- "Agents acting on the renin-angiotensin system"
veterinary[4,2] <- "Insulins and analogues for injection"

combined <- rbind(human,veterinary)

combined$`Name of Drug` <- factor(combined$`Name of Drug`)

human_medicines <- factor(human$`Name of Drug`)

ycolor <- ifelse(levels(combined$`Name of Drug`) %in% human_medicines, "#00BFC4", "#FF61CC")

approval_year <- combined %>%
  ggplot() +
  geom_tile(aes(x = Year, y = `Name of Drug`, fill = Remark), width = 1, height = 1, colour = "black", size = 1) +
  scale_x_continuous(breaks = 1997:2022, labels = 1997:2022) +
  scale_fill_manual(values=c("#00BA38", "#F8766D")) +
  labs(fill = "") +
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(b = 0, t = 0, r = 1, l = 1),
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black", face = "bold", size = 20),
    axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = ycolor),
    axis.title = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 20),
    legend.key.size = unit(0.7, 'cm'),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "#000000", linetype = 2))

human2 <- human[,c("Name of Drug","Drug Target","Active Substance")]
human_tab <- ggtexttable(human2, rows = NULL,
                         theme = ttheme(
                           colnames.style = colnames_style(color = "white", fill = "#00BFC4", size = 12),
                           tbody.style = tbody_style(color = "black", fill = c("#f2f2f2", "#d5f4f5")))) %>%
  tab_add_title("Drugs for People", face = "bold", size = 15, color = "#00BFC4")%>%
  table_cell_font(row = 3:9, column = 1:3, size = 12)

veterinary2 <- veterinary[,c("Name of Drug","Drug Target","Active Substance")]
vet_tab <- ggtexttable(veterinary2, rows = NULL,
                       theme = ttheme(
                         colnames.style = colnames_style(color = "white", fill = "#FF61CC", size = 12),
                         tbody.style = tbody_style(color = "black", fill = c("#f2f2f2", "#ffdef4")))) %>%
  tab_add_title("Drugs for Animals", face = "bold", size = 15, color = "#FF61CC") %>%
  table_cell_font(row = 3:9, column = 1:3, size = 12)

people <- image_read("people.png")
animals <- image_read("animals.png")

credit <- "Characters png from pngtree.com and Lovepik.com"
created <- "Dashboard: @Elijah_Rona"

dash_label <- "With over 1300 active substances used to make drugs according to data from the European Medicines Agency, only seven (7) are
used in medicines for humans and animals. This dashboard aims to list the seven active substances, share common drug names with
one of the seven substances, and state the use of the drug. Also, plotted is the year which each drug (for people and animals)
was either authorizued or rejected for marketing."

label_plot <- ggplot() + 
  xlim(0:1) +
  annotate("text", x = 0, y = 1, size=5.9, label = dash_label, hjust = 0) + 
  theme_void()

dashboard <- ggdraw() +
  draw_plot(label_plot, x = -0.03, y = 0.75, height = 0.3, width = 1) +
  draw_plot(approval_year, x = 0.15, y = 0.107, height = 0.5, width = 0.7) +
  draw_plot(human_tab, x = 0, y = 0.47, height = 0.5, width = 0.5) +
  draw_plot(vet_tab, x = 0.5, y = 0.47, height = 0.5, width = 0.5)+
  draw_line(
    x = c(0.005, 0.148, 0.148, 0.005,0.005),
    y = c(0.27, 0.27, 0.58, 0.58, 0.27),
    color = "#00BFC4", size = 2
  ) +
  draw_line(
    x = c(0.995, 0.852, 0.852, 0.995,0.995),
    y = c(0.27, 0.27, 0.58, 0.58, 0.27),
    color = "#FF61CC", size = 2
  ) +
  draw_image(people, x = -0.08, y = 0.27, width = 0.3, height = 0.3) +
  draw_image(animals, x = 0.81, y = 0.27, width = 0.2, height = 0.3) +
  draw_label(created, x = 0.01, y = 0.1, size = 15,
             vjust = 2, hjust = 0, fontface = "bold") +
  draw_label(credit, x = 0.01, y = 0.07, size = 13,
             vjust = 2, hjust = 0) +
  draw_label("Drugs", x = 0.01, y = 1.01, size = 30,
             vjust = 2, hjust = 0, fontface = "bold") +
  theme(plot.background = element_rect(fill="#f2f2f2", color="#f2f2f2"),
        panel.background = element_rect(fill="#f2f2f2", color="#f2f2f2"),
        panel.grid = element_blank())

ggsave2("dashboard.png",dashboard, width = 15, height = 12, dpi=700)
