
# Visual CV ---------------------------------------------------------------

# inspired by the code here 
# https://github.com/toebR/Own_R_Viz_Projects/tree/master/graphical_CV_template


# load packages -----------------------------------------------------------
# install.packages(c("cowplot", "extrafont", "tidyverse", "lubridate"))

library(cowplot)
library(extrafont)
library(tidyverse)
library(lubridate)


# some style elements -----------------------------------------------------

# font_import()
# loadfonts(device = "win")

# I reordered the colours so that the colours for my schools match the school's
# colors
colour_palette <- RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1, 3:9, 2, 10)]

# dates to be used for the plot -------------------------------------------

birth_date <- mdy("2-9-1994")
first_month <- rollback(birth_date, roll_to_first = T)
last_year <- year(today())

# plot data ---------------------------------------------------------------

life_details <-
  tibble(date = paste(paste(rep(1, 12),
                            month.abb),
                      rep(year(first_month):last_year, each = 12)) %>% 
           dmy(), # creates a vector of all the months from since your birthday and 
         # parses them as dates
         work = NA,
         ed = NA)  %>%
  filter(date >= first_month & date <= today()) %>% # remove months before you were born 
  # and month from the current year that 
  # haven't happened yet
  mutate(ed = fct_inorder(
    # any date ranges not defined are coded as "Nothing" 
    case_when(
      date >= "2000-09-01" & date <= "2012-06-01" ~ "School",
      date >= "2013-09-01" & date <= "2017-06-01" ~ "BA in Political Science",
      date >= "2017-09-01" & date <= "2018-10-01" ~ "MA in Applied Politics: Public Opinion",
      TRUE ~ "Nothing"
    )
  )) %>%
  mutate(work = fct_inorder(
    # any date ranges not defined are coded as "Nothing" 
    case_when(date >= "2011-07-01" & date <= "2011-08-01" ~ "Sailing Instructor",
              date >= "2012-07-01" & date <= "2012-08-01" ~ "Sailing Instructor",
              date >= "2014-01-01" & date <= "2015-06-01" ~ "Fabrication Lab Monitor", 
              date >= "2016-05-01" & date <= "2017-04-01" ~ "Vice-President Academic: Scarborough Campus Students' Union",
              date >= "2017-10-01" & date <= "2018-08-01" ~ "Research Assistant - Laurier Institute for the Study of Public Opinion and Policy", 
              date >= "2019-01-01" & date <= "2019-08-01" ~ "Research Assistant - Dept. of Political Science, McMaster University", 
              date >= "2019-10-01" ~ "Analyst at Statistics Canada",
              TRUE ~ "Nothing"
    )
  ))

# Main Plots --------------------------------------------------------------

education_plot <- life_details %>%
  ggplot(aes(x = month(date),
             y = year(date),
             color = ed)) +
  geom_point(size = 5,
             shape = 15,
             show.legend = FALSE) +
  scale_color_manual(values = c("#dedede", colour_palette[c(8, 9, 10)])) +
  geom_segment(aes(x = 1,
                   xend = 12,
                   y = 1993,
                   yend = 1993),
               colour = "black",
               arrow = arrow(type = "closed",
                             angle = 15,
                             length = unit(0.15, "inches"))) +
  geom_text(aes(x = 6, 
                y = 1992, 
                label = "Month"),
            colour = "black",
            family = "Roboto") +
  geom_segment(aes(x = 0,
                   xend = 0,
                   y = 2002,
                   yend = 2010),
               colour = "black",
               linetype = "dashed") +
  geom_segment(aes(x = 0,
                   xend = 0,
                   y = 1994,
                   yend = 2002),
               colour = "black") +
  geom_text(aes(x = 0.5, 
                y = 2021.5, 
                label = "Education"),
            colour = "black",
            hjust = 0,
            size = 5,
            family = "Roboto")  +
  theme_void()



job_plot <- life_details %>%
  ggplot(aes(x = month(date),
             y = year(date),
             color = work)) +
  geom_point(size = 5,
             shape = 15,
             show.legend = FALSE) +
  scale_color_manual(values = c("#dedede", colour_palette)) +
  geom_segment(aes(x = 1,
                   xend = 12,
                   y = 1993,
                   yend = 1993),
               colour = "black",
               arrow = arrow(type = "closed",
                             angle = 15,
                             length = unit(0.15, "inches"))) +
  geom_text(aes(x = 6, 
                y = 1992, 
                label = "Month"),
            colour = "black",
            family = "Roboto") +
  geom_segment(aes(x = 0,
                   xend = 0,
                   y = 2002,
                   yend = 2010),
               colour = "black",
               linetype = "dashed") +
  geom_segment(aes(x = 0,
                   xend = 0,
                   y = 1994,
                   yend = 2002),
               colour = "black") +
  geom_text(aes(x = 0.5, 
                y = 2021.5, 
                label = "Job Experience"),
            colour = "black",
            hjust = 0,   
            size = 5,
            family = "Roboto") +
  theme_void()



# labels to be put on the right -------------------------------------------

job_labels <-
  tibble(year = c(2011.5, 2014.5, 2016, 2017.5, 2019, 2020),
         work = c("Sailing Instructor: HMCS Ontario",
                  "Fabrication Lab Monitor: UTSC Fab Lab",
                  "Vice-President Academic: UTSC Students' Union",
                  "Research Assistant, Teaching Assistant: WLU",
                  "Research Assistant: McMaster",
                  "Analyst: Statistics Canada") %>%
           fct_reorder(., year)) %>%
  ggplot(aes(x = 1,
             y = year,
             label = work,
             colour = work)) +
  geom_text(hjust = 0,
            show.legend = F,
            family = "Roboto") +
  scale_x_continuous(limits = c(1, 8)) +
  scale_y_continuous(limits = c(1992, 2021.5)) +
  scale_color_manual(values = colour_palette) +
  theme_void()



# labels to be put on the left --------------------------------------------

school_labels <- 
  tibble(year = c(2007, 2015, 2018),
         work = c("Elementary and High School",
                  "BA in Political Science, University of Toronto",
                  "MA in Applied Politics: Public Opinion,\nWilfird Laurier University") %>%
           fct_reorder(., year)) %>%
  ggplot(aes(x = 1,
             y = year,
             label = work,
             colour = work)) +
  geom_text(hjust = 1,
            show.legend = F,
            family = "Roboto") +
  geom_text(aes(x = 1, 
                y = 2000, 
                label = "Year"),
            colour = "black",
            angle = 90,
            family = "Roboto") +
  scale_x_continuous(limits = c(-8, 1)) +
  scale_y_continuous(limits = c(1992, 2021.5)) +
  scale_color_manual(values = c(colour_palette[c(8, 9, 10)])) +
  theme_void()



# combine plots and save output -------------------------------------------

plot_grid(
  school_labels,
  education_plot,
  job_plot,
  job_labels,
  nrow = 1,
  rel_widths = c(1, .75, .75, 1)) +
  ggsave(filename = "visual_cv.png",
         height = 166,
         width = 310,
         units = "mm")
