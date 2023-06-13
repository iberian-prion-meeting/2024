library(dplyr)
library(reshape2)
library(gargle)
library(googlesheets4)
library(knitr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyr)
library(lubridate)
library(kableExtra)
library(gt)
library(gtExtras)

raw_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1DAeG4zs6R4TnbjZ9k0PwBQvDek5l4-aRSCtD16ctxE4/edit?usp=sharing", sheet = "Sheet1") %>% 
  mutate(Start = format(Start, format = "%H:%M"),
         End = format(End, format = "%H:%M"),
         Time = paste0(Start, "- ", End),
         Speaker = ifelse(is.na(Speaker), "", Speaker),
         Session = ifelse(is.na(Session), "", Session)) %>% 
  mutate(cnt = cumsum(Session != "")) %>% 
  group_by(Day, Start) %>% 
  mutate(Session = ifelse(Session == "", paste0(rep(" ", 1 + cnt), collapse = ""), Session)) %>% 
  ungroup()

start_day <- 10


cat('---\ntitle: "Programme"\n---', file = "programme.qmd")

tmp <- split(raw_dat, raw_dat[["Day"]]) %>% 
  lapply(function(i) { 
    cat("\n\n## ", start_day + unique(i[["Day"]]), "May 2023  {-}\n\n", file = "programme.qmd", append = TRUE)
    select(i, Time, Title, Speaker, Session) %>% 
      mutate(Session = ifelse(!grepl("^ ", x = Session), paste0("Session: ", Session), Session)) %>% 
      setNames(c("Time", "Title", "Speaker", "Session")) %>% 
      group_by(Session) %>% 
      gt() %>% 
      # cols_width(
      #   Title ~ px(350),
      #   Speaker ~ px(250)
      # ) %>% 
      gt_highlight_rows(
        rows = Title == "Coffee break" | Title == "Coffee break & Posters",
        fill = "bisque",
        bold_target_only = TRUE,
        target_col = Title
      ) %>% 
      tab_options(row.striping.include_table_body = TRUE,
                  row_group.background.color = "khaki1") %>% 
      as_raw_html %>% 
      cat(file = "programme.qmd", append = TRUE)
  })

pdf_dat <- mutate(raw_dat, Time = paste0(Start, " - ", End))

c(readLines("./programme-pdf/header.tex"),
  select(pdf_dat, Day, Session, Time, Title, Speaker, Session) %>% 
    mutate(Day = paste0(start_day + Day, "th May\\newline2023"),
           Title = escape_latex(Title)) %>% 
    mutate(Speaker = ifelse(!grepl(pattern = "^Oral", x = Title), 
                            ifelse(Speaker != "", paste0("\\textit{", Title, "}, ", "\\textbf{", Speaker, "}"), Title),
                                   Speaker)) %>% 
    select(-Title) %>% 
    kable(escape = FALSE, format = "latex", booktabs = TRUE) %>%
    column_spec(1L, width = "3em") %>% 
    #column_spec(2L, width = "7em") %>%
    column_spec(2L, width = "13em") %>%
    column_spec(3L, width = "7em") %>%
    column_spec(4L, width = "30em") %>%
    collapse_rows() %>% 
    print %>% 
    capture.output(),
  "\\end{document}") %>% 
  cat(file = "./programme-pdf/programme.tex", append = FALSE, sep = "\n")

system("pdflatex --shell-escape ./programme-pdf/programme.tex")
