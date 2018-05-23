# dados dengue no datasus

datasus0407 <- read.csv2("dengue_datasus0407.csv", skip = 4,
                         stringsAsFactors = FALSE) %>% as_tibble()

datasus0818 <- read.csv2("dengue_datasus0818.csv", skip = 4,
                         stringsAsFactors = FALSE) %>% as_tibble()

datasus0418 <- bind_rows(datasus0407, datasus0818)

gsub1 <- function(x) gsub("[..|/]", "", x)

gsub2 <- function(pattern, replacement, x) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ignore.case = TRUE)
  return(x)
}

from <- format(ISOdate(2000, 1:12, 1), "%B")
to <- 1:12 %>% stringr::str_pad(2, pad = "0") %>% paste0("01", .)

datasus0418 %<>% 
  mutate(date = Ano.mês.processamento %>% 
           gsub1 %>% 
           gsub2(from, to, x = .)) %>% 
  select(date, n = Internações) %>% 
  filter(!grepl("^20", date)) %>% 
  mutate(date = dmy(date))

datasus0418 %<>% 
  mutate(ano = year(date))

# dados população

pop0418 <- read.csv2("pop0418.csv", skip = 3) %>% as_tibble()

datasus0418 %<>% 
  left_join(x = ., 
            y = pop0418 %>% 
              select(ano = Ano, 
                     pop = População_residente),
            by = "ano") %>% 
  mutate(txi = n/pop * 100000)
