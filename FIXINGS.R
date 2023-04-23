###########################################################
cat("\014")
options(digits=16)
# setwd("~/DOING/tesouro-titulos")
source('./funs_TPF.R')

#########################################################################
cat('\n***** FIXING CDI *****\n')
DI_RAW  <- rbcb::get_series(c(r_252 = 4389), start_date = '2000-07-01')

DI <- DI_RAW |>
  filter(date > as.Date('2000-07-01')) |>
  mutate(fte_dia = round(FTE(r_252), 8) ) |>
  mutate(fte_fec = cumprod(fte_dia)) |>
  mutate(fte_abe = replace_na(dplyr::lag(fte_fec,1),1), .before = fte_fec) |>
  print()

# saveRDS(DI, 'FIXING_DI.rds')
write_csv2(DI, './FIXING/FIXING_DI.csv')

#########################################################################
cat('\n***** FIXING SELIC e VNA de LFT *****\n')
SELIC_RAW  <- rbcb::get_series(c(r_252 = 1178, meta = 432), start_date = '2000-07-01')

SELIC <- left_join(SELIC_RAW$r_252, SELIC_RAW$meta, by = c('date')) |>
  filter(date > as.Date('2000-07-01')) |>
  mutate(fte_dia = round(FTE(r_252), 8) ) |>
  mutate(fte_fec = cumprod(fte_dia)) |>
  mutate(fte_abe = replace_na(dplyr::lag(fte_fec,1),1), .before = fte_fec) |>
  mutate(VNA = trunc2(1000*fte_abe, 6)) |>
  mutate(VNA_PROJ = trunc2(1000*fte_abe*round(FTE(meta),8), 6)) |>
  print()

# saveRDS(SELIC, 'FIXING_SELIC.rds')
write_csv2(SELIC, './FIXING/FIXING_SELIC.csv')

#########################################################################
cat('\n***** FIXING IPCA e VNA de NTNB *****\n')
ipca_raw <- tibble(get_sidra(api = '/t/1737/n1/all/v/2266/p/all/d/v2266%2013')) |>
  select(`Mês (Código)`, Valor) |>
  rename(anomes = `Mês (Código)`,
         indice = Valor) |>
  mutate(date  = as.Date(paste0(anomes, '15'), '%Y%m%d'), .before = 1) |>
  select(-c(anomes)) |>
  print()

ipca_dtbase <- pull(filter(ipca_raw, date == as.Date('2000-06-15')), indice) |> print()
ult_data    <- max(ipca_raw$date) |> print()

ipca <- ipca_raw |>
  rows_append(tibble(date = fn_add_months(ult_data, 1))) |>
  mutate(MoM = RET(indice,1)) |>
  filter(date > as.Date('2000-07-01')) |>
  mutate(fte_ipca = round(replace_na(dplyr::lag(indice/ipca_dtbase,1),1),16)) |>
  mutate(VNA = trunc2(1000*fte_ipca,6)) |>
  print()

# saveRDS(IPCA, 'FIXING_IPCA.rds')
write_csv2(ipca, './FIXING/FIXING_IPCA.csv')

#         #########################################################################
#         expec <- get_monthly_market_expectations('IPCA', start_date = '2022-01-01') |>
#           # select(date, reference_date, median, base) |>
#           filter(base == 1) |>
#           mutate(reference_date = as.Date(paste0(reference_date, '-15'))) |>
#           select(date, reference_date, median, mean) |>
#           mutate(diff = (reference_date - date) ) |>
#           filter(diff < ddays(30)) |>
#           group_by(date) |>
#           filter(row_number(diff) == 1) |>
#           ungroup() |>
#           arrange(date) |>
#           print()
#
#         expec1 <- expec |>
#           print()
#
#         tmp <- left_join(expec1, VNA_NTNB1, join_by(reference_date == date)) |>
#           mutate(DT_LIQ = add.bizdays(date, 1, 'Brazil/ANBIMA')) |>
#           mutate(DC_IPCA_num = as.numeric(DT_LIQ - reference_date)) |>
#           mutate(DC_IPCA_den = as.numeric((reference_date %m+% months(1)) - reference_date)) |>
#           mutate(fl_pub = ifelse(diff > 0, 1, 0)) |>
#           mutate(pro_rata = (1+round(median,2)/100)^trunc2(DC_IPCA_num/DC_IPCA_den, 14)) |>
#           mutate(VNA_PROJ = trunc2(VNA*pro_rata,6)) |>
#           # select(date, reference_date, mean, DC_IPCA_num, VNA, VNA_PROJ) |>
#           select(-c(mean)) |>
#           filter(date > as.Date('2023-01-01')) |>
#           print()
#
#
#         # tmp2 <- tmp |>
#         #   get_dupes(date) |>
#         #   print()
#
#
#


#########################################################################
cat('\n*************************')
cat('\n***** FIM DO SCRIPT *****\n')
cat('*************************\n')
