###############################################################################
# AUTHOR: PAULO FERREIRA NAIBERT
# WEBSITE: https://pfnaibert.github.io/
###############################################################################

###############################################################################
# pacotes
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(here)

library(bizdays)
# holidays('Brazil/ANBIMA')
# holidays('Brazil/B3')

library(rbcb)
library(sidrar)
library(GetTDData)
library(rb3)

###############################################################################
# CARTA CIRCULAR 3498
# https://www.bcb.gov.br/estabilidadefinanceira/exibenormativo?tipo=Carta%20Circular&numero=3498
# DRM INSTRUCOES DE PREENCHIMENTO
# https://www.bcb.gov.br/estabilidadefinanceira/leiautedocumentoDRM

verts_pad_3498 <-c(21,42,63,126,seq(1,5)*252, 2520)
verts_pad_DRM  <-c(1,21,42,63,126,seq(1,5)*252, 2520, 2521)
verts_pad_3876 <-c(1,21,42,63,126,seq(1,5)*252, 2520, 2521)

###############################################################################
# ================================
# https://analisemacro.com.br/htmls/post-Precificando-NTNF.html

# ================================
# LEGISLAÇÂO

# DECRETO Nº 9.292, DE 23 DE FEVEREIRO DE 2018
# https://www.planalto.gov.br/ccivil_03/_Ato2015-2018/2018/Decreto/D9292.htm

# https://www.gov.br/tesouronacional/pt-br/divida-publica-federal/mercado-interno/titulos-da-divida-interna
# LEI No 10.179, DE 6 DE FEVEREIRO DE 2001.
# http://www.planalto.gov.br/ccivil_03/LEIS/LEIS_2001/L10179.htm

# Metodologia de Cálculo dos Títulos da Dívida Interna
# https://www.tesourotransparente.gov.br/publicacoes/metodologia-de-calculo-dos-ttulos-da-divida-interna/2008/26
# https://sisweb.tesouro.gov.br/apex/f?p=2501:9::::9:P9_ID_PUBLICACAO:26310

#
# https://www.bcb.gov.br/estabilidadefinanceira/leiautedocumentoscrd

# https://www.anbima.com.br/pt_br/informar/taxas-de-titulos-publicos.htm

###########################################################################################
###########################################################################################
# TABELA PARA ARREDONDAMENTO/TRUNCAGEM
#
#   |==================================================|=====================|===========|
#   |                                                  | Cálculo em Base 100 | Base 1000 |
#   |---------|----------------------------------|-----|-----|-------|-------|-----------|
#   |Critério | Variável                         | LTN | LFT | NTN-B | NTN-C |   NTN-F   |
#   |---------|----------------------------------|-----|-----|-------|-------|-----------|
#   |   A     | Juros Semestrais                 | --  | --  |   6   |   6   |     5     |
#   |   A     | Projeções                        | --  | --  |   2   |   2   |    --     |
#   |   A     | Fator Acumulado Taxa Selic       | --  | 16  |  --   |  --   |    --     |
#   |   A     | Fluxo de Pagamento Descontados   | --  | --  |  10   |  10   |     9     |
#   |   T     | Fator Pro Rata (projeções)       | --  | --  |  14   |  14   |    --     |
#   |   T     | Fator Acumulado Índice de Preços | --  | --  |  16   |  16   |    --     |
#   |   T     | Taxa de Retorno (% aa)           |  4  |  4  |   4   |   4   |     4     |
#   |   T     | VNA / VNA projetado              | --  |  6  |   6   |   6   |    --     |
#   |   T     | PU                               |  6  |  6  |   6   |   6   |     6     |
#   |   T     | Exponencial de Dias              | 14  | 14  |  14   |  14   |    14     |
#   |   T     | Cotação (%)                      | --  |  4  |   4   |   4   |    --     |
#   |   T     | Valor Financeiro                 |  2  |  2  |   2   |   2   |     2     |
#   |=========|==========================================================================|
#
###########################################################################################
###########################################################################################

###########################################################################################
###########################################################################################
###########################################################################################
# vertices       <- c(1,21,42,63,126,252,504,756,1008,1260,2520,2521)/252
# vertices_names <- c('1D','1M','2M','3M','6M','1Y','2Y','3Y','4Y','5Y','10Y','>10Y')
###########################################################################################
###########################################################################################
###########################################################################################

#########################################################################
num2pc <- function(number){
  paste0(sprintf('%.2f', 100*number), '%')
}

#########################################################################
fn_inimes <- function(my_date){
floor_date(my_date, 'month')
}

#########################################################################
fn_inimes_du <- function(my_date){
adjust.next(floor_date(my_date, 'month'), 'Brazil/ANBIMA')
}

#########################################################################
fn_fimmes <- function(my_date){
ceiling_date(my_date, 'month') - 1
}

#########################################################################
fn_fimmes_du <- function(my_date){
add.bizdays(ceiling_date(my_date, 'month'), -1, 'Brazil/ANBIMA')
}

#########################################################################
fn_add_months <- function(my_date, number){
my_date %m+% months(number)
}

#########################################################################
# TRUNC2
trunc2 <- function(number, ndecimal){
  trunc(number*(10^ndecimal))/10^ndecimal
}

#########################################################################
# RET
RET <- function(x, nlag){
  x/dplyr::lag(x, nlag) - 1
}

#########################################################################
# diff
DIFF_LAG <- function(x, nlag){
  x-dplyr::lag(x, nlag)
}

#########################################################################
# FTE
FTE <- function(RATE, TIME = 1, DAYCOUNT = 252){
  (1+RATE/100)^(trunc2(TIME/DAYCOUNT, 14))
}

#########################################################################
# RATE_BULLET
RATE_BULLET <- function(PV=1, FV, TIME, DAYCOUNT = 252){
  100*((FV/PV)^(DAYCOUNT/TIME)-1)
}

#########################################################################
# FWD_RATE
FWD_RATE <- function(RATE1, RATE2, TIME1, TIME2, DAYCOUNT = 252){
  RATE_BULLET(PV=1, FV = FTE(RATE2, TIME2)/FTE(RATE1, TIME1), TIME = (TIME2 - TIME1))
}

#########################################################################
# FTE_LIN
FTE_LIN <- function(RATE, TIME, DAYCOUNT = 360){
  (1+(RATE/100)*trunc2(TIME/DAYCOUNT, 14))
}

#########################################################################
# PV
PV_BULLET <- function(FV=1, RATE, TIME, DAYCOUNT = 252, AT = NULL, nsmall = 6){

  PV <- FV/(1+RATE/100)^(trunc2(TIME/DAYCOUNT, 14))

  if(is.null(AT)){return(PV)}
  else if(AT == 'T'){return(trunc2(PV, nsmall))}
  else if(AT == 'A'){return(round(PV, nsmall))}
}

#########################################################################
# FV
FV_BULLET <- function(PV=1, RATE, TIME, DAYCOUNT = 252, AT = NULL, nsmall = 6){
  FV <- PV*(1+RATE/100)^(trunc2(TIME/DAYCOUNT, 14))

  if(is.null(AT)){return(FV)}
  else if(AT == 'T'){return(trunc2(FV, nsmall))}
  else if(AT == 'A'){return(round(FV, nsmall))}
}

#########################################################################
# COTACAO
COTACAO <- function(RATE, TIME){
  PV_BULLET(FV=1, RATE, TIME, DAYCOUNT = 252, AT = 'T', nsmall = 6)
}

#########################################################################
# DURTATION
DURATION <- function(PV_VEC, TIME_VEC){
  as.numeric(crossprod(PV_VEC, TIME_VEC))/sum(PV_VEC)
}

#########################################################################
# MODIFIED DURTATION
DURMOD <- function(PV_VEC, TIME_VEC, ytm){
  -DURATION(PV_VEC, TIME_VEC)/(1+ytm/100)
}

#########################################################################
# DV01
DV01 <- function(PV_VEC, TIME_VEC, ytm){
  1e-4*as.numeric(crossprod(PV_VEC, TIME_VEC))/(1+ytm/100)
}

#########################################################################
# CONVEXITY
CONVEXITY <- function(PV_VEC, TIME_VEC, ytm){
  as.numeric(crossprod(TIME_VEC*(1+TIME_VEC), PV_VEC))/(sum(PV_VEC)*(1+ytm/100)^2 )
}

#########################################################################
# APPROX_QUAD
APPROX_QUAD <- function(PV_VEC, TIME_VEC, ytm, SHOCK_BPS){
  SHOCK  <- SHOCK_BPS*1e-4
  DF_YTM <- 1/(1+ytm/100)

  -SHOCK*as.numeric(crossprod(TIME_VEC, PV_VEC))*DF_YTM +
  0.5*(as.numeric(crossprod(TIME_VEC*(1+TIME_VEC), PV_VEC)))*(SHOCK*DF_YTM)^2
}

#########################################################################
# PV_YTM
PV_YTM <- function(ytm, FV_VEC, TIME_VEC){
  sum(FV_VEC/(1+ytm)^TIME_VEC)
}

#########################################################################
# NPV_YTM
NPV_YTM <- function(ytm, FV_VEC, TIME_VEC, PV_TARGET){
  PV_YTM(ytm, FV_VEC, TIME_VEC) - PV_TARGET
}

#########################################################################
FN_YTM <- function(FV_VEC, TIME_VEC, PV_TARGET, interval = c(0,2)){
  uniroot(NPV_YTM, FV_VEC, TIME_VEC, PV_TARGET,
          interval = interval, check.conv = TRUE,
          tol = .Machine$double.eps)
}

#########################################################################
# PV_YTM_PRIME
PV_YTM_PRIME <- function(ytm, FV_VEC, TIME_VEC){
  -sum(FV_VEC*TIME_VEC/(1+ytm)^(TIME_VEC-1))
}

#########################################################################
# FIND YTM
FN_YTM_NR <- function(FV_VEC, TIME_VEC, PV_TARGET, TOL = .Machine$double.eps, INI_GUESS = 0.12){
  # FIND YTM BY NEWTON-RAPHSON

  xi <- INI_GUESS
  yi <- NPV_YTM(xi, FV_VEC, TIME_VEC, PV_TARGET)
  di <- PV_YTM_PRIME(xi, FV_VEC, TIME_VEC)
  niter <- 0

while ( (abs(-yi) > TOL) & (niter < 100) ) {
  niter <- niter + 1
  xi <- (xi - yi/di)
  yi <- NPV_YTM(xi, FV_VEC, TIME_VEC, PV_TARGET)
  di <- PV_YTM_PRIME(xi, FV_VEC, TIME_VEC)
}

list(N_ITER = niter,
     YIELD = 100*xi,
     NPV = yi)
}

#########################################################################
# preco LFT
LFT <- function(DT_REF, DT_VCTO, RATE, LIQ = 0, VNA){
  DT_LIQ      <- add.bizdays(DT_REF, LIQ, 'Brazil/ANBIMA')
  DT_VCTO_ADJ <- adjust.next(DT_VCTO, 'Brazil/ANBIMA')
  TIME        <- bizdays(DT_LIQ, DT_VCTO_ADJ, 'Brazil/ANBIMA')

  # RESULT
  trunc2(VNA*COTACAO(RATE, TIME),6)
}

#########################################################################
# preco LTN
LTN <- function(DT_REF, DT_VCTO, RATE, LIQ = 0){
  DT_LIQ      <- add.bizdays(DT_REF, LIQ, 'Brazil/ANBIMA')
  DT_VCTO_ADJ <- adjust.next(DT_VCTO, 'Brazil/ANBIMA')
  TIME        <- bizdays(DT_LIQ, DT_VCTO_ADJ, 'Brazil/ANBIMA')

  # RESULT
  PV_BULLET(FV = 1000, RATE = RATE, TIME = TIME, AT = 'T', nsmall = 6)
}

#########################################################################
# CF NTNF (FACA)
CF_NTNF <- function(DT_REF, DT_VCTO, RATE, LIQ = 0){

  DT_LIQ    <- add.bizdays(DT_REF, LIQ, 'Brazil/ANBIMA')
  PMT_dates <- adjust.next(c(rev(seq.Date(DT_VCTO, DT_LIQ, by = '-6 month'))), 'Brazil/ANBIMA')

# 1000*sqrt(1+10%) Truncado na 5a casa e 48.80885
# 1000*sqrt(1+10%) Arredondado na 6a casa e 48.808850
  CF_TBL <- tibble(
    DT_REF  = DT_REF,
    DT_LIQ  = DT_LIQ,
    DT_VCTO = c(PMT_dates, tail(PMT_dates,1)),
    PMT     = c(rep(48.80885, NROW(PMT_dates)), 1000) ) |>
    mutate(DU = bizdays(DT_LIQ, DT_VCTO, 'Brazil/ANBIMA'), .after = DT_VCTO) |>
    mutate(PV_PMT = PV_BULLET(FV = PMT, RATE = RATE, TIME = DU), .after = PMT) |>
    filter(DU > 0)

return(CF_TBL)
}

#########################################################################
# CF NTNF (FACA)
CF_NTNF1 <- function(DT_REF, DT_VCTO, RATE, LIQ = 0, CURVA){

CF_TBL <- left_join(CF_NTNF(DT_REF, DT_VCTO, RATE, LIQ = 0),
                    CURVA, join_by(DU == biz_days)) |>
  mutate(PV_DI = PMT/FTE(r_252, DU))

return(CF_TBL)
}

#########################################################################
# preco NTNF (FACA)
NTNF <- function(DT_REF, DT_VCTO, RATE, LIQ = 0){

CF_TBL   <- CF_NTNF(DT_REF, DT_VCTO, RATE, LIQ = 0)

list(
  CF_TBL = CF_TBL,
  PRECO  = trunc2(sum(CF_TBL$PV_PMT), 6)
  )
}

#########################################################################
# preco NTNF (FACA)
NTNF2 <- function(DT_REF, DT_VCTO, RATE, CURVA = NULL, LIQ = 0){

DT_LIQ   <- add.bizdays(DT_REF, LIQ, 'Brazil/ANBIMA')
CF_TBL   <- CF_NTNF(DT_REF, DT_VCTO, RATE, CURVA, LIQ = 0)
TIME_VEC <- CF_TBL$DU/252

if( is.null(CURVA) ){
  PV_VEC <- CF_TBL$PV_PMT
  YTM    <- RATE
} else{
  PV_VEC <- CF_TBL$PV_DI
  YTM    <- FN_YTM(FV_VEC = CF_TBL$PMT, TIME = TIME_VEC,
                   PV_TARGET = trunc2(sum(CF_TBL$PV_DI), 6))$root
}

  NTNF_OBJ = list(
    CF_TBL       = CF_TBL,
    PRECO        = trunc2(sum(CF_TBL$PV_PMT), 6),
    YTM          = YTM,
    DURATION     = DURATION(PV_VEC, TIME_VEC),
    DURATION_MOD = DURMOD(PV_VEC, TIME_VEC, YTM),
    DV01         = DV01(PV_VEC, TIME_VEC, YTM),
    CONVEXIDADE  = CONVEXITY(PV_VEC, TIME_VEC, YTM)
  )

  return(NTNF_OBJ)
}

#########################################################################
# CF NTNB (BOLA)
CF_NTNB <- function(mydata){

ref_date   <- mydata$ref_date
liq_date   <- mydata$liq_date
matur_date <- mydata$matur_date
yield      <- mydata$yield_bid
VNA        <- mydata$VNA

PMT_dates <- adjust.next(c(rev(seq.Date(matur_date, liq_date, by = '-6 month'))), 'Brazil/ANBIMA')

# 100*sqrt(1+6%) Truncado na 6a casa e 2.956301
# sqrt(1+6%) Arredondado na 8a casa eh 0.02956301
# (multiplica por VNA e temos o resultado da PMT de juros)

CF_tbl <- tibble(ref_date  = ref_date,
                 liq_date  = liq_date,
                 PMT_date  = c(PMT_dates, tail(PMT_dates,1)),
                 PMT       = c(rep(2.956301, NROW(PMT_dates)), 100),
                 yield_bid = yield) |>
  mutate(DU = bizdays(liq_date, PMT_date, 'Brazil/ANBIMA'), .after = PMT_date) |>
  mutate(PV_PMT = round(fn_PV_bullet(PMT, yield, DU),10)) |>
  filter(DU > 0)

return(CF_tbl)
}

#########################################################################
# price NTNB (BOLA)
NTNB <- function(ref_date, matur_date, yield, VNA){

liq_date <- add.bizdays(ref_date, 1, 'Brazil/ANBIMA')

PMT_dates <- adjust.next(c(rev(seq.Date(matur_date, ref_date, by = '-6 month'))), 'Brazil/ANBIMA')

# 100*sqrt(1+6%) Truncado na 6a casa e 2.956301
# sqrt(1+6%) Arredondado na 8a casa eh 0.02956301
# (multiplica por VNA e temos o resultado da PMT de juros)

CF_tbl <- tibble(ref_date  = ref_date,
                 liq_date  = liq_date,
                 PMT_date  = c(PMT_dates, tail(PMT_dates,1)),
                 PMT       = c(rep(2.956301, NROW(PMT_dates)), 100),
                 yield_bid = yield) |>
  mutate(DU = bizdays(liq_date, PMT_date, 'Brazil/ANBIMA'), .after = PMT_date) |>
  mutate(PV_PMT = round(fn_PV_bullet(PMT, yield, DU),10)) |>
  filter(DU > 0)

trunc2(trunc2(sum(CF_tbl$PV_PMT),4)*VNA/100,6)
}
#########################################################################
VERTS_3498 <- function(my_df){

vert_1 <- my_df |>
  filter(vert >= 21 & vert <= 2520) |>
  mutate(find_nearest_vert1(vert, verts_pad_3498)) |>
  mutate(gap_size = vert_pos - vert_ant) |>
  mutate(pc_ant = ifelse(gap_size == 0, 1, (vert_pos - vert)/gap_size),
         pc_pos = ifelse(gap_size == 0, 0, (vert - vert_ant)/gap_size) ) |>
  select(-c(gap_size))

vert_0 <- my_df |>
  filter(vert < 21) |>
  mutate(vert_ant = NA, vert_pos = 21) |>
  mutate(pc_ant = NA, pc_pos = vert/21 )

vert_2520 <- my_df |>
  filter(vert > 2520) |>
  mutate(vert_ant = 2520, vert_pos = NA) |>
  mutate(pc_ant = vert/2520, pc_pos = NA)

bind_rows(vert_1, vert_0, vert_2520) |>
  mutate(CF_ant = round(pc_ant*CF,2),
         CF_pos = round(pc_pos*CF,2)) |>
  select(-c(pc_ant, pc_pos))
}

#########################################################################
CF_3498 <- function(my_df){
vert_3498 <- VERTS_3498(my_df)

left_join(tibble(vert = verts_pad_3498),
bind_rows( vert_3498 |>
             select(vert_ant, CF_ant) |>
             rename(vert = vert_ant, CF = CF_ant),
           vert_3498 |>
             select(vert_pos, CF_pos) |>
             rename(vert = vert_pos, CF = CF_pos) ) |>
  group_by(vert) |>
  summarise(CF = sum(CF)) |>
  filter(!is.na(vert)) |>
  arrange(vert), by=c('vert'))
}

#########################################################################
CURVA_DI_VERTS <- function(my_refdate){
  rb3::yc_get(my_refdate) |>
  select(-c(cur_days, r_360)) |>
  mutate(r_252 = 100*r_252) |>
  mutate(FTE = FTE(r_252, biz_days)) |>
  mutate(FL_VERTICE = 1L, .before = r_252)
}

#########################################################################
find_nearest_vert <- function(value, vert_vec){
  diff <- (value - vert_vec)
  id1 <- which(diff == min(diff[diff>0]))
  id2 <- which(diff == max(diff[diff<0]))
  c('DU' = value, 'DU_ANT' = vert_vec[id1], 'DU_POS' = vert_vec[id2])
}

#########################################################################
find_nearest_vert1 <- function(verts_moveis, verts_fixos){

N        <- length(verts_moveis)
my_verts <- matrix(NA, nrow = N, ncol = 2)
colnames(my_verts) <- c('vert_ant', 'vert_pos')

for(i in seq_along(verts_moveis)){
  diff <- (verts_moveis[i] - verts_fixos)

  vert_ant <- ifelse( any(diff == 0), verts_moveis[i],
                      verts_fixos[which(diff == min(diff[diff>0]))] )
  vert_pos <- ifelse( any(diff == 0), verts_moveis[i],
                      verts_fixos[which(diff == max(diff[diff<0]))] )

   my_verts[i,] <- c(vert_ant, vert_pos)
}

tibble(as.data.frame(my_verts))
}

#########################################################################
INTERPOL_FF <- function(value, vert_vec, rate_vec){
  diff <- (value - vert_vec)
  id1 <- which(diff == min(diff[diff>0]))
  id2 <- which(diff == max(diff[diff<0]))

  INTERPOL_FF0( value, rate_vec[id1], rate_vec[id2],
                vert_vec[id1], vert_vec[id2] )
}

#########################################################################
CURVA_DI_FULL <- function(VERT_DF){

my_refdate <- max(VERT_DF$refdate)

ALLVERTS <- left_join(tibble(biz_days = seq(1, max(VERT_DF$biz_days))),
                    VERT_DF, by = c('biz_days')) |>
  mutate(refdate = my_refdate) |>
  relocate(refdate) |>
  mutate(forward_date = add.bizdays(refdate, biz_days, 'Brazil/ANBIMA')) |>
  mutate(FL_VERTICE = ifelse(is.na(FL_VERTICE), 0L, 1L))

tbl_na <- ALLVERTS |> filter(is.na(r_252))
seq_na <- tbl_na$biz_days

biz_days <- VERT_DF$biz_days
rates    <- VERT_DF$r_252

for(i in seq_along(seq_na)){
  ALLVERTS$r_252[seq_na[i]] <- INTERPOL_FF(seq_na[i], biz_days , rates)
}

# RESULT
ALLVERTS |>
  mutate(FTE = FTE(RATE = r_252, TIME = biz_days) ) |>
  mutate(FWD   = round(RATE_BULLET(FV = FTE/dplyr::lag(FTE,1), TIME = 1), 4) )
}

#########################################################################
CURVA_DI_VERTS_PAD <- function(VERT_DF){

my_refdate <- max(VERT_DF$refdate)
verts_pad <- c(1, 21, 42, 63, 126, 252*seq(1,5), 2520)

verts1_df <- VERT_DF |>
  filter(biz_days %in% verts_pad) |>
  select(refdate, biz_days, FL_VERTICE, r_252)

vertsna <- verts_pad[!verts_pad %in% verts1_df$biz_days]

verts0_df <- tibble(refdate = my_refdate,
                    biz_days = vertsna, FL_VERTICE = 0, r_252 = NA)

biz_days <- VERT_DF$biz_days
rates    <- VERT_DF$r_252

for(i in seq_along(vertsna)){
verts0_df$r_252[i] <- INTERPOL_FF(vertsna[i], biz_days, rates)
}

# RESULT
bind_rows(verts1_df, verts0_df) |>
  arrange(biz_days)
}

#########################################################################
# INTERPOL0
INTERPOL0 <- function(dx, r1, r2, d1, d2){
  (r1 + (r2-r1)*(dx-d1)/(d2-d1))
}

#########################################################################
# INTERPOL_LIN
INTERPOL_LIN <- function(dx, r1, r2, d1, d2){
  100*INTERPOL0(dx, r1/100, r2/100, d1, d2)
}

#########################################################################
# INTERPOL_EXP
INTERPOL_EXP <- function(dx, r1, r2, d1, d2){
  100*(exp(INTERPOL0(dx, log(1+r1/100), log(1+r2/100), d1, d2)) - 1)
}

#########################################################################
# INTERPOL_FF
INTERPOL_FF0 <- function(dx, r1, r2, d1, d2){
  RATE_BULLET(FV = exp(INTERPOL0(dx, log(FTE(r1, d1)), log(FTE(r2, d2)), d1, d2)), TIME = dx)
}

#########################################################################
# INTERPOL_FF2
INTERPOL_FF2 <- function(dx, r1, r2, d1, d2){
  RATE_BULLET(FV = FTE(r1, d1)*FTE(FWD_RATE(r1, r2, d1, d2), TIME = (d2-d1)), TIME = d2)
}

#########################################################################
# BETA_OLS
BETA_OLS <- function(X,y){
# FUNCAO para calcular apenas o BETA de OLS
# beta_hat = (X'X)^(-1)(X'y)
  solve(crossprod(X), crossprod(X,y))
}

#########################################################################
# BETA_OLS
OLS_FIT <- function(X,y,betahat){
# FUNCAO para calcular apenas o FIT de OLS com erros e erros quadrados
# yhat = X \betahat

  yhat <- X%*%betahat
  err  <- y - yhat
  tibble(yobs = y, yhat = yhat, err=err, errsq=err^2)
}

###############################################################
DNS_LOADS <- function(LAMBDA, MATURS){

# FUNCAO para calcular os factor loadings do dns (dynamic nelson-siegel)
# --------------------------
# y_t(\tau_i) =
# \beta_{1,t} +
# \beta_{2,t} ( \dfrac{1 - exp( \lambda \tau_i ) }{ \lambda \tau_i } ) +
# \beta_{3,t} ( \dfrac{1 - exp( \lambda \tau_i ) }{ \lambda \tau_i } - exp(\lamdba \tau_i) ) +
# \varepsilon_{i,t}
# --------------------------
# B1_FL level, long-term
# B2_FL slope, short-term
# B3_FL curvature, medium term

NMATURS <- length(MATURS)
LT      <- LAMBDA*MATURS; LT_inv  <- 1/LT
EXP_LT  <- exp(-LT)     ; EXP_LT1 <- 1 - EXP_LT

B2_FL <- (EXP_LT1)*LT_inv # slope, short-term
B3_FL <- B2_FL - EXP_LT # curvature, medium term

cbind(B1_FL = 1, B2_FL = B2_FL, B3_FL = B3_FL)
}

###############################################################
DNS_LOSS_LAMBDA <- function(LAMBDA, YIELDS, MATURS){
# FUNCAO perda para a escolha do lambda
  LOADS    <- DNS_LOADS(LAMBDA, MATURS)
  BETA_HAT <- BETA_OLS(LOADS, YIELDS)
  YHAT     <- YIELDS%*%BETA_HAT

# RESULT: sum of squared erros
sum((YIELDS - YHAT)^2)
}

###############################################################
DNS_LAMBDA <- function(YIELDS, MATURS){
# FUNCAO para a escolha do lambda

optimize(DNS_LOSS_LAMBDA, YIELDS=YIELDS, MATURS=MATURS,
         interval = c(0,2))

# optim(0.1, DNS_LOSS_LAMBDA, YIELDS=YIELDS, MATURS=MATURS,
#       method = 'Brent', lower = 0.01, upper = 1)
}

#########################################################################
## SHOCKS BACEN
# CIRCULAR Nº 3.876, DE 31 DE JANEIRO DE 201
# pp. 4-6

SHOCK_CP_ALTA <- function(TIME, SHOCK_CP = 5){
  # ALTA CP
  SHOCK_CP*exp(-0.25*TIME)
}

SHOCK_CP_BAIXA <- function(TIME, SHOCK_CP = 5){
  # BAIXA CP
  -SHOCK_CP*exp(-0.25*TIME)
}

SHOCK_STEEP <- function(TIME, SHOCK_CP = 5, SHOCK_LP = 3){
  # STEEPENER
  expt4 <- exp(-0.25*TIME);
  -0.65*abs(SHOCK_CP*expt4) + 0.9*abs(SHOCK_LP*(1-expt4))
}

SHOCK_FLAT <- function(TIME, SHOCK_CP = 5, SHOCK_LP = 3){
  # FLATTENER
  expt4 <- exp(-0.25*TIME);
  0.8*abs(SHOCK_CP*expt4) - 0.6*abs(SHOCK_LP*(1-expt4))
}

FN_CRUVA_BACEN <- function(CURVA_DI){
# EFETUAR CHOQUES BACEN

CURVA_DI |>
  select(refdate, biz_days, forward_date, r_252) |>
  mutate(r_alta     = r_252 + 4,
         r_baixa    = r_252 - 4,
         r_cp_alta  = r_252 + SHOCK_CP_ALTA(biz_days/252),
         r_cp_baixa = r_252 + SHOCK_CP_BAIXA(biz_days/252),
         r_flat     = r_252 + SHOCK_FLAT(biz_days/252),
         r_steep    = r_252 + SHOCK_STEEP(biz_days/252))
}

#########################################################################
read_TD_file <- function(sPath, nm_years = c(year(lubridate::today())), nm_assets = 'LTN'){
# read A BUNCH OF FILES

stopifnot("nm_assets must be like: \n 'LTN', 'LFT', 'NTN-F', 'NTN-B', 'NTN-B_Principal', 'NTN-B1'" = (nm_assets %in% c('LTN', 'LFT', 'NTN-F', 'NTN-B', 'NTN-B_Principal', 'NTN-B1')))


files       <- dir(sPath)

years_idx  <- str_extract(files, '[0-9]{4}') %in% nm_years
assets_idx <- str_remove(files, '_[0-9]{4}.xls') %in% nm_assets

print(files[(assets_idx & years_idx)])

file_vec  <- paste0(sPath, files[(assets_idx & years_idx)])
file_list <- vector(mode = 'list', length = length(file_vec))

for(i in seq_along(file_vec)){
file_list[[i]] <- read_TD_file0(file_vec[i])
}

return(bind_rows(file_list))
}

#########################################################################
read_TD_file0 <- function(sFile){
# read ONE FILE

sheets         <- readxl::excel_sheets(sFile)
my_list        <- vector(mode = 'list', length = length(sheets))
# names(my_list) <- sheets

  for(i in seq_along(sheets)){
    my_list[[i]] <- read_excel(sFile, sheet = sheets[i], skip = 2L,
                               col_names = c('DT_BASE',
                                             'TX_COMPRA', 'TX_VENDA',
                                             'PU_COMPRA', 'PU_VENDA', 'PU_BASE')) |>
      mutate(NM_PAPEL = str_trim(str_remove(sheets[i], '[0-9]{6}')), .before = 1) |>
      mutate(DT_BASE     = as.Date(DT_BASE, '%d/%m/%Y')) |>
      mutate(DT_LIQ      = add.bizdays(DT_BASE, 1, 'Brazil/ANBIMA'), .after = DT_BASE) |>
      mutate(tmp     = str_extract(sheets[i], '[0-9]{6}'), .after = DT_LIQ) |>
      mutate(DT_VCTO = as.Date(paste0('20', str_sub(tmp, 5,6),
                                   '-', str_sub(tmp, 3,4),
                                   '-', str_sub(tmp, 1,2) )), .after = DT_LIQ )|>
      select(-c(tmp)) |>
      mutate(DT_VCTO_ADJ = adjust.next(DT_VCTO, 'Brazil/ANBIMA'), .after = DT_VCTO) |>
      mutate(DU     = bizdays(DT_LIQ, DT_VCTO_ADJ, 'Brazil/ANBIMA'), .after = DT_VCTO_ADJ) |>
      mutate(DU_252 = trunc2(DU/252,14), .after = DU) |>
      mutate(TX_COMPRA = 100*TX_COMPRA,
             TX_VENDA  = 100* TX_VENDA) |>
      mutate(FL_BULLET = ifelse(NM_PAPEL %in% c('LTN', 'LFT', 'NTN-B Princ'), 1, 0),
             .after = 1)
  }

return(bind_rows(my_list))
}
