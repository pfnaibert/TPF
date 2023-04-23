###########################################################
cat("\014")
options(digits=16)
source('./funs_TPF.R')

###########################################################
dates <- bizseq('2022-12-15', today(), 'Brazil/B3') |> print()

dates[i]
CURVA_DI_VERTS(ymd('2023-04-20'))

for(i in seq_along(dates)){
# saveRDS(CURVA_DI_VERTS(dates[i]), paste0('./CURVAS/DI_', dates[i]))
  print(dates[i])
write_csv2( CURVA_DI_VERTS(dates[i]),
            paste0('./CURVAS/DI_', dates[i], '.csv') )
}

###########################################################
