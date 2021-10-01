# calcClim.R 
# Base predictions for climatological precision

calc_ClimDaily_dayOfYearWindow_seamless = function(QobsCal,datesCal,datesClim=NULL,inc=14){

  # Test for .csv or RData date formats
  if (is.na(as.Date(datesCal[1],format="%Y-%m-%d"))){datesCal = as.Date(datesCal,format = "%d/%m/%Y")} else {datesCal = as.Date(datesCal,format = "%Y-%m-%d")}

  if(is.null(datesClim)) {datesClim = datesCal}

  yearsCal = format(datesCal,'%Y')

  yearsCal = as.integer(unique(yearsCal))
  n_yearsCal = length(yearsCal)
  deltas = seq(-inc,inc)
  n_deltas = length(deltas)
  clim_mat = matrix(nrow=366,ncol=(n_deltas*n_yearsCal))

  for (j in 1:365){
    counter = 1
    for (year in yearsCal){
      dateThisYear =as.Date(paste(j,'-',year,sep=''),format='%j-%Y')
      for (delta in deltas){
        dateSel = dateThisYear + delta
        iDateSel = which(datesCal==dateSel)
        if (length(iDateSel)==1){
          clim_mat[j,counter] = QobsCal[iDateSel]
        } else {
          clim_mat[j,counter] = NA
        }
        counter = counter+1
      }
    }
  }
  clim_mat[366,] = clim_mat[365,]

  nDaysClim = length(datesClim)
  clim = matrix(nrow=nDaysClim,ncol=(n_deltas*n_yearsCal))
  for (n in 1:nDaysClim){
    d = as.integer(format(datesClim[n],'%j') )
    clim[n,] = clim_mat[d,]
  }

  return(clim)

}


