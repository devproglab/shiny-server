options(encoding = "UTF-8")

# using() install missing libraries and load all the rest --------------------------------------
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only=TRUE))
  need <- libs[req==FALSE]
  if (length(need)>0) { 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("data.table", "readxl", "dplyr", "xts", "plotly", "KFAS", "seasonal", "stats",
      "lubridate", "xml2", "imputeTS", "ISOweek", "stringr", "forecast", "ggplot2", "tsbox", "tempdisagg", "writexl", "Quandl", 'mcGlobaloptim')

theme_set(theme_minimal())
options(scipen=999)

gen_report <- function() {
  # single files
  current_folder <- 'data_inputs/single'
  list_of_files <- list.files(current_folder, '') 
  l_date <- matrix(ncol=2, nrow=length(list_of_files))
  l_date <- data.frame(l_date)
  for (i in list_of_files) {
    file <- data.frame(read_excel(paste(current_folder,i,sep='/')))
    l_date[which(list_of_files==i),1] <- i
    l_date[which(list_of_files==i),2] <- as.character(ymd(last(file[,1])))
  }
  current_folder <- 'data_inputs/long'
  list_of_files <- list.files(current_folder, '') 
  l_date2 <- matrix(ncol=2, nrow=length(list_of_files))
  l_date2 <- data.frame(l_date2)
  for (i in list_of_files) {
    file <- data.frame(read_excel(paste(current_folder,i,sep='/')))
    l_date2[which(list_of_files==i),1] <- i
    if (!i=='price_structure_long.xlsx') {
      l_date2[which(list_of_files==i),2] <- as.character(ymd(max(file$date)))
    } else {
      l_date2[which(list_of_files==i),2] <- max(file$date)
    }
  }
  l_date <- rbind(l_date, l_date2)
  names(l_date) <- c('file', 'last_updated')
  write_xlsx(l_date, 'data_output/last_updated.xlsx')
  return(l_date)
}

make_weekly <- function(name) {
  weekly <- read_excel(paste('./data_inputs/panel/', name, '.xlsx', sep=''), col_names=FALSE)
  dates <- c(t(as.vector(weekly[1,][-1])))
  dates <- str_extract(dates, '(?<=\\()[^{}]+(?=\\))')
  w <- which(str_detect(dates,'(на 17сентября 2018 года)'))
  dates[w] <- paste('на 17', substr(dates[w], 6, nchar(dates[w])))
  dates <- str_extract(dates, '(|.)\\d .* \\d\\d\\d\\d')
  dates <- str_trim(dates)
  
  day <- str_extract(dates, '([0-9]+)')
  month <- str_trim(str_extract(dates, '(?<=\\d) .*\\b (?=\\d\\d\\d\\d)'))
  year <- str_extract(dates, '(\\d\\d\\d\\d)')
  
  cyr_month <- c('янв', 'фев', 'мар', 'апр', 'мая', 'июн', 'июл', 'авг', 'сент', 'окт', 'ноя', 'дек')
  en_month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  for (i in 1:12) {
    month[which(str_detect(month, cyr_month[i]))] <- en_month[i]
  }

  ymd <- as.Date(paste(day, month, year, sep=' '), format='%d %m %Y')
  weekly <- data.frame(weekly[-1,])
  weekly[,-1] <- apply(weekly[,-1], 2, as.numeric)
  names(weekly) <- c('id', format(ymd, '%Y-%m-%d'))
  weekly <- reshape2::melt(weekly, id=c("id"), variable.name = "date", value.name = "value")
  write_xlsx(weekly, paste('./data_inputs/long/', name, '_long.xlsx', sep=''))

}
# make_monthly() convert a wide-format panel dataset (see data_sources.xlsx) into long format -----
make_monthly <- function(name, start="1899-12-30", cpi=NULL) {
  monthly <- read_excel(paste('./data_inputs/panel/', name, '.xlsx', sep=''), col_names=FALSE)
  meta <- monthly[,1]
  monthly <- monthly[,-1]
  monthly <- as.data.frame(t(monthly))
  monthly[,2:length(monthly)] <- sapply(monthly[,2:length(monthly)], as.numeric)
  monthly[,1] <- as.Date(monthly[,1], origin = "1899-12-30")
  monthly <- as.data.frame(cbind(meta, t(monthly)))
  names(monthly) <- monthly[1,]
  monthly <- monthly[-1,]
  if (name=='wages') {
    monthly <- t(monthly)
    monthly[-1,] <- as.numeric(monthly[-1,]) / as.numeric(cpi)
    monthly[-1,] <- as.numeric(monthly[-1,]) * as.numeric(last(cpi))
    monthly <- t(monthly)
    monthly <- data.frame(monthly, check.names = FALSE)
  } 
  monthly <- reshape2::melt(monthly, id=c("id"), variable.name = "date", value.name = "value")
  monthly$date <- as.Date(monthly$date)
  monthly$value <- as.numeric(monthly$value)
  monthly <- monthly[complete.cases(monthly),]
  monthly <- monthly[monthly$date>=as.Date(start),]
  write_xlsx(monthly, paste('./data_inputs/long/', name, '_long.xlsx', sep=''))
}
update_price_str <- function(last_year=2019) {
  df <- read_excel('./data_inputs/panel/price_structure.xlsx', col_names=FALSE)
  
  for (i in 2014:last_year) {
    delta <- i - 2014
    subset <- cbind(df[-1,1], df[-1,-1][,(11*delta+1):(11*(delta+1))])
    subset[is.na(subset)] <- 0
    subset[,-1] <- apply(subset[,-1], 2, as.numeric)
    subset[, 4] <- rowSums(subset[,4:6])
    subset <- subset[,-c(5,6)]
    subset <- subset[,-6]
    names(subset) <- c('product', 'Стоимость сырья', 'Расходы на производство', 'Прибыль/убыток', 'НДС, акциз, иные налоги', 'Плата за доставку, осуществляемую переработчиком', 'Оборот посреднического звена', 'Торговая наценка', 'НДС, начисленный в рознице')
    subset <- subset[-which(rowSums(subset[,-1])==0),c(1,2,9,3:8)]
    subset_long <- reshape2::melt(subset, id.vars=c('product'))
    subset_long$date <- i + 1
    if (i==2014) {
      final <- subset_long
    } else {
      final <- rbind(final,subset_long)
    }
  }
  
  write_xlsx(final, './data_inputs/long/price_structure_long.xlsx')
}
# get_from_long() extract a particular series from a long dataset  -----------------------------
get_from_long <- function(from, id, name, impute=FALSE) {
  monthly <- read_excel(paste('./data_inputs/long/', from, '.xlsx', sep='') )
  df <- data.frame(monthly[monthly$id==id,c(2,3)])
  df$date = as.Date(df$date)
  df <- df[order(df$date),]
  rownames(df) <- df$date
  df <- df[,-1, drop=FALSE]
  df <- xts(df, order.by=as.Date(rownames(df)))
  if (!str_detect(from, 'weekly')) {
    df <- apply.monthly(df, 'mean')
    index(df) <- as.yearmon(index(df))
    
    if (impute) {
      t <- interval(as.Date('2015-01-01'), today()) %/% months(1) + 1
      foo <- seq(from=1, by=1, length.out=t)
      timeindex <- seq(ymd('2015-01-01'),
                       today(),
                       by = '1 month')
      ref <- xts(foo, order.by = timeindex)
      df <- merge(df, ref)
      df[,1] <- na_interpolation(df[, 1], option = "spline", method = "fmm")
      df <- df[,1]
    }
  }
  names(df) <- name
  return(df)
}
# get_factor() extract a particular series from an .xlsx file dataset  -------------------------
get_factor <- function(name, impute=FALSE, disagg=FALSE) {
  df <- data.frame(read_excel(paste('./data_inputs/single/', name, '.xlsx', sep='')))
  rownames(df) <- df$date
  df <- as.xts(df)
  df <- df[,-1]
  df <- apply.monthly(df, 'mean')
  if (impute) {
    t <- interval(as.Date('2015-01-01'), today()) %/% months(1) + 1
    foo <- seq(from=1, by=1, length.out=t)
    timeindex <- seq(ymd('2015-01-01'),
                     today(),
                     by = '1 month')
    ref <- xts(foo, order.by = timeindex)
    df <- merge(df, ref)
    df[,1] <- na_interpolation(df[, 1], option = "spline")
    df <- df[,1]
  }
  if (disagg) {
    if (periodicity(df)$scale=='quarterly') { 
      freq=4
      timeindex <- seq(ymd('2015-01-01'),
                       as.Date(last(index(df))+months(3)),
                       by = '1 month')
    }
    if (periodicity(df)$scale=='yearly') { 
      freq=1 
      timeindex <- seq(ymd('2015-01-01'),
                       as.Date(last(index(df))+months(12)),
                       by = '1 month')
    }
    dis <- td(ts(df, freq=freq) ~ 1, to = "monthly", method = "fast")
    df <- xts(predict(dis), order.by=timeindex)
  } else {
    index(df) <- as.yearmon(index(df)) + 1/12
  }
  names(df) <- name
  return(df)
}
# get_fx_moex() parse exchange rate from MOEX  -------------------------------------------------
get_fx_moex <- function(fx = "EUR/RUB", start_date = "2015-01-01") {
  url <- paste0(
    "https://www.moex.com/export/derivatives/currency-rate.aspx?language=en&currency=",
    fx,
    "&moment_start=", start_date,
    "&moment_end=", today()
  )
  data <- read_xml(url)
  date <- xml_attr(xml_find_all(data, "//rtsdata/rates/rate"), "moment")
  value <- xml_attr(xml_find_all(data, "//rtsdata/rates/rate"), "value")
  exrate <- data.frame(cbind(date, value))
  rownames(exrate) <- exrate$date
  exrate <- as.xts(exrate)
  exrate <- exrate[,-1]
  exrate <- apply.monthly(exrate, 'mean')
  index(exrate) <- as.yearmon(index(exrate))
  names(exrate) <- 'exrate'
  return(exrate)
}

# model_estimation() estimate a state-space model  ---------------------------------------------
model_estimation = function(df = df, smoothpar = 8, dummy = NULL, movingmean=0, model_name, varlabels, price_str=NULL, reset=NULL, nosave=FALSE, ar=FALSE, weekly=FALSE) {
  weights <- read_xlsx('./data_inputs/vesa.xlsx')
  weight <- as.numeric(weights[weights[,1]==model_name,][,2])
  if (model_name == 'Лекарственные средства, пачка') {
    weight = 1.663
  }
  
  if (!weekly) {
    df_list <- df
    df <- (Reduce(function(df1, df2) merge(df1, df2), df))
    span <- max(which(!is.na(df[,1])))
    df <- df[1:span,]
    ## impute missing factor values
    for (i in names(df[,-1])) {
      length <- max(which(!is.na(df[,i])))
      if (!length==span) {
        arima <- auto.arima(df[,i], approximation=TRUE, stepwise=TRUE)
        df[(length+1):span,i] <- forecast(arima, h=span-length)$mean
      }
    }
    ## de-trend inputs
    dfmod_unsmoothed = do.call(merge, lapply(df, function(x) {log(x/stats::lag(x))}))
    dfmod_unsmoothed = dfmod_unsmoothed[complete.cases(dfmod_unsmoothed)]
    start <- c(year(index(dfmod_unsmoothed))[1],month(index(dfmod_unsmoothed)[1]))
    t <- lapply(as.ts(dfmod_unsmoothed, start=start), function(e) stl(e, s.window = 12, t.window=smoothpar))
    seasonal <- as.xts(t[[1]][[1]][,'seasonal'])
    noise <- as.xts(t[[1]][[1]][,'remainder'])
    dfmod_sa <- do.call(merge, lapply(as.ts(dfmod_unsmoothed, start=start), function(e) {as.xts(stl(e, s.window = 12, t.window=smoothpar)[[1]][,'trend'])} ))
    ## add specified dummy variables
    if (!is.null(dummy)) {
      for (i in 1:ncol(dummy)) {
        if (!dummy[2,i]==2) {
          length <- ncol(dfmod_sa)
          dfmod_sa$dummy <- abs(as.numeric(dummy[2,i]) - 1)
          dfmod_sa[index(dfmod_sa)>=as.yearmon(dummy[1,i]),]$dummy <- as.numeric(dummy[2,i])
          dfmod_unsmoothed$dummy <- abs(as.numeric(dummy[2,i]) - 1)
          dfmod_unsmoothed[index(dfmod_unsmoothed)>=as.yearmon(dummy[1,i]),]$dummy <- as.numeric(dummy[2,i])   
          names(dfmod_sa)[length+1] <- colnames(dummy)[i]
          names(dfmod_unsmoothed)[length+1] <- colnames(dummy)[i]
        } else {
          span <- as.numeric(dummy[3,i])
          length <- ncol(dfmod_sa)
          dfmod_sa$dummy <- 0
          dfmod_sa[(index(dfmod_sa)>=as.yearmon(dummy[1,i])-span/12)&(index(dfmod_sa)<=as.yearmon(dummy[1,i])+span/12),]$dummy <- 1
          dfmod_unsmoothed$dummy <- 0
          dfmod_unsmoothed[(index(dfmod_unsmoothed)>=as.yearmon(dummy[1,i])-span/12)&(index(dfmod_unsmoothed)<=as.yearmon(dummy[1,i])+span/12),]$dummy <- 1 
          names(dfmod_sa)[length+1] <- colnames(dummy)[i]
          names(dfmod_unsmoothed)[length+1] <- colnames(dummy)[i]
        }
      }
    }
  } else {
    df_list <- df
    x_w <- data.frame(date=index(df[[1]]), df[[1]])
    w <- ifelse(nchar(isoweek(x_w$date))==1, paste(0, isoweek(x_w$date), sep=''), isoweek(x_w$date))
    x_w$date <- ISOweek2date(paste(year(x_w$date), '-W', w, '-1', sep=''))
    
    ref <- data.frame(date=ceiling_date(seq(from=as.Date(paste(year(df[[1]])[1], '01', '01', sep='-')), 
                                            to=last(index(df[[1]])), by='week'), unit='weeks', week_start=1))
    
    x_w <- merge(ref, x_w, all=TRUE)
    x_w <- xts(x_w[,-1, drop=FALSE], order.by=x_w[,1])
    x_w <- na_locf(x_w)
    prices_complete <- x_w
    x_w <- diff(log(x_w), lag=1)
    x_w <- x_w[-1,]
    
    span <- as.Date(paste(year(last(index(x_w))), month(last(index(x_w))), '01', sep='-'))
    x_m <- (Reduce(function(df1, df2) merge(df1, df2), df[-1]))
    ref <- data.frame(date=seq(from=as.Date(first(index(x_m))), to=span, by='months'))
    x_m <- data.frame(x_m)
    x_m$date <- as.Date(as.yearmon(rownames(x_m)))
    x_m <- merge(ref, x_m, all=TRUE)
    
    max <- length(x_m$date)
    
    for (i in names(x_m[,-1])) {
      length <- max(which(!is.na(x_m[,i])))
      if (!length==max) {
        arima <- auto.arima(x_m[,i], approximation=TRUE, stepwise=TRUE)
        x_m[(length+1):max,i] <- forecast(arima, h=max-length)$mean
      }
    }
    x_m <- xts(x_m[,-1], order.by=x_m$date)
    index(x_m) <- ceiling_date(as.Date(index(x_m)), unit='weeks', week_start=1)
    orig_regressors <- x_m
    
    x_m <-  do.call(merge, lapply(x_m, function(x) {log(x/stats::lag(x))}))[-1,]

    dfmod_unsmoothed <- merge(x_w, x_m, all=TRUE)
    dfmod_unsmoothed <- dfmod_unsmoothed[1:max(which(!is.na(dfmod_unsmoothed[,1]))),]
    
    start <- c(year(index(dfmod_unsmoothed))[1],month(index(dfmod_unsmoothed)[1]))
    x_m_sa <- do.call(merge, lapply(ts(x_m, start=start, frequency=12), function(e) {xts(stl(e, s.window = 12, t.window=smoothpar)[[1]][,'trend'], order.by=index(x_m))} ))
    
    start <- c(year(index(dfmod_unsmoothed))[1],isoweek(index(dfmod_unsmoothed)[1]))
    t <- lapply(ts(dfmod_unsmoothed[,1], start=start, frequency=52), function(e) stl(e, s.window = 12, t.window=smoothpar))
    seasonal <- xts(t[[1]][[1]][,'seasonal'], order.by=index(dfmod_unsmoothed))
    noise <- xts(t[[1]][[1]][,'remainder'], order.by=index(dfmod_unsmoothed))
    trend <- xts(t[[1]][[1]][,'trend'], order.by=index(dfmod_unsmoothed))

    dfmod_sa <- merge(trend, x_m_sa, all=TRUE)
    dfmod_sa <- dfmod_sa[1:max(which(!is.na(dfmod_sa[,1]))),]
    dfmod_sa <- na_locf(dfmod_sa)
    names(dfmod_sa) <- names(dfmod_unsmoothed)
  }
  # make price structure decomposition
  if (!is.null(price_str)) {
    price_decomp <- cost_str_decomp(prices = if(weekly) {prices_complete} else{df[,1]}, product = price_str, weekly=weekly)
  } else {
    price_decomp = NULL
  }
  
  ## simple linear model (iid residuals assumption)
  predictors = dfmod_sa[, -1]
  price      = dfmod_sa[, 1]
  npred      = ncol(predictors) + 1
  
  formula = formula(paste('price', paste(names(predictors), collapse=" + "), sep=" ~ "))
  testmod = lm(formula, data=dfmod_sa)
  summary(testmod)
  
  ecterm = as.xts(coredata(residuals(testmod)), order.by = index(dfmod_sa))
  ar1ec = lm(coredata(ecterm) ~ coredata(stats::lag(ecterm,1)) -1)
  summary(ar1ec)
  
  ## % of price variation that may be treated as one SD of measurement error
  measerr = 0
  
  ## set up SS-model
  
  ### n.obs.
  N = nrow(dfmod_sa)
  
  ### state transition matrix
  dimSS   = npred*2-1
  
  Tt      = diag(dimSS)
  diag(Tt)[1:npred] <- 0
  diag(Tt[2:npred, (npred+1):dimSS]) <- 1
  if (ar) {
    Tt[1,1] <- 0
  }
  
  ### state variance matrix
  
  Rt = diag(dimSS)
  
  if (!movingmean) {
    diag(Rt)[(npred+1):dimSS] <- 0
  } else {
    # diag(Rt[2:npred, (npred+1):dimSS]) <- 1
    # diag(Rt)[(npred+1):dimSS] <- 0
  }


  Qt = diag(dimSS)
  diag(Qt) <- NA
  if (!movingmean) {
    diag(Qt)[(npred+1):dimSS] <- 0
  }
  
  M_zero <- matrix(0,nrow=N, ncol=npred-1)
  t <- cbind(rep(1, N), coredata(predictors), M_zero)
  Zt = array( t(t), c(1,dimSS,N))
  
  ### measurement error matrix
  Ht = array(measerr*price, c(1,1,N))
  
  ## initial conditions
  coefinit = coef(testmod)
  
  a1 = c(coefinit, coefinit[-1])
  P1 = diag(0, dimSS)
  P1[1,1] <- 0.1
  
  P1inf = diag(0, nrow = dimSS, ncol = dimSS)

  if (!is.null(reset)) {
    diag(P1)[npred+reset[1,]] <- 0.1
    diag(P1)[1+reset[1,]] <- 0.1
    a1[npred+reset[1,]] <- reset[2,]
    a1[1+reset[1,]] <- reset[2,]
  }

  ### define the model
  model = SSModel(coredata(price) ~ -1 + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1,
                                                   P1inf = P1inf) 
                  , H = Ht)
  
  ## define model updating function
  update_model <- function(pars, model) {
    if (!movingmean) {
      #mean reversion
      # diag(model["Q"][,,1])[1:npred] = pars[1:npred]^2
      diag(model["Q"][,,1])[1:npred] = exp(pars[1:npred])
      if (ar) {
        diag(model["T"][,,1])[2:npred] = pars[(npred+1):(dimSS)]^2/(1+pars[(npred+1):(dimSS)]^2)
        diag(model["T"][,,1][2:npred, (npred+1):dimSS]) = 1 - pars[(npred+1):(dimSS)]^2/(1+pars[(npred+1):(dimSS)]^2)
      }
    } else {
      #moving mean
      # model["Q"] = diag(pars[1:dimSS]^2)
      model["Q"] = diag(exp(pars[1:dimSS]))
      if (ar) {
        diag(model["T"][,,1])[2:npred] = pars[(dimSS+1):(dimSS+npred-1)]^2/(1+pars[(dimSS+1):(dimSS+npred-1)]^2)
        diag(model["T"][,,1][2:npred, (npred+1):dimSS]) = 1 - pars[(dimSS+1):(dimSS+npred-1)]^2/(1+pars[(dimSS+1):(dimSS+npred-1)]^2)
      }
    }
    model
  }
  
  ### define model likelihood
  SSLik_mod = function(x) {-logLik(update_model(pars = x, model = model))}
  
  ### run ML-optimization with parinit = initital guesses
  
  if (!movingmean) {
    #mean reversion
    parinit = log(c(summary(testmod)$coefficients[, 2]))
    upper = c(summary(testmod)$coefficients[, 2]*2)
    lower = c(summary(testmod)$coefficients[, 2]/2)
  } else {
    #moving mean
    guesses <- (apply(
      rbind(apply(abs(diff(predictors[1:15,]))[-1,],2,median),
            apply(abs(scale(predictors[1:15,], scale = FALSE)),2,median)
      )
      , 2, min)/0.06745)^2
    guesses <- ifelse((guesses==0|is.na(guesses)), 0.0001, guesses)
    parinit = log(c(summary(testmod)$coefficients[, 2], guesses))
    upper = c(summary(testmod)$coefficients[, 2]*2, guesses*2)
    lower = c(summary(testmod)$coefficients[, 2]/2, guesses/2)
  }
  if (ar) {
    parinit <- c(parinit, rep(sqrt(0.5/(1-0.5)), npred-1))
    upper <- c(upper, rep(1, npred-1))
    lower <- c(lower, rep(0, npred-1))
  }
  ## obtain model parameters
  
  # modest = optim(par = parinit, fn = SSLik_mod, method = 'L-BFGS-B',
  #                control = list('trace' = 3, 'maxit' = 15000),
  #                upper = upper,
  #                lower = lower)
  
  modest = optim(par = parinit, fn = SSLik_mod, method = 'L-BFGS-B',
                 control = list('trace' = 3, 'maxit' = 15000))
  parest = modest$par
  
  # modest = multiStartoptim(objectivefn = SSLik_mod, lower=lower, upper=upper,
  #                          method='L-BFGS-B', nbtrials=20, typerunif='sobol',
  #                          localsearch = 'median',
  #                          control = list(trace = 0, 'maxit' = 2000), verb=TRUE)
  # parest = modest$res$par
  
  ### update model and run smoother
  modelest  = update_model(pars = as.numeric(parest), model = model)
  smoothest = KFS(modelest)

  ### extract estimated time-varying coefficients
  alphahat   = as.xts(coredata(smoothest$alphahat), order.by = index(dfmod_sa))
  alphahatsd = as.xts(t(apply(smoothest$V, 3, function(x) {sqrt(diag(x))})), order.by = index(dfmod_sa))
  ### record state names
  statenames = c('demand', names(dfmod_sa)[-1])
  
  alphahat <- alphahat[,1:npred]
  alphahatsd <- alphahatsd[,1:npred]
  
  ### make decomposition tab
  decomp = cbind(dfmod_unsmoothed[,1], alphahat[,1], alphahat[,-1]*dfmod_sa[, -1], seasonal, noise)
  names(decomp) = c('value', 'demand', names(dfmod_sa)[-1], 'seasonal', 'noise')
  
  if (!weekly) {
    decomp2 <- rollsum(decomp, 12, align='right', fill = NA, na.rm=TRUE)
  } else {
    decomp2 <- rollsum(decomp, 52, align='right', fill = NA, na.rm=TRUE)
  }
  decomp2 <- decomp2[complete.cases(decomp2),]
  decomp2 <- decomp2[,-which(names(decomp2)=='seasonal')]
  
  colsums <- colSums(abs(decomp2[,-1]))
  abssum <- sum(abs(decomp2[,-1]))
  
  shares <- colsums / c(abssum) * 100
  names(shares) <- c('Инфляция спроса', varlabels, 'Прочие факторы')
  
  ret_list <- list(model_name = model_name, df = df_list, shares=shares, testmod = testmod, dfmod_sa = dfmod_sa, dfmod_unsmoothed = dfmod_unsmoothed,
                   modelest = modelest, smoothest = smoothest, parest = parest,
                   alphahat = alphahat, alphahatsd = alphahatsd, statenames = statenames, decomp = decomp, decomp2=decomp2, varlabels=varlabels,
                   price_decomp = price_decomp, weight=weight, weekly=weekly)
  if (!nosave) {
    saveit <- function(..., string) {
      x <- list(...)
      names(x) <- string
      save(list=names(x), file=paste('./data_output/models/', string, '.RData', sep=''), envir=list2env(x))
    }
    saveit(ret_list, string = model_name)
  }
  return(ret_list)
  
}
# Plotting functions
source('plots.R')
# cost_str_decomp() decomposes prices changes into cost structure elements ----------------------
cost_str_decomp <- function(prices, product, weekly=FALSE) {
  cost_structure <- read_xlsx('./data_inputs/long/price_structure_long.xlsx')
  data <- cost_structure[cost_structure$product==product,]
  full <- as.xts(matrix(nrow=length(prices), ncol=length(unique(data$variable))), order.by=as.Date(index(prices)))
  count <- 1
  for (i in unique(data$variable)) {
    df <- data[data$variable==i,c(3,4)]
    df$value <- as.numeric(df$value)
    df$date <- ymd(as.numeric(df$date), truncated = 2L)
    df <- as.xts(df[,1], order.by=df$date)
    if (!weekly) {
      int <- interval(as.Date('2015-01-01'), last(as.Date(index(prices)))) %/% months(1) + 1
      foo <- seq(from=1, by=1, length.out=int)
      timeindex <- seq(ymd('2015-01-01'),
                       as.Date(last(index(prices))),
                       by = '1 month')
      ref <- xts(foo, order.by = timeindex)
      df <- merge(df, ref)
      df[,1] <- na_interpolation(df[, 1], option = "linear")
      df <- df[,1]
      names(df) <- i
      index(df) <- as.yearmon(index(df))
      full[,count] <- df
      count <- count + 1
    } else {
      ref <- data.frame(date=ceiling_date(seq(from=as.Date(paste(year(prices)[1], '01', '01', sep='-')), 
                                              to=last(index(prices)), by='week'), unit='weeks', week_start=1))
      
      foo <- seq(from=1, by=1, length.out=nrow(ref))
      ref <- xts(foo, order.by = ref$date)
      df <- merge(df, ref)
      df[,1] <- na_interpolation(df[, 1], option = "linear")
      df <- df[,1]
      names(df) <- i
      df <- df[which(index(df) %in% index(prices))]
      full[,count] <- df
      count <- count + 1
    }
  }
  names <- c('Стоимость сырья', 'Расходы на производство', 'Прибыль/убыток', 'НДС, акциз, иные налоги', 'Плата за доставку, осуществляемую переработчиком', 'Оборот посреднического звена', 'Торговая наценка', 'НДС, начисленный в рознице')
  names(full) <- names
  full <- full[1:length(prices),]/100
  
  q = seq_along(index(full))
  full_smooth = do.call(merge, lapply(full, function(x) {
    as.xts(loess(x~q, degree=2, span=0.4)$fitted,
           order.by = index(full))
  }))
  names(full_smooth) <- names
  decomp <- diag(as.vector(prices)) %*% full_smooth
  decomp <- as.xts(decomp, order.by=index(prices))
  return(decomp)
}
save_cost_decomp <- function(estimated, filename) {
  decomp <- estimated$price_decomp
  names <- names(estimated$price_decomp)
  p <- plot_price_decomp(estimated)
  fname <- paste(filename, '.html', sep='')
  options(encoding = "Windows-1251")
  htmlwidgets::saveWidget(p, file = fname, selfcontained=TRUE)
  options(encoding = "UTF-8")
  file.rename(from = fname,
              to   = paste('./data_output/indices/', fname))
  decomp <- data.frame(decomp)
  decomp$price <- rowSums(decomp)
  decomp$date <- rownames(decomp)
  decomp <- decomp[,c(length(decomp), length(decomp)-1, 1:(length(decomp)-2))]
  names(decomp) <- c('Дата', 'Розничная цена', names)
  fname <- paste('./data_output/indices/', filename, '.xlsx', sep='')
  write_xlsx(as.data.frame(decomp), fname)
}
loadR <- function(fileName){
  #loads an RData file, and returns it
  fileName <- paste('./data_output/models/', fileName, '.RData', sep='')
  load(fileName)
  get(ls()[ls() != "fileName"])
}


