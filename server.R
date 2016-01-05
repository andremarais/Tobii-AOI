file.choose2 <- function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose();
  }, error = function(ex) {
    
  })
  pathname;
}
# Themes for plots ====
plot.theme <- theme(
  panel.background = element_rect(fill = "#272b30"),
  plot.background = element_rect(fill = "#272b30"),
  axis.text.x = element_text(size = 10),
  axis.title = element_text(size = 10),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank()
)

plot.theme.2 <- theme(
  panel.background = element_rect(fill = "#272b30"),
  plot.background = element_rect(fill = "#272b30"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.border = element_blank()
)

#functions for columns ====

focus.select <- function(df, ad, measurement, metric, ID, label) {
  rel.columns <-
    colnames(df)[which((gregexpr(ad, colnames(df)) != -1) &
                         (gregexpr(metric, colnames(df)) != -1) &
                         (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1))]
  focus.areas <- gsub(paste(measurement, '_', sep = ''),'',rel.columns)
  focus.areas <-
    gsub(paste(ad,'\\.(jpg_|png_|jpeg_)', sep = ''),'', focus.areas)
  focus.areas <- gsub('\\.\\d', '', focus.areas)
  focus.areas <- gsub(paste('_',metric, sep = ''),'',focus.areas)

  selectInput(ID,
              label = label,
              choices = unique(focus.areas))
}

plot.function <- function(df, ad, measurement, metric, focus, col) {
  measurements <-
    df[df$X != 'All Recordings',which((gregexpr(ad, colnames(df)) != -1) &
                                        (gregexpr(metric, colnames(df)) != -1) &
                                        (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1) & 
                                        (gregexpr(paste(focus, '_', sep = ''), colnames(df)) != -1))]
  measurements <- melt(measurements[!is.na(measurements)])
  
  if (nrow(measurements) <= 2)
    return({
      ggplot(data.frame(label = 'Too few fixations for density plot')) + geom_text(aes(x = 0, y = 0, label = label)) + plot.theme.2
    })
  
  ggplot(data.frame(x = measurements$value)) +
    geom_density(
      aes(x = x), col = paste(col,'3', sep = ''), fill = paste(col,'1', sep = ''), alpha = .85
    ) +
    plot.theme +
    xlab('seconds') +
    ggtitle('Density plot')
}

text.function <- function(df, ad, measurement, metric, focus) {
  measurements <-
    df[df$X != 'All Recordings',which((gregexpr(ad, colnames(df)) != -1) &
                                        (gregexpr(metric, colnames(df)) != -1) &
                                        (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1) & 
                                        (gregexpr(paste(focus, '_', sep = ''), colnames(df)) != -1))]
  measurements <- melt(measurements[!is.na(measurements)])
  
  paste(
    'Sample size', nrow(measurements),
    '\nMean of values', round(mean(measurements$value),2),
    '\nStandard deviation', if (is.na(sd(measurements$value)))
      ''
    else
      round(sd(measurements$value),2), sep = ': '
  )
}

mw.function <- function(df, ad1, focus1, ad2, focus2, measurement, metric) {
  measurements1 <-
    df[df$X != 'All Recordings',which((gregexpr(ad1, colnames(df)) != -1) &
                                        (gregexpr(metric, colnames(df)) != -1) &
                                        (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1) & # ^: to look at start of string
                                        (gregexpr(paste(focus1, '_', sep = ''), colnames(df)) != -1))]
  measurements2 <-
    df[df$X != 'All Recordings',which((gregexpr(ad2, colnames(df)) != -1) &
                                        (gregexpr(metric, colnames(df)) != -1) &
                                        (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1) &
                                        (gregexpr(paste(focus2, '_', sep = ''), colnames(df)) != -1))]
  
  measurements1 <- measurements1[!is.na(measurements1)]
  measurements2 <- measurements2[!is.na(measurements2)]
  
  w <-
    wilcox.test(measurements1, measurements2, if (mean(measurements1) < mean(measurements2))
      'l'
      else
        'g')
  paste(
    gsub('_|\\.', ' ', ad1), ": ", focus1, '\nvs.\n',
    gsub('_|\\.', ' ', ad2), ": ", focus2, '\n\n',
    if (max(table(measurements1), table(measurements2)) > 1)
      "Tied values, can't compute exact p-value\n",
    'p-value: ', round(w$p.value,4), '\n\n',
    
    if (w$p.value < 0.05)
      # Reject H0
    {
      paste(
        'Reject H0.\n\n',
        gsub('_|\\.', ' ', ad1), ": ", focus1, ' ', metric, ' values are significantly ', if (mean(measurements1) < mean(measurements2))
          'less '
        else
          'greater than ', gsub('_|\\.', ' ', ad2), ": ", focus2, sep = ''
      )
    }
    else
    {
      paste(
        'Fail to reject H0.\n\nNo signifcant difference between ', gsub('_|\\.', ' ', ad1), ": ", focus1, ' and ', gsub('_|\\.', ' ', ad2), ": ", focus2, sep = ''
      )
    },
    
    sep = ''
  )
  
}

# function for overview ====
overview <- function(df, ads, measurement, metric) {
  
  a <- c()
  b <- c()
  c <- c()
  i <- 1
  for (ad in ads) {
    rel.columns <-
      colnames(df)[which((gregexpr(ad, colnames(df)) != -1) &
                           (gregexpr(metric, colnames(df)) != -1) &
                           (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1))]
    focus.areas <- gsub(paste(measurement, '_', sep = ''),'',rel.columns)
    focus.areas <-
      gsub(paste(ad,'\\.(jpg_|png_|jpeg_)', sep = ''),'', focus.areas)
    focus.areas <- gsub('\\.\\d', '', focus.areas)
    focus.areas <- gsub(paste('_',metric, sep = ''),'',focus.areas)
    for (focus.area in focus.areas) {
      a[i] <- ad
      b[i] <- focus.area
      h <-
        df[df$X != 'All Recordings',which((gregexpr(ad, colnames(df)) != -1) &
                                            (gregexpr(metric, colnames(df)) != -1) &
                                            (gregexpr(paste('^',measurement, '_', sep = ''), colnames(df)) != -1) & 
                                            (gregexpr(paste(focus.area, '_', sep = ''), colnames(df)) != -1))]
      c[i] <- mean(h[!is.na(h)])
      i <- i + 1
    }
  }
  
  df <- data.frame(Ad = a, Focus_Areas = b, Mean = c)
  df <- aggregate(data = df, Mean ~ Ad + Focus_Areas, sum)
  
  df$Ad <- gsub('\\.|_', ' ', df$Ad)
  df$Focus_Areas <- gsub('\\.|_', ' ', df$Focus_Areas)
  
  return(df)
}


# ServerUI====
shinyServer(function(input, output, session) {
  # browse & import ====
#   observe({
#     if (input$browse == 0)
#       return()
#     updateTextInput(session, "path",  value = file.choose2())
#     
#   })
  
  
  contentInput <- reactive({
    infile <- input$file1
    df <- read.delim(infile$datapath, na.strings = '-')
    cn <- colnames(df[,2:ncol(df)])
    measurements <- unique(substring(cn,1,sapply(gregexpr('_', cn), '[[',1)-1))
    ads <- unique(substring(cn, sapply(gregexpr('_', cn), '[[',1) +1, sapply(gregexpr('(\\.)(jpg|png|jpe)', cn), '[[',1)-1))
    
    isolate({
      list(df,measurements,ads)
    })
  })
  
  # metric select ====
  output$MetricSelect <- renderUI({
    if (is.null(input$file1))
      return()
    selectInput(
      'MetricSelect',
      label = 'Select Metric',
      choices = c('Mean', 'Max', 'Min','Sum', 'Median', 'Stdev'),
      selected = 'Mean'
    )
  })
  
  output$MeasurementSelect <- renderUI({
    if (is.null(input$file1))
      return()
    selectInput('MeasurementSelect',
                label = 'Select Measurement',
                choices =  contentInput()[[2]],
                selected =  contentInput()[[2]][1])
    
    
  })
  
  
  
  
### Column 1 ----
  output$AdSelect1 <- renderUI({
    if (is.null(input$file1))
      return()
    selectInput('AdSelect1',
                label = 'Select Ad 1',
                choices = contentInput()[[3]],
                selected = contentInput()[[3]][1])
    
  })
  
  output$FocusSelect1 <- renderUI({
    if (input$file1 == 0 ||
        is.null(input$AdSelect1) || input$AdSelect1 == '')
      return()
    focus.select(contentInput()[[1]], input$AdSelect1, input$MeasurementSelect, input$MetricSelect, 'FocusSelect1', 'Select focus')
  })
  
  output$plot1 <- renderPlot({
    if (input$file1 == 0 ||
        is.null(input$AdSelect1) || input$AdSelect1 == '')
      return()
    plot.function(contentInput()[[1]], input$AdSelect1, input$MeasurementSelect, input$MetricSelect, input$FocusSelect1, 'dodgerblue')
  })
  
  output$descriptive1 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect1) || input$AdSelect1 == '')
      return()
    text.function(contentInput()[[1]], input$AdSelect1, input$MeasurementSelect, input$MetricSelect, input$FocusSelect1)
  })
  
  output$test12 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect1) ||
        input$AdSelect1 == ''  ||
        is.null(input$AdSelect2) || input$AdSelect2 == '')
      return()
    mw.function(
      contentInput()[[1]], input$AdSelect1, input$FocusSelect1, input$AdSelect2, input$FocusSelect2, input$MeasurementSelect,input$MetricSelect
    )
  })
  
  output$test13 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect1) ||
        input$AdSelect1 == ''  ||
        is.null(input$AdSelect3) || input$AdSelect3 == '')
      return()
    mw.function(
      contentInput()[[1]], input$AdSelect1, input$FocusSelect1, input$AdSelect3, input$FocusSelect3, input$MeasurementSelect,input$MetricSelect
    )
  })
  
  
  
### Column 2 ----
  output$AdSelect2 <- renderUI({
    if (is.null(input$file1))
      return()
    selectInput('AdSelect2',
                label = 'Select Ad 2',
                choices = contentInput()[[3]],
                selected = contentInput()[[3]][2])
    
  })
  
  output$FocusSelect2 <- renderUI({
    if (input$file1 == 0 ||
        is.null(input$AdSelect2) || input$AdSelect2 == '')
      return()
    focus.select(contentInput()[[1]], input$AdSelect2, input$MeasurementSelect, input$MetricSelect, 'FocusSelect2', 'Select focus')
  })
  
  output$plot2 <- renderPlot({
    if (input$file1 == 0 ||
        is.null(input$AdSelect2) || input$AdSelect2 == '')
      return()
    plot.function(contentInput()[[1]], input$AdSelect2, input$MeasurementSelect, input$MetricSelect, input$FocusSelect2, 'olivedrab')
  })
  
  output$descriptive2 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect2) || input$AdSelect2 == '')
      return()
    text.function(contentInput()[[1]], input$AdSelect2, input$MeasurementSelect, input$MetricSelect, input$FocusSelect2)
  })
  
  output$test23 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect2) ||
        input$AdSelect2 == ''  ||
        is.null(input$AdSelect3) || input$AdSelect3 == '')
      return()
    mw.function(
      contentInput()[[1]], input$AdSelect2, input$FocusSelect2, input$AdSelect3, input$FocusSelect3, input$MeasurementSelect,input$MetricSelect
    )
  })
  
### Column 3 ----
  output$AdSelect3 <- renderUI({
    if (is.null(input$file1))
      return()
    selectInput('AdSelect3',
                label = 'Select Ad 3',
                choices = contentInput()[[3]],
                selected = contentInput()[[3]][3])
    
  })
  
  output$FocusSelect3 <- renderUI({
    if (input$file1 == 0 ||
        is.null(input$AdSelect3) || input$AdSelect3 == '')
      return()
    focus.select(contentInput()[[1]], input$AdSelect3, input$MeasurementSelect, input$MetricSelect, 'FocusSelect3', 'Select focus')
  })
  
  output$plot3 <- renderPlot({
    if (input$file1 == 0 ||
        is.null(input$AdSelect3) || input$AdSelect3 == '')
      return()
    plot.function(contentInput()[[1]], input$AdSelect3, input$MeasurementSelect, input$MetricSelect, input$FocusSelect3, 'firebrick')
  })
  
  output$descriptive3 <- renderText({
    if (input$file1 == 0 ||
        is.null(input$AdSelect3) || input$AdSelect3 == '')
      return()
    text.function(contentInput()[[1]], input$AdSelect3, input$MeasurementSelect, input$MetricSelect, input$FocusSelect3)
  })
  
  
  output$OverallView <- renderPlot({
    if (input$file1 == 0) return()
    ggplot(overview(contentInput()[[1]], contentInput()[[3]], input$MeasurementSelect, input$MetricSelect)) +
      geom_histogram(
        aes(x = reorder(Focus_Areas, Mean),
            y = Mean),
        stat = 'identity',
        col = 'dodgerblue3',
        fill = 'dodgerblue1',
        alpha = .85,
        width = .75,
        position = 'stack'
      ) +
      facet_grid(. ~ Ad) +
      coord_flip() +
      theme(
        panel.background = element_rect(fill = '#272b30'),
        plot.background = element_rect(fill = '#272b30'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank()) 
  })
  
  output$Overalltable <- renderDataTable({
    if (input$file1 == 0) return()
    
    df <- overview(contentInput()[[1]], contentInput()[[3]], input$MeasurementSelect, input$MetricSelect)
    df$Mean <- round(df$Mean,1)
    spread(df, Ad, Mean, fill = 0)
    },
    options = list(paging = FALSE))
})


