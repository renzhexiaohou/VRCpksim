##################################################################
# Author: Lizhi Chen/Yubo Xiao                                   #
# Phone: 13564107086/13971656760                                 #
# email: lizhi.chen@certara.com                                  #
# script created: Jan,10,2019                                    #
# Software version: R 3.5.2                                      #
##################################################################
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)
library(dplyr)
library(stringr)
library(deSolve)
library(PKNCA)

shinyServer(
  function(input, output, session) {
    # treatment UI - reactive UI design -------------------------------
    # inserted or removed treatment input
    inserted <- c()
    observeEvent(input$insertBtn, {
      btn <- input$insertBtn
      id <- paste0('treatment', btn)
      insertUI(
        selector = '#placeholder',
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          # tags$p(paste('Element number')), 
          column(3, numericInput(paste0("timepoint",btn), NULL, value = 84)),
          column(3, numericInput(paste0("dosage",btn), NULL, value = 200)),
          column(3, numericInput(paste0("interval",btn), NULL, value = 12)),
          column(3, numericInput(paste0("times",btn), NULL, value = 1)),
          id = id
        )
      )
      inserted <<- c(id, inserted)
    })
    observeEvent(input$removeBtn, {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[length(inserted)])
      )
      inserted <<- inserted[-length(inserted)]
    })
    
    # treatment data - extract from UI ------------
    ## flag every treatment (insert or remove) action
    flag <- reactiveValues()
    flag[["flag0"]] <- 0
    insflaglist <- reactiveValues()
    insflaglist[["ins0"]] <- 0
    remflaglist <- reactiveValues()
    remflaglist[["rem0"]] <- 0
    
    # logic value for each flag
    maxflag <- function(flag) {
      max(unlist(reactiveValuesToList(flag)))
    }
    observeEvent(input$insertBtn, {
      flag[[paste0("flag", maxflag(flag) + 1)]] <- maxflag(flag) + 1
      insflaglist[[paste0(maxflag(flag))]] <- TRUE
      remflaglist[[paste0(maxflag(flag))]] <- FALSE
    })
    observeEvent(input$removeBtn, {
      flag[[paste0("flag", maxflag(flag) + 1)]] <- maxflag(flag) + 1
      insflaglist[[paste0(maxflag(flag))]] <- FALSE
      remflaglist[[paste0(maxflag(flag))]] <- TRUE
    })
    
    ## manipulate treatment data
    treatment <- reactive({
      # extract target variables from reactive "list"
      unlisted <- function(data, var) {
        data[str_which(names(data), var)] %>% unlist()
      }
      ordered <- function(data) {
        names(data) %>% str_extract(., "[:digit:]+") %>% as.integer()
      } 
      inputlist <- reactiveValuesToList(input)
      trt <- tibble(timepoint = unlisted(inputlist, "timepoint"),
                    tptname = unlisted(inputlist, "timepoint") %>% ordered(),
                    dosage = unlisted(inputlist, "dosage"),
                    dosname = unlisted(inputlist, "dosage") %>% ordered(),
                    interval = unlisted(inputlist, "interval"),
                    intname = unlisted(inputlist, "interval") %>% ordered(),
                    times = unlisted(inputlist, "times"),
                    timname = unlisted(inputlist,"times") %>% ordered())
      trt1 <- trt %>% select(timepoint, tptname) %>% rename(ord = tptname)
      trt2 <- trt %>% select(dosage, dosname) %>% rename(ord = dosname)
      trt3 <- trt %>% select(interval, intname) %>% rename(ord = intname)
      trt4 <- trt %>% select(times, timname) %>% rename(ord = timname)
      
      # treatment data (have both inserted and removed records)
      treatment <- trt1 %>%
        left_join(trt2, by = "ord") %>%
        left_join(trt3, by = "ord") %>%
        left_join(trt4, by = "ord") %>%
        arrange(ord)
      
      # final treatment data (has excluded removed records)
      nrow_removed <- sum(unlist(reactiveValuesToList(remflaglist)))
      if(nrow_removed == 0) {return(treatment)}
      if(nrow_removed >=1) {return(treatment[-(2:(nrow_removed + 1)),])}
    })
    
    # dosing data - prepare for sim ---------------------
    # convert dosing times into every duplicated dosing record
    doseduplicate <- function(data, times) {
      if (!is.data.frame(data)) {
        cat("input data should be data.frame or tibble :)")
        stop()
      }
      if (!is.character(times)) {
        cat("times variable should be a string type name :o")
        stop()
      }
      l <- list()
      s <- split(data, row.names(data))
      rownames_to_column(data) %>%
        .[["rowname"]] %>%
        lapply(function(x) {
          l[[paste0(x)]] <- rep(s[x], s[[x]][[times]])
        }) %>%
        unlist(recursive = FALSE) %>%
        bind_rows()
    } 
    dosing <- reactive({
      # browser()
      # dosing records converted by function
      # relative time to the first dosing timepoint
      # combine multiple dosages into one if have the same dosing timepoint 
      dosing <- treatment() %>% 
        doseduplicate("times") %>% 
        group_by(ord) %>% 
        mutate(relativetime = cumsum(interval) - interval + unique(timepoint)) %>%
        group_by(relativetime) %>%
        mutate(time = unique(relativetime),
               dose = sum(dosage)) %>% 
        ungroup() %>% 
        select(time, dose) %>% 
        distinct() %>%
        arrange(time)
    })
    
    # pkdes function -------
    pkdes <- function(t, y, p) {
      Vmax = p[1]
      Km = p[2]
      
      V2 = p[3]
      V3 = p[4]
      Q = p[5]
      
      Fr = p[6]
      KA = p[7]
      
      K23 = Q / V2
      K32 = Q / V3
      
      dy = vector(length = 1)
      
      dy[1] = -KA*y[1]
      CONC = y[2]/V2
      dy[2] = Fr*KA*y[1] + K32*y[3] - K23*y[2] - Vmax*CONC/(Km + CONC)
      dy[3] = K23*y[2] - K32*y[3]
      
      list(dy)
    }
    
    # pksim function with lsoda wrapped
    pksim <- function(data, func, parms, times, tlag) {
      # define pk dosing event data.frame
      dosevent <- data.frame(var = rep(1, nrow(data)),
                             time = data$time + tlag,
                             value = data$dose,
                             method = rep("add", nrow(data)))
      # keep lsoda function as primary style
      sim <-  lsoda(y = c(y1 = 0, y2 = 0, y3 = 0), 
                    times = times, 
                    func = func,
                    parms = parms,
                    events = list(data=dosevent),
                    rtol = 1e-05, atol = 1e-05) %>% 
        as.data.frame()
      
      pksim <- data %>%
        left_join(sim, ., "time")
    }
    
    
    # pkconc data - simulation under deSolve ---------------------------------------------
    pkconc <- reactive({
      # browser()
      pkparams <- c(input$vmax,
                    input$km,
                    input$v2, 
                    input$v3,
                    input$q,
                    input$fr,
                    input$ka)
      obstimes <- seq(0, (max(dosing()$time) + 72)*10, 1)/10
      pkconc <- pksim(data = dosing(),
                      func = pkdes,
                      parms = pkparams,
                      times = obstimes,
                      tlag = input$tlag) %>% 
        mutate(conc = y2/input$v2) %>% 
        select(time, dose, conc, y1, y2, y3)
      return(pkconc)
    })

        
    # prevent reactive expressions from crossing confliting or dependence  -----------
    reac <- reactiveValues(auto = c(), AUCrange = c(), AUC = c(), TRT = c(), DOS = c(), PKCONC = c())
    observe({
      input$partialauc
      reac$AUCrange <- isolate(input$partialauc)
    })
    observe({
      treatment()
      dosing()
      pkconc()
      reac$PKCONC <- pkconc()
      reac$TRT <- treatment()
      reac$DOS <- dosing()
    })
    
    observe({
      input$partialauc
      reac$auto <- FALSE
    })
    observe({
      input$autoauc
      reac$auto <- TRUE
    })
    
    # time range UI for partial AUC ------------------------------------------
    output$sliderAUC <- renderUI({
      trt <- reac$TRT
      if(reac$auto) trt <- treatment()
      dos <- reac$DOS
      
      minAUC <- 0
      maxAUC <- last(dos$time) + 72
      startAUC <- max(trt$timepoint + trt$interval*(trt$times - 1))
      endAUC <- max(trt$timepoint + trt$interval*trt$times)
      # other purpose: 
      # startAUC <- last(trt$timepoint) + last(trt$interval)*(last(trt$times) - 1)
      # endAUC <- last(trt$timepoint) + last(trt$interval)*last(trt$times)
      
      sliderInput("partialauc", label = NULL,
                  min = minAUC, max = maxAUC, step = 1,
                  value = c(startAUC, endAUC))
    })
    

    # time range UI for observed time -----------------------------------------
    output$sliderOBSTIME <- renderUI({
      # trt <- reac$TRT
      # if(reac$auto) trt <- treatment()
      dos <- reac$DOS
      
      minOBS <- 0
      maxOBS <- last(dos$time) + 72 + 24
      startOBS <- 0
      endOBS <- last(dos$time) + 72
      
      sliderInput("observedtime", label = NULL, width = "98%",
                  min = minOBS, max = maxOBS, step = 1, 
                  value = c(startOBS, endOBS))
    })
    
    # pkplot function  ------------------------------------
    pkplot <- function(data, x, y, dosing) {
      # define appropriate concentration range (height for y axis)
      concrange.txt <- data.frame(
        x = rep(max(dosing$time) + 72, 2)*0.95,
        y = c(input$concrange[2], input$concrange[1]) + max(input$concrange[2], data$conc + 1)*0.05,
        name = paste0(c(input$concrange[2], input$concrange[1]), " mg/L")
      )
      # ggplot!
      ggplot(data = data, aes(x = x, y = y)) +
        geom_line(size = 1) +
        # dynamic AUC showing
        geom_area(aes(x = ifelse(x >= reac$AUCrange[1] & x <= reac$AUCrange[2], x, NA_real_)),
                  fill = "#9898fb", alpha = 0.5,  colour = "black", size = 0.15, na.rm = TRUE) +
        # dynamic TDM range showing
        geom_hline(aes(yintercept = input$concrange[2]), colour = "red", linetype = "dashed", size = 1.5) +
        geom_hline(aes(yintercept = input$concrange[1]), colour = "#0080ff", linetype = "dashed", size = 1.5) +
        geom_text(data = concrange.txt, aes(label = name, fontface = 2), colour = c("red", "#0080ff"), size = 5) +
        theme_bw(base_rect_size = 1) +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 16),
              axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
        xlab("Time after first dose (hr)") + ylab("Concentration (mg/L)")
    }
    
    # pkconc plot  ---------------------------------------------------------
    output$pkconcplot1 <- renderPlot({
      pk <- reac$PKCONC %>% 
        filter(time>=input$observedtime[1],
               time<=input$observedtime[2])
      trt <- reac$TRT
      dos <- reac$DOS
      
      pkplot(pk, pk$time, pk$conc, dos) +
        scale_x_continuous(breaks = seq(0, min(trt$interval)*100, min(trt$interval)),
                           limits = c(input$observedtime[1], input$observedtime[2]))
    })
    
    
    # partial AUC from NCA --------------------------------------------------
    output$auclower <- renderText({reac$AUCrange[1]})
    output$aucupper <- renderText({reac$AUCrange[2]}) 
    reacAUC <- reactive({
      # PKNCA packages code as followed
      # (since ncappc packages can't be used in linux sys)
      pk <- reac$PKCONC %>%
        mutate(subject = 1, time = time, conc = conc) %>%
        distinct() %>%
        as.data.frame()
      auc <- PKNCAconc(pk, conc~time|subject) %>%
        PKNCAdata(.,intervals=data.frame(
          start=input$partialauc[1],
          end=input$partialauc[2],
          auclast=TRUE
        )) %>%
        pk.nca() %>%
        as.data.frame(.$result)
      round(auc$PPORRES, digits = 2)
      # alternatively you can use ncappc packages
      # pk <- reac$PKCONC %>% mutate(id = 1) %>% as.data.frame()
      # 
      # ncappc(pk, idNmObs = "id", timeNmObs = "time", concNmObs = "conc",
      #        evid = F, AUCTimeRange = input$partialauc,
      #        onlyNCA = T, noPlot = F, printOut = F)
      # round(ncaOutput$AUClower_upper, digits = 2)
    })
    
    # observe({
    #   reacAUC()
    #   reac$AUC <- reacAUC()
    # })
    # 
    output$pkauc1 <- renderText({
      # browser()
      reacAUC()
    })
    
    
    # DataTable in UI  ---------------------------------------------------------
    output$pkconcDT1 <- DT::renderDT(
      {reac$PKCONC%>%
          mutate(conc = round(conc, digits = 4),
                 y1 = round(y1, digits = 4),
                 y2 = round(y2, digits = 4),
                 y3 = round(y3, digits = 4))},
      options = list(
        lengthMenu = list(c(5, 10, 15, -1), 
                          c('5', '10', '15', 'All')),
        pageLength = 5,
        pagingType = "simple_numbers",
        # autoWidth = TRUE,
        scrollX = TRUE,
        rowCallback = DT::JS(
          'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
        )
    
    # Preview & Download  -------------------------------------------------------
    output$pkconcDT2 <- DT::renderDT(
      {reac$PKCONC%>%
          mutate(conc = round(conc, digits = 4),
                 y1 = round(y1, digits = 4),
                 y2 = round(y2, digits = 4),
                 y3 = round(y3, digits = 4))},
      options = list(
        lengthMenu = list(c(5, 10, 15, -1), 
                          c('5', '10', '15', 'All')),
        pageLength = 5,
        pagingType = "simple_numbers",
        # autoWidth = TRUE,
        scrollX = TRUE,
        rowCallback = DT::JS(
          'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
        )
    output$pkparameter <- renderTable({
      tibble('Vmax' = input$vmax, 'Km' = input$km,
             'V2' = input$v2, 'V3' = input$v3, 'Q' = input$q, 
             'Ka' = input$ka, 'Tlag' = input$tlag, 'F' = input$fr)
    })  
    output$pkdosing <- renderTable({
      tibble('给药时间' = reac$DOS$time,
             '给药剂量' = reac$DOS$dose)
    })
    output$pkconcplot2 <- renderPlot({
      pk <- reac$PKCONC %>% 
        filter(time>=input$observedtime[1],
               time<=input$observedtime[2])
      trt <- reac$TRT
      dos <- reac$DOS
      
      pkplot(pk, pk$time, pk$conc, dos) +
        scale_x_continuous(breaks = seq(0, min(trt$interval)*100, min(trt$interval)),
                           limits = c(input$observedtime[1], input$observedtime[2]))
    })
    
    output$pkauc2 <- renderText({
      # browser()
      reacAUC()
    })
    
    
    # generate pdf report -----------------------------------------------------
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(vmax = input$vmax,
                       km = input$km,
                       v2 = input$v2,
                       v3 = input$v3,
                       q = input$q,
                       ka = input$ka,
                       tlag = input$tlag,
                       fr = input$fr,
                       TRT = reac$TRT,
                       DOS = reac$DOS,
                       PKCONC = reac$PKCONC,
                       concrange = input$concrange,
                       partialauc = input$partialauc,
                       AUCrange = reac$AUCrange,
                       observedtime = input$observedtime,
                       txt_pkmodelstr = input$txt_pkmodelstr,
                       txt_pkparameter= input$txt_pkparameter,
                       txt_pkdosing= input$txt_pkdosing,
                       txt_pkconcplot2= input$txt_pkconcplot2,
                       txt_pkconcDT2= input$txt_pkconcDT2
        )
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    })
