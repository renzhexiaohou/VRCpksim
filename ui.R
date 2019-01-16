##################################################################
# Author: Yubo Xiao                                              #
# Phone: 13971656760                                             #
# email: xiaoyubocpuS@hotmail.com                                #
# script created: Jan,08,2019                                    #
# Software version: R 3.5.2                                      #
##################################################################
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)


shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
    navbarPage(
      title = ("伏立康唑血药浓度预测助手v0.1"),
      
      # first page: description -------------------------------------------------
      tabPanel("软件说明",
               h3(strong("参考文献")),
               hr(),
               h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'>Understanding variability with voriconazole using a population pharmacokinetic approach: implications for optimal dosing</body></strong>")),
               h5(HTML("<strong style='color:#000000'></strong> <a style=color:#3498db; href = 'https://academic.oup.com/jac/article/69/6/1633/834658'> Dolton MJ, Mikus G, Weiss J, Ray JE, McLachlan AJ.  Br J Clin Pharmacol. 2014 Aug;78(2):343-52.</a>")),
               h5(HTML("<strong style='color:#000000'>Objectives:</strong> Voriconazole exhibits highly variable, non-linear pharmacokinetics and is associated with a narrow therapeutic range. This study aimed to investigate the population pharmacokinetics of voriconazole in adults, including the effect of CYP2C19 genotype and drug-drug interactions.")),
               h5(HTML("<strong style='color:#000000;'>Methods:</strong> Non-linear mixed effects modelling (NONMEM) was undertaken of six voriconazole studies in healthy volunteers and patients. Dosing simulations to examine influential covariate effects and voriconazole target attainment (2-5 mg/L) stratified by CYP2C19 phenotype were performed.")),
               h5(HTML("<strong style='color:#000000;'>Results:</strong> We analysed 3352 voriconazole concentration measurements from 240 participants. A two-compartment pharmacokinetic model with first-order oral absorption with lag time and Michaelis-Menten elimination best described voriconazole pharmacokinetics. Participants with one or more CYP2C19 loss-of-function (LoF) alleles had a 41.2% lower Vmax for voriconazole. Co-administration of phenytoin or rifampicin, St John's wort or glucocorticoids significantly increased voriconazole elimination. Among patients receiving 200 mg of voriconazole twice daily, predicted trough concentrations on day 7 were <2 mg/L for oral and intravenous regimens for 72% and 63% of patients without CYP2C19 LoF alleles, respectively, with 49% and 35% below this threshold with 300 mg twice daily dosing. Conversely, these regimens resulted in 29%, 39%, 57% and 77% of patients with CYP2C19 LoF alleles with voriconazole trough concentrations ≥5 mg/L.")),
               h5(HTML("<strong style='color:#000000;background-color:#F8F9F9'>Conclusion:</strong> Current dosing regimens for voriconazole result in subtherapeutic exposure in many patients without CYP2C19 LoF alleles, suggesting the need for higher doses, whereas these regimens result in supratherapeutic exposure in a high proportion of patients with reduced CYP2C19 activity. These findings support the essential role of therapeutic drug monitoring in ensuring efficacious and safe voriconazole exposure.")),
               img(src="pkmodelstr.png",height = 150),
               br(),
               h3(strong("使用申明")),
               hr(),
               h4("*仅供临床药师或医师参考"),
               img(src="2sdh.png",height = 55, width = 225)
      ),
      
      # second page: simulation -------------------------------------------------
      tabPanel("模拟预测",
               #左栏
               fluidRow(
                 column(4,
                        style = "background: #ffffff",
                        # 模型参数的设定 ----------------------------------------
                        titlePanel(h3(strong("模型参数"))),
                        # hr(),
                        wellPanel(
                          style = "background: #ededed",
                          fluidRow(
                            column(4,numericInput("ka", "Ka (1/h)", value = 0.53)),
                            column(4,numericInput("tlag", "Tlag (h)", value = 0.162)),
                            column(4,numericInput("fr", "F", value = 0.942))
                          ),
                          fluidRow(
                            column(6,numericInput("vmax", "Vmax (L/h)", value = 43.9)),
                            column(6,numericInput("km", "Km (mg/L)", value = 3.33))
                          ),
                          fluidRow(
                            column(6, numericInput("v2", "V2 (L)", value = 27.1)),
                            column(6, numericInput("v3", "V3 (L)", value = 127)),
                            column(6, numericInput("q", "Q (L/h)", value = 35.1))
                          ),
                          h6("* PK参数及协变量默认值基于参考文献")
                        ),
                        # 给药方案的设定 ------------------------------------------
                        titlePanel(h3(strong("给药方案"))),
                        wellPanel(
                          style = "background: #ededed",
                          fluidRow(
                            column(3, numericInput("timepoint0", "医嘱时间", value = 0, min = 0, step = 1)),
                            column(3, numericInput("dosage0", "剂量 (mg)", value = 400, min = 0, step = 100)),
                            column(3, numericInput("interval0", "间隔", value = 12, min = 0, step = 2)),
                            column(3, numericInput("times0", "次数", value = 6, min = 1, step = 1)),
                            tags$div(id = 'placeholder')
                          ),
                          wellPanel(style='background: #ededed; padding:0px;',
                                    fluidRow(style='background: #ededed; padding:0px;',
                                             column(9,style='background: #ededed; padding:0px;',
                                                    wellPanel(style='background: #ededed; padding:0px;',
                                                              column(4, style='background: #ededed; padding:14px;',
                                                                     h5(strong("观测时间"))
                                                              ),
                                                              column(8,style='background: #ededed; padding:4px;',
                                                                     # tags$style(type = "text/css", ".irs-slider {width: 20px; height: 20px; top: 15px;}"),
                                                                     uiOutput("sliderOBSTIME", inline = T)
                                                              )
                                                    )
                                             ),
                                             column(3,style='background: #ededed; padding:0px;',
                                                    wellPanel(style='background: #ededed;',
                                                              column(6,style='background: #ededed; padding:1px;', 
                                                                     actionButton('insertBtn', NULL, width = "85%",
                                                                                  icon = icon(":O", class = "glyphicon glyphicon-plus"),
                                                                                  style='padding:5px;
                                                                                  color: white; background-color: #ec4c3c;
                                                                                  border-width: 0px;
                                                                                  font-size:70%')), 
                                                              column(6, style='background: #ededed; padding:1px;',
                                                                     actionButton('removeBtn', NULL, width = "85%",
                                                                                  icon = icon(":X", class = "glyphicon glyphicon-minus"),
                                                                                  style='padding:5px;
                                                                                  color: white; background-color: #3498db;
                                                                                  border-width: 0px;
                                                                                  font-size:70%'))
                                                                     )
                                                                     )
                                             # actionButton('steadystate', NULL, width = "0%",
                                             #              icon = icon(":X", class = "glyphicon glyphicon-minus"),
                                             #              style='padding:5px;
                                             #              color: white; background-color: #3498db;
                                             #              border-width: 0px;
                                             #              font-size:70%')
                                                                     )
                                                              ),
                          # h6("* 给药剂量默认值为说明书推荐日剂量"),
                          # hr(),
                          titlePanel(h5(strong("浓度范围 (mg/L)"))),
                          sliderInput("concrange", label = NULL, min = 0, max = 10, step = 0.1, value = c(1,5)),
                          h6("* 根据临床指南，建议伏立康唑的治疗窗为1-5mg/L")
                                    )
                        ),
                 column(8,
                        # 药时曲线呈现  ----------------------------------------
                        titlePanel(h3(strong("药时曲线"))),
                        br(),
                        plotOutput(outputId = "pkconcplot1"),
                        # 模拟数据呈现  ----------------------------------------
                        titlePanel(h3(strong("模拟数据"))),
                        div(DT::DTOutput("pkconcDT1"))
                 )
      ),
      # absolutePanel(
      #   top = 106, right = 458, width = 160, height = 10, draggable = FALSE,
      #   sliderInput("concrange1", label = NULL, min = 0, max = 10, step = 0.5, value = c(1,5))
      # ),
      # absolutePanel(
      #   top = 115, right = 345, width = 160, height = 10, draggable = FALSE,
      #   checkboxInput("autoauc", HTML("<strong>Auto AUC </strong>"), TRUE)
      # ),
      # absolutePanel(
      #   top = 121, right = 428, width = 10, height = 10, draggable = FALSE,
      #   actionButton('autoauc', NULL, icon("auto", class = "glyphicon glyphicon-refresh"),
      #                style='padding:0px;
      #                color: #3498db; background-color: #ffffff;
      #                border-width: 0px;
      #                font-size:70%')
      # ),
      absolutePanel(
        top = 105, right = 306, width = 160, height = 10, draggable = FALSE,
        titlePanel(h5(strong("Range")))
      ),
      absolutePanel(
        top = 105, right = 248, width = 160, height = 10, draggable = FALSE,
        uiOutput("sliderAUC", inline = T)
      ),
      absolutePanel(
        top = 123, right = 8, width = 220, height = 10, draggable = FALSE,
        HTML(paste0("<strong>AUC<sub>", textOutput(outputId = "auclower", inline = T), "-",
                    textOutput(outputId = "aucupper", inline = T), "hr</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                    textOutput(outputId = "pkauc1", inline = T), "</code> hr*mg/L</strong>"))
      )
      ),
      
      # third page: preview & download ! ----------------------------------------
      tabPanel("文档下载",
               fluidRow(
                 column(6,
                        h2(HTML("<strong><code style='color:#ec4c3c;background-color:#ffffff'>添加文字</code></strong>")),
                        hr(),
                        h3("结构模型"),
                        img(src="pkmodelstr.png",height = 200),
                        textInput("txt_pkmodelstr", "", "", 1000, "此处编辑"),
                        hr(),
                        h3("模型参数"),
                        tableOutput("pkparameter"),
                        textInput("txt_pkparameter", "", "", 1000, "此处编辑"),
                        hr(), 
                        h3("给药方案"),
                        tableOutput("pkdosing"),
                        textInput("txt_pkdosing", "", "", 1000, "此处编辑"),
                        hr(), 
                        h3("药时曲线"),
                        plotOutput("pkconcplot2"),
                        textInput("txt_pkconcplot2", "", "", 1000, "此处编辑"),
                        hr(),
                        h3("模拟数据"),
                        div(DT::DTOutput("pkconcDT2")),
                        textInput("txt_pkDT2", "", "", 1000, "此处编辑"),
                        hr()
                 ),
                 column(6,
                        h2(HTML("<strong><code style='color:#3498db;
                                background-color:#ffffff'>生成文档</code></strong>")),
                        hr(),
                        downloadButton("report", "点击下载", class = "btn-primary")
                        )
               )
      ),
      footer = h6("Copyright 2019 上海强世信息科技有限公司", align = "right")
    )
  )
)
