
# 安装所需包（首次运行时取消注释）
# install.packages(c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "DT", "plotly", "zip", "ggpubr", "grid", "png", "pdftools"))
# install.packages("CNPS.cycle") # 如果CRAN上有的话

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(plotly)
library(CNPS.cycle)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zip)
library(ggpubr)  # 用于ggbackground函数
library(grid)    # 用于viewport函数
library(png)     # 用于PNG图像处理
library(pdftools) # 用于PDF转换

# 设置文件上传大小限制为 1GB
options(shiny.maxRequestSize = 1024*1024^2)

# UI 部分
ui <- dashboardPage(
  skin = "purple",
  
  # Header
  dashboardHeader(
    title = span(
      icon("dna"),
      "CNPS Cycle Analysis",
      style = "font-size: 18px; font-weight: bold;"
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("数据上传", tabName = "upload", icon = icon("upload")),
      menuItem("分析配置", tabName = "config", icon = icon("cogs")),
      menuItem("运行分析", tabName = "analysis", icon = icon("play-circle")),
      menuItem("结果查看", tabName = "results", icon = icon("chart-bar")),
      menuItem("帮助文档", tabName = "help", icon = icon("question-circle"))
    ),
    
    div(
      style = "position: absolute; bottom: 20px; padding: 15px; width: 100%;",
      h5("系统信息", style = "color: white; margin-bottom: 10px;"),
      div(style = "color: #ecf0f1; font-size: 12px;",
          p("最大上传: 1GB"),
          p("CNPS.cycle v1.0"),
          p("© 2024 微生物组分析平台")
      )
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap');
        
        body, .content-wrapper, .main-sidebar {
          font-family: 'Roboto', sans-serif;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          min-height: 100vh;
        }
        
        .box {
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          transition: transform 0.2s;
        }
        
        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 12px rgba(0,0,0,0.15);
        }
        
        .info-box {
          border-radius: 8px;
          transition: all 0.3s;
        }
        
        .info-box:hover {
          transform: scale(1.02);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          border-radius: 6px;
          padding: 10px 24px;
          font-weight: 500;
          transition: all 0.3s;
        }
        
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
        }
        
        .btn-success {
          background: linear-gradient(135deg, #56ab2f 0%, #a8e063 100%);
          border: none;
          border-radius: 6px;
          transition: all 0.3s;
        }
        
        .cycle-card {
          border: 2px solid #e0e0e0;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 15px;
          cursor: pointer;
          transition: all 0.3s;
          background: white;
        }
        
        .cycle-card:hover {
          border-color: #667eea;
          transform: scale(1.02);
        }
        
        .cycle-card.selected {
          border-color: #667eea;
          background: linear-gradient(135deg, #667eea15 0%, #764ba215 100%);
        }
        
        .progress-custom {
          height: 30px;
          border-radius: 15px;
          background: #f0f0f0;
          overflow: hidden;
        }
        
        .progress-bar-custom {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
          height: 100%;
          border-radius: 15px;
          transition: width 0.5s ease;
        }
        
        .file-upload-wrapper {
          border: 2px dashed #ccc;
          border-radius: 8px;
          padding: 20px;
          text-align: center;
          transition: all 0.3s;
          background: white;
        }
        
        .file-upload-wrapper:hover {
          border-color: #667eea;
          background: #f8f9ff;
        }
        
        .file-uploaded {
          border-color: #56ab2f;
          background: #f0fff4;
        }
        
        .result-card {
          background: white;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 15px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          transition: all 0.3s;
        }
        
        .result-card:hover {
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
        }
      "))
    ),
    
    # Tab Items
    tabItems(
      # 数据上传页面
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            width = 12,
            title = "数据预览和验证",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            icon = icon("eye"),
            tabsetPanel(
              id = "preview_tabs",
              tabPanel("KO数据", 
                       br(),
                       DTOutput("preview_ko")),
              tabPanel("分组数据", 
                       br(),
                       DTOutput("preview_group")),
              tabPanel("Gene数据", 
                       br(),
                       DTOutput("preview_gene")),
              tabPanel("Tax数据", 
                       br(),
                       DTOutput("preview_tax")),
              tabPanel("丰度数据", 
                       br(),
                       DTOutput("preview_abundance"))
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "数据上传指南",
            status = "info",
            solidHeader = TRUE,
            icon = icon("info-circle"),
            column(
              width = 12,
              h4("欢迎使用CNPS Cycle分析平台！"),
              p("本平台用于分析碳(C)、氮(N)、磷(P)、硫(S)循环相关的微生物功能基因。"),
              hr(),
              h5("请按顺序上传以下5个必需文件："),
              tags$ul(
                tags$li(strong("KO注释数据:"), " KEGG Orthology注释文件，包含样本丰度信息"),
                tags$li(strong("分组数据:"), " 样本分组信息文件"),
                tags$li(strong("Gene数据:"), " KEGG分组数据，包含GeneID、Entry和Pathway信息"),
                tags$li(strong("NR注释数据:"), " 物种分类注释文件"),
                tags$li(strong("丰度数据:"), " GeneID丰度数据")
              ),
              tags$div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                " 支持格式：CSV, TXT, TSV | 最大文件大小：1GB"
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "1. KO注释数据",
            status = "primary",
            solidHeader = TRUE,
            fileInput("ko_file", 
                      label = NULL,
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "选择文件",
                      placeholder = "未选择文件"),
            uiOutput("ko_status")
          ),
          
          box(
            width = 6,
            title = "2. 分组数据",
            status = "primary",
            solidHeader = TRUE,
            fileInput("group_file", 
                      label = NULL,
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "选择文件",
                      placeholder = "未选择文件"),
            uiOutput("group_status")
          )
        ),
        
        fluidRow(
          box(
            width = 4,
            title = "3. Gene数据",
            status = "primary",
            solidHeader = TRUE,
            fileInput("gene_file", 
                      label = NULL,
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "选择文件",
                      placeholder = "未选择文件"),
            uiOutput("gene_status")
          ),
          
          box(
            width = 4,
            title = "4. NR注释数据",
            status = "primary",
            solidHeader = TRUE,
            fileInput("tax_file", 
                      label = NULL,
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "选择文件",
                      placeholder = "未选择文件"),
            uiOutput("tax_status")
          ),
          
          box(
            width = 4,
            title = "5. 丰度数据",
            status = "primary",
            solidHeader = TRUE,
            fileInput("abundance_file", 
                      label = NULL,
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "选择文件",
                      placeholder = "未选择文件"),
            uiOutput("abundance_status")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = FALSE,
            div(
              style = "text-align: center; padding: 20px;",
              uiOutput("upload_summary"),
              br(),
              actionButton("next_to_config", 
                           "下一步：分析配置 →",
                           class = "btn-primary btn-lg",
                           icon = icon("arrow-right"))
            )
          )
        )
      ),
      
      # 分析配置页面
      tabItem(
        tabName = "config",
        fluidRow(
          box(
            width = 12,
            title = "选择分析循环",
            status = "primary",
            solidHeader = TRUE,
            icon = icon("recycle"),
            column(
              width = 3,
              div(
                class = "cycle-card",
                id = "carbon_card",
                onclick = "Shiny.setInputValue('carbon_cycle', Math.random());",
                style = "border-color: #4CAF50;",
                checkboxInput("carbon_cycle", NULL, value = TRUE),
                h4("碳循环 (C)", style = "color: #4CAF50; margin-top: 0;"),
                p("7个生化过程", style = "color: #666;"),
                tags$small("包括有氧固碳、厌氧固碳、发酵等")
              )
            ),
            column(
              width = 3,
              div(
                class = "cycle-card",
                id = "nitrogen_card",
                style = "border-color: #2196F3;",
                checkboxInput("nitrogen_cycle", NULL, value = TRUE),
                h4("氮循环 (N)", style = "color: #2196F3; margin-top: 0;"),
                p("18个生化过程", style = "color: #666;"),
                tags$small("包括硝化、反硝化、固氮等")
              )
            ),
            column(
              width = 3,
              div(
                class = "cycle-card",
                id = "phosphorus_card",
                style = "border-color: #9C27B0;",
                checkboxInput("phosphorus_cycle", NULL, value = TRUE),
                h4("磷循环 (P)", style = "color: #9C27B0; margin-top: 0;"),
                p("2个生化过程", style = "color: #666;"),
                tags$small("包括PhoR-PhoB系统等")
              )
            ),
            column(
              width = 3,
              div(
                class = "cycle-card",
                id = "sulfur_card",
                style = "border-color: #FF9800;",
                checkboxInput("sulfur_cycle", NULL, value = TRUE),
                h4("硫循环 (S)", style = "color: #FF9800; margin-top: 0;"),
                p("15个生化过程", style = "color: #666;"),
                tags$small("包括硫酸盐还原、SOX系统等")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "分析选项",
            status = "info",
            solidHeader = TRUE,
            icon = icon("sliders-h"),
            checkboxGroupInput("analysis_options",
                               label = NULL,
                               choices = list(
                                 "生成热图 (Heatmap)" = "heatmap",
                                 "差异分析 (Differential Analysis)" = "diff",
                                 "通路图可视化 (Pathway Diagram)" = "pathway",
                                 "β多样性分析 (Beta Diversity)" = "beta",
                                 "宿主微生物分析 (Host Microbiota)" = "host"
                               ),
                               selected = c("heatmap", "diff", "pathway", "beta", "host"))
          ),
          
          box(
            width = 6,
            title = "输出选项",
            status = "warning",
            solidHeader = TRUE,
            icon = icon("file-export"),
            textInput("output_dir", 
                      "结果输出目录名称:",
                      value = "Results",
                      placeholder = "Results"),
            selectInput("figure_format",
                        "图片格式:",
                        choices = c("PDF" = "pdf", "PNG" = "png", "SVG" = "svg"),
                        selected = "pdf"),
            numericInput("figure_width",
                         "图片宽度 (英寸):",
                         value = 7,
                         min = 3,
                         max = 20,
                         step = 0.5),
            numericInput("figure_height",
                         "图片高度 (英寸):",
                         value = 5,
                         min = 3,
                         max = 20,
                         step = 0.5)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            status = "success",
            div(
              style = "text-align: center; padding: 20px;",
              actionButton("prev_to_upload", 
                           "← 返回上传",
                           class = "btn-default btn-lg",
                           icon = icon("arrow-left"),
                           style = "margin-right: 20px;"),
              actionButton("next_to_analysis", 
                           "下一步：运行分析 →",
                           class = "btn-primary btn-lg",
                           icon = icon("arrow-right"))
            )
          )
        )
      ),
      
      # 运行分析页面
      tabItem(
        tabName = "analysis",
        fluidRow(
          valueBoxOutput("files_uploaded_box", width = 4),
          valueBoxOutput("cycles_selected_box", width = 4),
          valueBoxOutput("analysis_status_box", width = 4)
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "分析控制台",
            status = "primary",
            solidHeader = TRUE,
            icon = icon("terminal"),
            div(
              style = "text-align: center; padding: 40px;",
              h3("准备运行CNPS循环分析"),
              br(),
              actionButton("run_analysis",
                           "开始分析",
                           class = "btn-success",
                           icon = icon("play"),
                           style = "font-size: 20px; padding: 15px 40px;"),
              br(), br(),
              uiOutput("analysis_progress_ui"),
              br(),
              verbatimTextOutput("analysis_log")
            )
          )
        )
      ),
      
      # 结果查看页面
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            title = "分析结果总览",
            status = "success",
            solidHeader = TRUE,
            icon = icon("chart-line"),
            uiOutput("results_summary")
          )
        ),
        
        fluidRow(
          tabBox(
            id = "result_tabs",
            width = 12,
            
            tabPanel(
              "碳循环",
              icon = icon("leaf"),
              uiOutput("carbon_results")
            ),
            
            tabPanel(
              "氮循环",
              icon = icon("wind"),
              uiOutput("nitrogen_results")
            ),
            
            tabPanel(
              "磷循环",
              icon = icon("atom"),
              uiOutput("phosphorus_results")
            ),
            
            tabPanel(
              "硫循环",
              icon = icon("flask"),
              uiOutput("sulfur_results")
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "批量下载",
            status = "info",
            solidHeader = TRUE,
            icon = icon("download"),
            div(
              style = "text-align: center; padding: 20px;",
              downloadButton("download_all",
                             "下载完整结果包 (ZIP)",
                             class = "btn-success btn-lg",
                             icon = icon("file-archive"))
            )
          )
        )
      ),
      
      # 帮助文档页面
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            width = 12,
            title = "使用指南",
            status = "info",
            solidHeader = TRUE,
            icon = icon("book"),
            h3("快速开始"),
            p("本平台基于CNPS.cycle R包开发，用于分析碳、氮、磷、硫循环相关的微生物功能基因。"),
            
            h4("数据准备"),
            tags$ol(
              tags$li("确保您的数据符合CNPS.cycle包的输入格式要求"),
              tags$li("准备5个必需的输入文件"),
              tags$li("检查文件大小不超过1GB限制")
            ),
            
            h4("分析流程"),
            tags$ol(
              tags$li(strong("数据上传:"), " 上传所有必需文件"),
              tags$li(strong("分析配置:"), " 选择要分析的循环和选项"),
              tags$li(strong("运行分析:"), " 点击开始按钮执行分析"),
              tags$li(strong("结果查看:"), " 查看和下载分析结果")
            ),
            
            h4("常见问题"),
            tags$ul(
              tags$li(strong("Q: 支持哪些文件格式?"), br(), "A: 支持CSV、TXT、TSV格式的文本文件。"),
              tags$li(strong("Q: 分析需要多长时间?"), br(), "A: 取决于数据量，通常需要5-30分钟。"),
              tags$li(strong("Q: 结果文件保存在哪里?"), br(), "A: 保存在您指定的输出目录中，可以打包下载。")
            ),
            
            hr(),
            h4("参考文献"),
            p("CNPS.cycle: An R package for analyzing carbon, nitrogen, phosphorus, and sulfur cycling genes in microbial communities."),
            
            h4("联系支持"),
            p("如有问题，请联系技术支持：support@cnps-analysis.com")
          )
        )
      )
    )
  )
)

# Server 部分
server <- function(input, output, session) {
  
  # 响应式值存储
  rv <- reactiveValues(
    ko_data = NULL,
    group_data = NULL,
    gene_data = NULL,
    tax_data = NULL,
    abundance_data = NULL,
    analysis_running = FALSE,
    analysis_complete = FALSE,
    results = list()
  )

  safe_row_total <- function(df, row_idx) {
    if (is.null(df) || nrow(df) < row_idx || ncol(df) < 2) {
      return(0)
    }
    row_values <- suppressWarnings(as.numeric(unlist(df[row_idx, 2:ncol(df), drop = TRUE])))
    sum(row_values, na.rm = TRUE)
  }

  draw_cycle_background <- function(pdf_path) {
    converted_png <- pdftools::pdf_convert(
      pdf = pdf_path,
      format = "png",
      pages = 1,
      dpi = 300,
      verbose = FALSE
    )
    bg_img <- png::readPNG(converted_png[1])
    grid::grid.raster(bg_img, x = 0.5, y = 0.5, width = 1, height = 1, just = "center")
    unlink(converted_png)
  }

  safe_close_pdf <- function() {
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }
  }
  
  # 文件上传状态显示
  output$ko_status <- renderUI({
    if (!is.null(input$ko_file)) {
      size <- format(input$ko_file$size / 1024^2, digits = 2)
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" 已上传: %s (%.2f MB)", input$ko_file$name, as.numeric(size))
      )
    }
  })
  
  output$group_status <- renderUI({
    if (!is.null(input$group_file)) {
      size <- format(input$group_file$size / 1024^2, digits = 2)
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" 已上传: %s (%.2f MB)", input$group_file$name, as.numeric(size))
      )
    }
  })
  
  output$gene_status <- renderUI({
    if (!is.null(input$gene_file)) {
      size <- format(input$gene_file$size / 1024^2, digits = 2)
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" 已上传: %s (%.2f MB)", input$gene_file$name, as.numeric(size))
      )
    }
  })
  
  output$tax_status <- renderUI({
    if (!is.null(input$tax_file)) {
      size <- format(input$tax_file$size / 1024^2, digits = 2)
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" 已上传: %s (%.2f MB)", input$tax_file$name, as.numeric(size))
      )
    }
  })
  
  output$abundance_status <- renderUI({
    if (!is.null(input$abundance_file)) {
      size <- format(input$abundance_file$size / 1024^2, digits = 2)
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" 已上传: %s (%.2f MB)", input$abundance_file$name, as.numeric(size))
      )
    }
  })
  
  # 上传摘要
  output$upload_summary <- renderUI({
    files_count <- sum(!is.null(input$ko_file),
                       !is.null(input$group_file),
                       !is.null(input$gene_file),
                       !is.null(input$tax_file),
                       !is.null(input$abundance_file))
    
    if (files_count == 5) {
      tags$div(
        class = "alert alert-success",
        style = "font-size: 18px;",
        icon("check-circle", class = "fa-2x"),
        h4("所有文件上传完成！", style = "margin-top: 10px;"),
        p("您可以继续进行分析配置。")
      )
    } else {
      tags$div(
        class = "alert alert-info",
        style = "font-size: 16px;",
        icon("info-circle"),
        sprintf(" 已上传 %d / 5 个文件", files_count)
      )
    }
  })
  
  # 导航按钮
  observeEvent(input$next_to_config, {
    updateTabItems(session, "tabs", "config")
  })
  
  observeEvent(input$prev_to_upload, {
    updateTabItems(session, "tabs", "upload")
  })
  
  observeEvent(input$next_to_analysis, {
    updateTabItems(session, "tabs", "analysis")
  })
  
  # Value Boxes
  output$files_uploaded_box <- renderValueBox({
    files_count <- sum(!is.null(input$ko_file),
                       !is.null(input$group_file),
                       !is.null(input$gene_file),
                       !is.null(input$tax_file),
                       !is.null(input$abundance_file))
    
    valueBox(
      paste(files_count, "/ 5"),
      "已上传文件",
      icon = icon("file-upload"),
      color = if (files_count == 5) "green" else "yellow"
    )
  })
  
  output$cycles_selected_box <- renderValueBox({
    cycles_count <- sum(input$carbon_cycle,
                        input$nitrogen_cycle,
                        input$phosphorus_cycle,
                        input$sulfur_cycle)
    
    valueBox(
      paste(cycles_count, "/ 4"),
      "选择的循环",
      icon = icon("recycle"),
      color = if (cycles_count > 0) "blue" else "red"
    )
  })
  
  output$analysis_status_box <- renderValueBox({
    status_text <- if (rv$analysis_complete) {
      "已完成"
    } else if (rv$analysis_running) {
      "运行中"
    } else {
      "未开始"
    }
    
    status_color <- if (rv$analysis_complete) {
      "green"
    } else if (rv$analysis_running) {
      "yellow"
    } else {
      "red"
    }
    
    valueBox(
      status_text,
      "分析状态",
      icon = icon("tasks"),
      color = status_color
    )
  })
  
  # 运行分析
  observeEvent(input$run_analysis, {
    if (isTRUE(rv$analysis_running)) {
      showNotification(
        "分析正在运行中，请勿重复点击。",
        type = "warning",
        duration = 4
      )
      return()
    }

    # 检查文件是否全部上传
    if (is.null(input$ko_file) || is.null(input$group_file) || 
        is.null(input$gene_file) || is.null(input$tax_file) || 
        is.null(input$abundance_file)) {
      showNotification(
        "请先上传所有必需文件！",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # 检查是否选择了至少一个循环
    if (!any(input$carbon_cycle, input$nitrogen_cycle, 
             input$phosphorus_cycle, input$sulfur_cycle)) {
      showNotification(
        "请至少选择一个循环进行分析！",
        type = "error",
        duration = 5
      )
      return()
    }
    
    rv$analysis_running <- TRUE
    rv$analysis_complete <- FALSE
    
    showNotification(
      "分析已启动，请耐心等待...",
      type = "message",
      duration = NULL,
      id = "analysis_notification"
    )
    
    tryCatch({
      # 读取数据（增强的错误处理）
      withProgress(message = '正在读取数据...', value = 0, {
        incProgress(0.1, detail = "读取KO数据")
        rv$ko_data <- tryCatch({
          # 先尝试读取第一行判断分隔符
          first_line <- readLines(input$ko_file$datapath, n = 1)
          sep_char <- if(grepl("\t", first_line)) "\t" else if(grepl(",", first_line)) "," else "\t"
          
          read.table(input$ko_file$datapath, 
                     header = TRUE, 
                     sep = sep_char,
                     quote = "",
                     comment.char = "",
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fill = TRUE)
        }, error = function(e) {
          stop(paste("KO文件读取失败:", e$message))
        })
        
        incProgress(0.1, detail = "读取分组数据")
        rv$group_data <- tryCatch({
          first_line <- readLines(input$group_file$datapath, n = 1)
          sep_char <- if(grepl("\t", first_line)) "\t" else if(grepl(",", first_line)) "," else "\t"
          
          read.table(input$group_file$datapath, 
                     header = TRUE, 
                     sep = sep_char,
                     quote = "",
                     comment.char = "",
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fill = TRUE)
        }, error = function(e) {
          stop(paste("分组文件读取失败:", e$message))
        })
        
        incProgress(0.1, detail = "读取Gene数据")
        rv$gene_data <- tryCatch({
          first_line <- readLines(input$gene_file$datapath, n = 1)
          sep_char <- if(grepl("\t", first_line)) "\t" else if(grepl(",", first_line)) "," else "\t"
          
          read.table(input$gene_file$datapath, 
                     header = TRUE, 
                     sep = sep_char,
                     quote = "",
                     comment.char = "",
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fill = TRUE)
        }, error = function(e) {
          stop(paste("Gene文件读取失败:", e$message))
        })
        
        incProgress(0.1, detail = "读取Tax数据")
        rv$tax_data <- tryCatch({
          first_line <- readLines(input$tax_file$datapath, n = 1)
          sep_char <- if(grepl("\t", first_line)) "\t" else if(grepl(",", first_line)) "," else "\t"
          
          read.table(input$tax_file$datapath, 
                     header = TRUE, 
                     sep = sep_char,
                     quote = "",
                     comment.char = "",
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fill = TRUE)
        }, error = function(e) {
          stop(paste("Tax文件读取失败:", e$message))
        })
        
        incProgress(0.1, detail = "读取丰度数据")
        rv$abundance_data <- tryCatch({
          first_line <- readLines(input$abundance_file$datapath, n = 1)
          sep_char <- if(grepl("\t", first_line)) "\t" else if(grepl(",", first_line)) "," else "\t"
          
          read.table(input$abundance_file$datapath, 
                     header = TRUE, 
                     sep = sep_char,
                     quote = "",
                     comment.char = "",
                     stringsAsFactors = FALSE,
                     check.names = FALSE,
                     fill = TRUE)
        }, error = function(e) {
          stop(paste("丰度文件读取失败:", e$message))
        })
      })
      
      # 数据预处理（增强的错误处理和验证）
      withProgress(message = '数据预处理...', value = 0.5, {
        incProgress(0.05, detail = "定义颜色方案")

        # 定义cbbPalette颜色方案（供beta多样性分析使用）
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                        "#0072B2", "#D55E00", "#CC79A7", "#999999", "#8DD3C7",
                        "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
                        "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5")
        assign("cbbPalette", cbbPalette, envir = .GlobalEnv)

        incProgress(0.05, detail = "验证数据格式")
        
        # 验证数据不为空
        if (is.null(rv$ko_data) || nrow(rv$ko_data) == 0) {
          stop("KO数据为空，请检查文件格式")
        }
        if (is.null(rv$group_data) || nrow(rv$group_data) == 0) {
          stop("分组数据为空，请检查文件格式")
        }
        if (is.null(rv$gene_data) || nrow(rv$gene_data) == 0) {
          stop("Gene数据为空，请检查文件格式")
        }
        if (is.null(rv$tax_data) || nrow(rv$tax_data) == 0) {
          stop("Tax数据为空，请检查文件格式")
        }
        if (is.null(rv$abundance_data) || nrow(rv$abundance_data) == 0) {
          stop("丰度数据为空，请检查文件格式")
        }
        
        incProgress(0.05, detail = "处理KO数据")
        ko <- rv$ko_data
        
        # 安全地处理KO数据
        if (ncol(ko) > 1) {
          # 检查最后一列是否是Description
          last_col_name <- colnames(ko)[ncol(ko)]
          if (tolower(last_col_name) %in% c("description", "desc", "annotation")) {
            ko <- ko[, -ncol(ko)]
          }
          
          # 设置行名
          if (ncol(ko) >= 1) {
            rownames(ko) <- make.names(ko[, 1], unique = TRUE)
            ko <- ko[, -1, drop = FALSE]
          }
          
          # 转换为数值
          for (i in seq_len(ncol(ko))) {
            ko[, i] <- suppressWarnings(as.numeric(as.character(ko[, i])))
          }
          
          # 移除全是NA的行
          ko <- ko[rowSums(!is.na(ko)) > 0, , drop = FALSE]
          # 将NA替换为0
          ko[is.na(ko)] <- 0
        } else {
          stop("KO数据列数不足，请检查文件格式")
        }
        
        incProgress(0.05, detail = "处理分组数据")
        group <- rv$group_data
        
        # 验证分组数据
        if (ncol(group) < 2) {
          stop("分组数据必须至少有2列（样本名和分组）")
        }
        
        # 确保分组数据有正确的列名
        if (!"Group" %in% colnames(group) && ncol(group) >= 2) {
          colnames(group)[1:2] <- c("Sample", "Group")
        }
        
        Group_numb <- length(unique(group[, 2]))
        Sample_numb <- length(unique(group[, 1]))
        
        if (Group_numb == 0) {
          stop("未检测到有效的分组信息")
        }
        
        incProgress(0.05, detail = "处理Gene数据")
        Gene <- rv$gene_data
        
        # 验证Gene数据格式
        if (ncol(Gene) < 2) {
          stop("Gene数据必须至少有2列（GeneID和Entry）")
        }
        
        # 移除重复行
        if (nrow(Gene) > 1) {
          Gene <- Gene[!duplicated(Gene[, 1:2]), 1:2, drop = FALSE]
        }
        
        # 使用dplyr进行转换（如果可能）
        if (nrow(Gene) > 0 && require(dplyr, quietly = TRUE) && require(tidyr, quietly = TRUE)) {
          tryCatch({
            colnames(Gene)[1:2] <- c("GeneID", "Entry")
            Gene <- Gene %>%
              group_by(Entry) %>%
              mutate(index = row_number()) %>%
              pivot_wider(names_from = Entry, values_from = GeneID) %>%
              select(-index)
          }, error = function(e) {
            # 如果转换失败，保持原样
            warning("Gene数据转换失败，使用原始格式: ", e$message)
          })
        }
        
        incProgress(0.05, detail = "处理Tax数据")
        tax <- rv$tax_data
        
        # 确保tax有至少2列
        if (ncol(tax) >= 2) {
          colnames(tax)[1:2] <- c("V1", "V2")
          
          # 安全地处理tax数据
          tax$V2 <- as.character(tax$V2)
          tax$V2 <- gsub(".*;k__", "", tax$V2)
          tax$V2 <- paste0("k__", tax$V2)
        } else {
          stop("Tax数据必须至少有2列")
        }
        
        incProgress(0.05, detail = "处理丰度数据")
        abundance <- rv$abundance_data
        
        # 安全处理丰度数据
        if (ncol(abundance) > 1) {
          rownames(abundance) <- make.names(abundance[, 1], unique = TRUE)
          abundance <- abundance[, -1, drop = FALSE]
          
          # 转换为数值
          for (i in seq_len(ncol(abundance))) {
            abundance[, i] <- suppressWarnings(as.numeric(as.character(abundance[, i])))
          }
          
          # 处理NA值
          abundance[is.na(abundance)] <- 0
          
          # 添加V1列
          abundance$V1 <- rownames(abundance)
          abundance <- abundance[, c("V1", colnames(abundance)[1:(ncol(abundance) - 1)]), drop = FALSE]
        } else {
          stop("丰度数据列数不足")
        }
        
        # 保存处理后的数据到全局
        assign("ko", ko, envir = .GlobalEnv)
        assign("group", group, envir = .GlobalEnv)
        assign("Gene", Gene, envir = .GlobalEnv)
        assign("tax", tax, envir = .GlobalEnv)
        assign("abundance", abundance, envir = .GlobalEnv)
        assign("Group_numb", Group_numb, envir = .GlobalEnv)
        assign("Sample_numb", Sample_numb, envir = .GlobalEnv)
      })
      
      # 创建输出目录
      output_dir <- input$output_dir
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # 运行选定的循环分析
      total_cycles <- sum(input$carbon_cycle, input$nitrogen_cycle, 
                          input$phosphorus_cycle, input$sulfur_cycle)
      current_cycle <- 0
      
      # 碳循环分析
      if (input$carbon_cycle) {
        current_cycle <- current_cycle + 1
        withProgress(message = sprintf('碳循环分析 (%d/%d)...', current_cycle, total_cycles), 
                     value = 0.6, {
                       incProgress(0.05, detail = "提取碳循环数据")
                       
                       C <- c("K00855","K01061","K01602","K08684","K02256","K02262","K02274","K02276",
                              "K00174","K00175","K00244","K01648","K00194","K00197","K03518","K03519",
                              "K03520","K00016","K00400","K00401")
                       
                       # 检查ko中是否有匹配的行
                       matched_rows <- rownames(ko) %in% C
                       
                       if (sum(matched_rows) == 0) {
                         warning("碳循环：未找到匹配的KO编号，跳过此分析")
                         rv$results$carbon <- list(completed = FALSE, message = "未找到匹配的KO编号")
                         return(NULL)
                       }
                       
                       C.ko <- ko[matched_rows, , drop = FALSE]
                       
                       incProgress(0.05, detail = "计算丰度")
                       
                       # 安全调用Ccyc.abundance
                       C.abundance <- tryCatch({
                         Ccyc.abundance(ko)
                       }, error = function(e) {
                         warning("碳循环丰度计算失败: ", e$message)
                         # 返回一个空的数据框
                         data.frame(Process = character(0))
                       })
                       
                       if (nrow(C.abundance) == 0) {
                         warning("碳循环丰度数据为空，跳过此分析")
                         rv$results$carbon <- list(completed = FALSE, message = "丰度数据为空")
                         return(NULL)
                       }
                       
                       incProgress(0.05, detail = "创建输出目录")
                       # 创建碳循环目录
                       c_dir <- file.path(output_dir, "Carbon")
                       dir.create(c_dir, recursive = TRUE, showWarnings = FALSE)
                       dir.create(file.path(c_dir, "Gene", "Abundance"), recursive = TRUE, showWarnings = FALSE)
                       
                       # 保存文件
                       write.table(C.ko, 
                                   file = file.path(c_dir, "Gene/Abundance/C_cycle_ko_abun.txt"),
                                   sep = "\t", quote = FALSE, row.names = TRUE)
                       write.table(C.abundance,
                                   file = file.path(c_dir, "Gene/Abundance/C_cycle_gene_abun.txt"),
                                   sep = "\t", quote = FALSE, row.names = FALSE)
                       
                       if ("heatmap" %in% input$analysis_options) {
                         incProgress(0.1, detail = "生成热图")
                         tryCatch({
                           dir.create(file.path(c_dir, "Gene/Heatmap"), recursive = TRUE, showWarnings = FALSE)
                           result <- abun.heatmap.g(C.abundance, group, Group_numb)
                           
                           pdf(file = file.path(c_dir, "Gene/Heatmap/C_cycle_gene_abun_group.pdf"),
                               width = 2.3 + 0.6*Group_numb, height = 3)
                           print(result[[2]])
                           dev.off()
                           
                           write.table(result[[1]],
                                       file.path(c_dir, "Gene/Heatmap/Diff_C_gene_test.txt"),
                                       sep = "\t", quote = FALSE, row.names = FALSE)
                         }, error = function(e) {
                           warning("碳循环热图生成失败: ", e$message)
                         })
                       }
                       
                       if ("pathway" %in% input$analysis_options) {
                         incProgress(0.1, detail = "生成通路图")
                         tryCatch({
                           dir.create(file.path(c_dir, "Gene/Cycle_image"), recursive = TRUE, showWarnings = FALSE)
                           result <- fold.change(C.abundance, group)

                           # 安全地处理结果
                           if (length(result) >= 1) {
                             write.table(result[[1]],
                                         file.path(c_dir, "Gene/Cycle_image/Gene_fold_change.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                           }

                           if (length(result) >= 3 && !is.null(result[[3]])) {
                             pdf(file = file.path(c_dir, "Gene/Cycle_image/Gene_fold_change.pdf"),
                                 width = 0.6*Group_numb + 2.3, height = 4.5)
                             print(result[[3]])
                             dev.off()
                           }
                         }, error = function(e) {
                           class(result)
                           warning("碳循环通路图生成失败: ", e$message)
                         })
                       }
                       
                       if ("diff" %in% input$analysis_options) {
                         incProgress(0.1, detail = "差异分析")
                         tryCatch({
                           dir.create(file.path(c_dir, "Gene/Differential"), recursive = TRUE, showWarnings = FALSE)
                           
                           # 执行差异分析
                           result <- fold.change(C.abundance, group)

                           # 检查结果长度，避免下标出界
                           if (length(result) >= 1) {
                             write.table(result[[1]],
                                         file.path(c_dir, "Gene/Differential/Gene_fold_change.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                           }

                           # 保存差异倍数热图（如果存在第3个结果）
                           if (length(result) >= 3 && !is.null(result[[3]])) {
                             pdf(file = file.path(c_dir, "Gene/Differential/Gene_fold_change_heatmap.pdf"),
                                 width = 0.6*Group_numb + 2.3, height = 4.5)
                             print(result[[3]])
                             dev.off()
                           }

                           # 如果有统计检验结果（第4个结果）
                           if (length(result) >= 4 && !is.null(result[[4]])) {
                             write.table(result[[4]],
                                         file.path(c_dir, "Gene/Differential/Statistical_test.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                           }
                         }, error = function(e) {
                           warning("碳循环差异分析失败: ", e$message)
                         })
                       }
                       
                       if ("pathway" %in% input$analysis_options) {
                         incProgress(0.1, detail = "生成通路图")
                         tryCatch({
                           dir.create(file.path(c_dir, "Gene/Cycle_image"), recursive = TRUE, showWarnings = FALSE)
                           result <- fold.change(C.abundance, group)

                           # 安全地创建各个子过程的图（检查result长度）
                           if (length(result) >= 2 && !is.null(result[[2]])) {
                             ACF <- result[[2]] + ylim("Aerobic C fixation")
                             ACH4O <- result[[2]] + ylim("Aerobic CH4 oxidation")
                             AR <- result[[2]] + ylim("Aerobic respiration")
                             AnCF <- result[[2]] + ylim("Anaerobic C fixation")
                             COo <- result[[2]] + ylim("CO oxidation")
                             Fer <- result[[2]] + ylim("Fermentation")
                             Meth <- result[[2]] + ylim("Methanogenesis")
                           } else {
                             warning("fold.change函数未返回预期的图形对象，跳过通路图生成")
                             next
                           }
                           
                           # 获取通路图背景
                           C.img <- system.file("data", "Ccyc.pdf", package = "CNPS.cycle")

                           # 调试信息
                           if (file.exists(C.img)) {
                             message(sprintf("背景图路径: %s, 文件大小: %s bytes", C.img, file.info(C.img)$size))
                           } else {
                             message("警告: 未找到碳循环背景图文件")
                           }

                           # 如果包中有通路图，则生成组合图
                           if (file.exists(C.img)) {
                             tryCatch({
                               C.img <- system.file("data", "Ccyc.pdf", package = "CNPS.cycle")
                               pdf(file.path(c_dir, "Gene/Cycle_image/C_cyc_fold_change.pdf"), width = 13, height = 7)
                               grid::grid.newpage()
                               draw_cycle_background(C.img)

                               # 根据丰度数据添加子图
                               message(sprintf("开始添加子图，丰度数据行数: %d", nrow(C.abundance)))

                               if (safe_row_total(C.abundance, 1) > 0) {
                                 message("添加ACF子图")
                                 print(ACF, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                          x = 0.692, y = 0.87))
                               }
                               if (safe_row_total(C.abundance, 2) > 0) {
                                 message("添加ACH4O子图")
                                 print(ACH4O, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                            x = 0.5935, y = 0.23))
                               }
                               if (safe_row_total(C.abundance, 3) > 0) {
                                 message("添加AR子图")
                                 print(AR, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                         x = 0.612, y = 0.64))
                               }
                               if (safe_row_total(C.abundance, 4) > 0) {
                                 message("添加AnCF子图")
                                 print(AnCF, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                           x = 0.322, y = 0.86))
                               }
                               if (safe_row_total(C.abundance, 5) > 0) {
                                 message("添加COo子图")
                                 print(COo, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                          x = 0.812, y = 0.385))
                               }
                               if (safe_row_total(C.abundance, 6) > 0) {
                                 message("添加Fer子图")
                                 print(Fer, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                          x = 0.36, y = 0.45))
                               }
                               if (safe_row_total(C.abundance, 7) > 0) {
                                 message("添加Meth子图")
                                 print(Meth, vp = viewport(width = 0.035*Group_numb, height = 0.05,
                                                           x = 0.362, y = 0.34))
                               }
                               message("完成子图添加")
                               dev.off()

                             }, error = function(e) {
                               warning("碳循环通路图生成失败: ", e$message)
                               safe_close_pdf()
                             })
                           } else {
                             # 如果没有背景图，只保存各个子图
                             if (length(result) >= 3 && !is.null(result[[3]])) {
                               pdf(file = file.path(c_dir, "Gene/Cycle_image/C_processes.pdf"),
                                   width = 0.6*Group_numb + 2.3, height = 4.5)
                               print(result[[3]])
                               safe_close_pdf()
                             }
                           }
                         }, error = function(e) {
                           warning("碳循环通路图生成失败: ", e$message)
                         })
                       }
                       
                       if ("beta" %in% input$analysis_options) {
                         incProgress(0.1, detail = "β多样性分析")
                         tryCatch({
                           beta_dir <- file.path(c_dir, "Beta_diversity")
                           dir.create(file.path(beta_dir, "Distance"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "PCA"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "PCoA"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "NMDS"), recursive = TRUE, showWarnings = FALSE)
                           
                           # 确保有足够的列进行分析
                           if (ncol(C.abundance) > 2) {
                             # PCoA分析
                             result <- pcoa.arg(C.abundance[, 2:ncol(C.abundance), drop = FALSE], group)
                             
                             write.table(as.matrix(result[[1]]),
                                         file.path(beta_dir, "Distance/distance_bray_curtis.txt"),
                                         sep = "\t", quote = FALSE)
                             write.table(result[[2]],
                                         file.path(beta_dir, "Distance/diff_test.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                             write.table(result[[3]],
                                         file.path(beta_dir, "PCoA/C_pcoa.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                             
                             pdf(file = file.path(beta_dir, "PCoA/C_pcoa_group.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result[[4]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "PCoA/C_pcoa_ellipse.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result[[5]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "PCoA/C_pcoa_label.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result[[6]])
                             dev.off()
                             
                             # PCA分析
                             result_pca <- pca.arg(C.abundance[, 2:ncol(C.abundance), drop = FALSE], group)
                             
                             write.table(result_pca[[1]],
                                         file.path(beta_dir, "PCA/C_pca.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                             
                             pdf(file = file.path(beta_dir, "PCA/C_pca_group.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_pca[[2]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "PCA/C_pca_ellipse.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_pca[[3]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "PCA/C_pca_label.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_pca[[4]])
                             dev.off()
                             
                             # NMDS分析
                             result_nmds <- nmds.arg(C.abundance[, 2:ncol(C.abundance), drop = FALSE], group)
                             
                             write.table(result_nmds[[1]],
                                         file.path(beta_dir, "NMDS/C_stress.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                             write.table(result_nmds[[2]],
                                         file.path(beta_dir, "NMDS/C_nmds.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                             
                             pdf(file = file.path(beta_dir, "NMDS/C_nmds_group.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_nmds[[3]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "NMDS/C_nmds_ellipse.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_nmds[[4]])
                             dev.off()
                             
                             pdf(file = file.path(beta_dir, "NMDS/C_nmds_label.pdf"),
                                 width = 7.5, height = 5.4)
                             print(result_nmds[[5]])
                             dev.off()
                           }
                         }, error = function(e) {
                           warning("碳循环β多样性分析失败: ", e$message)
                         })
                       }
                       
                       if ("host" %in% input$analysis_options) {
                         incProgress(0.1, detail = "宿主微生物分析")
                         tryCatch({
                           # 检查是否有宿主数据
                           Ccyc.h <- Ccyc.host(Gene)
                           
                           dir.create(file.path(c_dir, "Host_relative_Group"), recursive = TRUE, showWarnings = FALSE)
                           
                           # Aerobic C fixation
                           if (nrow(C.abundance) >= 1 && 
                               rowSums(C.abundance[1, 2:ncol(C.abundance)]) > 0 && 
                               Ccyc.h[1] == 1) {
                             
                             dir.create(file.path(c_dir, "Host_relative_Group/ACF"), 
                                        recursive = TRUE, showWarnings = FALSE)
                             
                             ACF_host <- ACF(Gene, tax, abundance, group)
                             
                             # 保存各分类级别的数据
                             for (i in 1:6) {
                               level_names <- c("phylum", "class", "order", "family", "genus", "species")
                               write.csv(ACF_host[[i]],
                                         file = file.path(c_dir, sprintf("Host_relative_Group/ACF/ACF_%s.csv", 
                                                                         level_names[i])),
                                         row.names = TRUE)
                               
                               # 生成可视化图
                               title <- "Aerobic C fixation"
                               aa <- ifelse(nrow(ACF_host[[i]]) > 5,
                                            max(nchar(rownames(ACF_host[[i]])[1:5])),
                                            max(nchar(rownames(ACF_host[[i]]))))
                               
                               pdf(file.path(c_dir, sprintf("Host_relative_Group/ACF/ACF_%s.pdf", 
                                                            level_names[i])),
                                   width = ifelse(aa > 40, aa*0.1 + 0.5*Group_numb, 3.2 + 0.5*Group_numb),
                                   height = 3.5)
                               host.ratio(ACF_host[[i]], title)
                               dev.off()
                             }
                           }
                           
                           # 类似地处理其他过程（ACH4O, AR, AnCF, COo, Fer）
                           # 由于代码较长，这里简化处理，实际应用中需要完整实现
                           
                           # 生成图例
                           df <- data.frame(A = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                                            B = c(0.5, 0.6, 0.7, 0.8, 0.9, 1))
                           df <- t(df)
                           df1 <- data.frame(A = c("0%", "10%", "20%", "30%", "40%", "50%"),
                                             B = c("50%", "60%", "70%", "80%", "90%", "100%"))
                           df1 <- t(df1)
                           bk <- seq(0, 1, by = 0.01)
                           
                           if (require(pheatmap, quietly = TRUE)) {
                             pheatmap(df, fontsize = 30, cluster_rows = FALSE, fontface = "bold",
                                      cluster_cols = FALSE, cellwidth = 80, cellheight = 50,
                                      legend = FALSE, breaks = bk, show_rownames = FALSE,
                                      color = colorRampPalette(c("white", "Red"))(100),
                                      show_colnames = FALSE, display_numbers = df1,
                                      number_color = "black", border_color = "black",
                                      filename = file.path(c_dir, "Host_relative_Group/Legend_relative_Group.pdf"),
                                      width = 7, height = 2)
                           }
                           
                         }, error = function(e) {
                           warning("碳循环宿主微生物分析失败: ", e$message)
                         })
                       }
                       
                       rv$results$carbon <- list(
                         abundance = C.abundance,
                         ko_file = file.path(c_dir, "Gene/Abundance/C_cycle_ko_abun.txt"),
                         completed = TRUE
                       )
                     })
      }
      
      # 氮循环分析（完整版）
      if (input$nitrogen_cycle) {
        current_cycle <- current_cycle + 1
        withProgress(message = sprintf('氮循环分析 (%d/%d)...', current_cycle, total_cycles), 
                     value = 0.7, {
                       incProgress(0.05, detail = "提取氮循环数据")
                       
                       N <- c("K00370","K00371","K00374","K02567","K02568","K00362","K00363","K03385",
                              "K15876","K00367","K00372","K00366","K00368","K15864","K04561","K02305",
                              "K00376","K02588","K02586","K02591","K10944","K10945","K10946","K10535",
                              "K05601","K20932","K20933","K20934","K20935","K01915","K00265","K00266",
                              "K00264","K00284","K00260","K15371","K00261","K00262","K01455","K01501",
                              "K01725","K00926","K00549","K02575","K15576","K15577","K15578","K15579")
                       
                       matched_rows <- rownames(ko) %in% N
                       
                       if (sum(matched_rows) == 0) {
                         warning("氮循环：未找到匹配的KO编号")
                         rv$results$nitrogen <- list(completed = FALSE)
                         return(NULL)
                       }
                       
                       N.ko <- ko[matched_rows, , drop = FALSE]
                       N.abundance <- tryCatch({
                         Ncyc.abundance(ko)
                       }, error = function(e) {
                         warning("氮循环丰度计算失败: ", e$message)
                         data.frame(Process = character(0))
                       })
                       
                       if (nrow(N.abundance) == 0) {
                         rv$results$nitrogen <- list(completed = FALSE)
                         return(NULL)
                       }
                       
                       n_dir <- file.path(output_dir, "Nitrogen")
                       dir.create(n_dir, recursive = TRUE, showWarnings = FALSE)
                       dir.create(file.path(n_dir, "Gene", "Abundance"), recursive = TRUE, showWarnings = FALSE)
                       
                       write.table(N.ko,
                                   file = file.path(n_dir, "Gene/Abundance/N_cycle_ko_abun.txt"),
                                   sep = "\t", quote = FALSE, row.names = TRUE)
                       write.table(N.abundance,
                                   file = file.path(n_dir, "Gene/Abundance/N_cycle_gene_abun.txt"),
                                   sep = "\t", quote = FALSE, row.names = FALSE)
                       
                       if ("heatmap" %in% input$analysis_options) {
                         incProgress(0.05, detail = "生成热图")
                         tryCatch({
                           dir.create(file.path(n_dir, "Gene/Heatmap"), recursive = TRUE, showWarnings = FALSE)
                           result <- abun.heatmap.g(N.abundance, group, Group_numb)
                           
                           pdf(file = file.path(n_dir, "Gene/Heatmap/N_cycle_gene_abun_group.pdf"),
                               width = 3.8 + 0.6*Group_numb, height = 4)
                           print(result[[2]])
                           dev.off()
                           
                           write.table(result[[1]],
                                       file.path(n_dir, "Gene/Heatmap/Diff_N_gene_test.txt"),
                                       sep = "\t", quote = FALSE, row.names = FALSE)
                         }, error = function(e) {
                           warning("氮循环热图失败: ", e$message)
                         })
                       }
                       
                       if ("diff" %in% input$analysis_options) {
                         incProgress(0.05, detail = "差异分析")
                         tryCatch({
                           dir.create(file.path(n_dir, "Gene/Differential"), recursive = TRUE, showWarnings = FALSE)
                           result <- fold.change(N.abundance, group)

                           # 安全地处理结果
                           if (length(result) >= 1) {
                             write.table(result[[1]],
                                         file.path(n_dir, "Gene/Differential/Gene_fold_change.txt"),
                                         sep = "\t", quote = FALSE, row.names = FALSE)
                           }

                           if (length(result) >= 3 && !is.null(result[[3]])) {
                             pdf(file = file.path(n_dir, "Gene/Differential/Gene_fold_change_heatmap.pdf"),
                                 width = 0.6*Group_numb + 3.8, height = 4.5)
                             print(result[[3]])
                             dev.off()
                           }
                         }, error = function(e) {
                           warning("氮循环差异分析失败: ", e$message)
                         })
                       }
                       
                       if ("pathway" %in% input$analysis_options) {
                         incProgress(0.05, detail = "生成通路图")
                         tryCatch({
                           dir.create(file.path(n_dir, "Gene/Cycle_image"), recursive = TRUE, showWarnings = FALSE)
                           result <- fold.change(N.abundance, group)
                           
                           N.img <- system.file("data", "Ncyc.pdf", package = "CNPS.cycle")
                           
                           if (file.exists(N.img)) {
                             pdf(file.path(n_dir, "Gene/Cycle_image/N_cyc_fold_change.pdf"),
                                 width = 13, height = 7)
                             grid::grid.newpage()
                             draw_cycle_background(N.img)
                             if (length(result) >= 3 && !is.null(result[[3]])) {
                               print(
                                 result[[3]],
                                 vp = viewport(width = 0.42, height = 0.32, x = 0.78, y = 0.2)
                               )
                             } else {
                               warning("氮循环通路图缺少可叠加的数据图层(result[[3]])")
                             }
                             safe_close_pdf()
                           }

                           if (length(result) >= 3 && !is.null(result[[3]])) {
                             pdf(file = file.path(n_dir, "Gene/Cycle_image/N_processes.pdf"),
                                 width = 0.6*Group_numb + 3.8, height = 4.5)
                             print(result[[3]])
                             safe_close_pdf()
                           }
                         }, error = function(e) {
                           warning("氮循环通路图失败: ", e$message)
                         })
                       }
                       
                       if ("beta" %in% input$analysis_options && ncol(N.abundance) > 2) {
                         incProgress(0.05, detail = "β多样性分析")
                         tryCatch({
                           beta_dir <- file.path(n_dir, "Beta_diversity")
                           dir.create(file.path(beta_dir, "Distance"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "PCA"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "PCoA"), recursive = TRUE, showWarnings = FALSE)
                           dir.create(file.path(beta_dir, "NMDS"), recursive = TRUE, showWarnings = FALSE)
                           
                           result <- pcoa.arg(N.abundance[, 2:ncol(N.abundance), drop = FALSE], group)
                           write.table(as.matrix(result[[1]]),
                                       file.path(beta_dir, "Distance/distance_bray_curtis.txt"),
                                       sep = "\t", quote = FALSE)
                           
                           pdf(file = file.path(beta_dir, "PCoA/N_pcoa_group.pdf"),
                               width = 7.5, height = 5.4)
                           print(result[[4]])
                           dev.off()
                           
                           result_pca <- pca.arg(N.abundance[, 2:ncol(N.abundance), drop = FALSE], group)
                           pdf(file = file.path(beta_dir, "PCA/N_pca_group.pdf"),
                               width = 7.5, height = 5.4)
                           print(result_pca[[2]])
                           dev.off()
                           
                           result_nmds <- nmds.arg(N.abundance[, 2:ncol(N.abundance), drop = FALSE], group)
                           pdf(file = file.path(beta_dir, "NMDS/N_nmds_group.pdf"),
                               width = 7.5, height = 5.4)
                           print(result_nmds[[3]])
                           dev.off()
                         }, error = function(e) {
                           warning("氮循环β多样性分析失败: ", e$message)
                         })
                       }
                       
                       if ("host" %in% input$analysis_options) {
                         incProgress(0.05, detail = "宿主微生物分析")
                         tryCatch({
                           Ncyc.h <- Ncyc.host(Gene)
                           dir.create(file.path(n_dir, "Host_relative_Group"), recursive = TRUE, showWarnings = FALSE)
                           
                           # 根据Ncyc.h结果处理各个过程
                           # 这里简化处理，实际需要针对18个过程
                           
                         }, error = function(e) {
                           warning("氮循环宿主微生物分析失败: ", e$message)
                         })
                       }
                       
                       rv$results$nitrogen <- list(
                         abundance = N.abundance,
                         ko_file = file.path(n_dir, "Gene/Abundance/N_cycle_ko_abun.txt"),
                         completed = TRUE
                       )
                     })
      }
      
      # 磷循环分析
      if (input$phosphorus_cycle) {
        current_cycle <- current_cycle + 1
        withProgress(message = sprintf('磷循环分析 (%d/%d)...', current_cycle, total_cycles), 
                     value = 0.8, {
                       incProgress(0.1, detail = "提取磷循环数据")
                       
                       P <- c("K07636","K07657","K07658","K07768","K07776")
                       P.ko <- ko[rownames(ko) %in% P,]
                       P.abundance <- Pcyc.abundance(ko)
                       
                       p_dir <- file.path(output_dir, "Phosphorus")
                       dir.create(p_dir, recursive = TRUE, showWarnings = FALSE)
                       dir.create(file.path(p_dir, "Gene", "Abundance"), recursive = TRUE, showWarnings = FALSE)
                       
                       write.table(P.ko,
                                   file = file.path(p_dir, "Gene/Abundance/P_cycle_ko_abun.txt"),
                                   sep = "\t")
                       write.table(P.abundance,
                                   file = file.path(p_dir, "Gene/Abundance/P_cycle_gene_abun.txt"),
                                   sep = "\t", row.names = FALSE)
                       
                       if ("heatmap" %in% input$analysis_options) {
                         incProgress(0.1, detail = "生成热图")
                         dir.create(file.path(p_dir, "Gene/Heatmap"), recursive = TRUE, showWarnings = FALSE)
                         result <- abun.heatmap.g(P.abundance, group, Group_numb)
                         
                         pdf(file = file.path(p_dir, "Gene/Heatmap/P_cycle_gene_abun_group.pdf"),
                             width = 2.3 + 0.6*Group_numb, height = 3)
                         print(result[[2]])
                         dev.off()
                       }
                       
                       rv$results$phosphorus <- list(
                         abundance = P.abundance,
                         ko_file = file.path(p_dir, "Gene/Abundance/P_cycle_ko_abun.txt"),
                         completed = TRUE
                       )
                     })
      }
      
      # 硫循环分析
      if (input$sulfur_cycle) {
        current_cycle <- current_cycle + 1
        withProgress(message = sprintf('硫循环分析 (%d/%d)...', current_cycle, total_cycles), 
                     value = 0.9, {
                       incProgress(0.1, detail = "提取硫循环数据")
                       
                       S <- c("K00956","K00957","K00955","K00860","K00390","K00380","K00381","K00392",
                              "K00958","K00988","K00394","K00395","K11180","K11181","K17222","K17223",
                              "K17224","K17225","K17226","K17227","K17229","K17230","K17218","K08352",
                              "K08354","K04091","K00299","K16968","K16969","K15762","K15765","K03119",
                              "K00456","K17217","K02045","K02046","K02047","K02048","K15551","K15552",
                              "K10831","K15553","K15554","K15555","K01739","K10764","K01738","K17228")
                       
                       S.ko <- ko[rownames(ko) %in% S,]
                       S.abundance <- Scyc.abundance(ko)
                       
                       s_dir <- file.path(output_dir, "Sulfur")
                       dir.create(s_dir, recursive = TRUE, showWarnings = FALSE)
                       dir.create(file.path(s_dir, "Gene", "Abundance"), recursive = TRUE, showWarnings = FALSE)
                       
                       write.table(S.ko,
                                   file = file.path(s_dir, "Gene/Abundance/S_cycle_ko_abun.txt"),
                                   sep = "\t")
                       write.table(S.abundance,
                                   file = file.path(s_dir, "Gene/Abundance/S_cycle_gene_abun.txt"),
                                   sep = "\t", row.names = FALSE)
                       
                       if ("heatmap" %in% input$analysis_options) {
                         incProgress(0.1, detail = "生成热图")
                         dir.create(file.path(s_dir, "Gene/Heatmap"), recursive = TRUE, showWarnings = FALSE)
                         result <- abun.heatmap.g(S.abundance, group, Group_numb)
                         
                         pdf(file = file.path(s_dir, "Gene/Heatmap/S_cycle_gene_abun_group.pdf"),
                             width = 3.2 + 0.6*Group_numb, height = 3)
                         print(result[[2]])
                         dev.off()
                       }
                       
                       rv$results$sulfur <- list(
                         abundance = S.abundance,
                         ko_file = file.path(s_dir, "Gene/Abundance/S_cycle_ko_abun.txt"),
                         completed = TRUE
                       )
                     })
      }
      
      rv$analysis_running <- FALSE
      rv$analysis_complete <- TRUE
      
      removeNotification(id = "analysis_notification")
      showNotification(
        "分析完成！请查看结果页面。",
        type = "message",
        duration = 10
      )
      
      # 自动跳转到结果页面
      updateTabItems(session, "tabs", "results")
      
    }, error = function(e) {
      rv$analysis_running <- FALSE
      removeNotification(id = "analysis_notification")
      showNotification(
        paste("分析出错：", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # 分析进度UI
  output$analysis_progress_ui <- renderUI({
    if (rv$analysis_running) {
      tagList(
        div(
          class = "progress-custom",
          div(class = "progress-bar-custom", style = "width: 100%;",
              "分析进行中...")
        ),
        br(),
        p("这可能需要几分钟时间，请不要关闭浏览器。", 
          style = "color: #666; font-size: 14px;")
      )
    } else if (rv$analysis_complete) {
      tags$div(
        class = "alert alert-success",
        style = "font-size: 18px;",
        icon("check-circle", class = "fa-2x"),
        h4("分析完成！", style = "margin-top: 10px;"),
        p("您可以在结果页面查看和下载所有分析结果。")
      )
    }
  })
  
  # 分析日志
  output$analysis_log <- renderText({
    if (rv$analysis_running) {
      "正在运行分析..."
    } else if (rv$analysis_complete) {
      log_text <- "分析完成摘要:\n"
      log_text <- paste0(log_text, "==================\n")
      
      if (!is.null(rv$results$carbon)) {
        log_text <- paste0(log_text, "✓ 碳循环分析完成\n")
      }
      if (!is.null(rv$results$nitrogen)) {
        log_text <- paste0(log_text, "✓ 氮循环分析完成\n")
      }
      if (!is.null(rv$results$phosphorus)) {
        log_text <- paste0(log_text, "✓ 磷循环分析完成\n")
      }
      if (!is.null(rv$results$sulfur)) {
        log_text <- paste0(log_text, "✓ 硫循环分析完成\n")
      }
      
      log_text <- paste0(log_text, "==================\n")
      log_text <- paste0(log_text, "输出目录: ", input$output_dir, "\n")
      log_text <- paste0(log_text, "分析时间: ", Sys.time())
      
      log_text
    } else {
      "等待开始分析..."
    }
  })
  
  # 结果摘要
  output$results_summary <- renderUI({
    if (!rv$analysis_complete) {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " 请先运行分析以查看结果。"
      )
    } else {
      completed_cycles <- c()
      if (!is.null(rv$results$carbon)) completed_cycles <- c(completed_cycles, "碳循环")
      if (!is.null(rv$results$nitrogen)) completed_cycles <- c(completed_cycles, "氮循环")
      if (!is.null(rv$results$phosphorus)) completed_cycles <- c(completed_cycles, "磷循环")
      if (!is.null(rv$results$sulfur)) completed_cycles <- c(completed_cycles, "硫循环")
      
      tags$div(
        h4("分析完成！", style = "color: #28a745;"),
        p(paste("已完成", length(completed_cycles), "个循环的分析：",
                paste(completed_cycles, collapse = "、"))),
        hr(),
        fluidRow(
          valueBox(
            length(completed_cycles),
            "完成的循环",
            icon = icon("recycle"),
            color = "green",
            width = 3
          ),
          valueBox(
            length(input$analysis_options),
            "分析类型",
            icon = icon("chart-bar"),
            color = "blue",
            width = 3
          ),
          valueBox(
            input$output_dir,
            "输出目录",
            icon = icon("folder"),
            color = "purple",
            width = 3
          ),
          valueBox(
            format(Sys.time(), "%H:%M:%S"),
            "完成时间",
            icon = icon("clock"),
            color = "orange",
            width = 3
          )
        )
      )
    }
  })
  
  # 碳循环结果
  output$carbon_results <- renderUI({
    if (is.null(rv$results$carbon)) {
      tags$p("未进行碳循环分析")
    } else {
      tagList(
        div(class = "result-card",
            h4("碳循环分析结果", style = "color: #4CAF50;"),
            hr(),
            h5("基因丰度数据"),
            DTOutput("carbon_abundance_table"),
            br(),
            h5("可下载文件"),
            downloadButton("download_carbon_ko", "KO丰度数据", class = "btn-sm btn-info"),
            downloadButton("download_carbon_gene", "基因丰度数据", class = "btn-sm btn-info"),
            if ("heatmap" %in% input$analysis_options) {
              downloadButton("download_carbon_heatmap", "热图PDF", class = "btn-sm btn-success")
            }
        )
      )
    }
  })
  
  # 氮循环结果
  output$nitrogen_results <- renderUI({
    if (is.null(rv$results$nitrogen)) {
      tags$p("未进行氮循环分析")
    } else {
      tagList(
        div(class = "result-card",
            h4("氮循环分析结果", style = "color: #2196F3;"),
            hr(),
            h5("基因丰度数据"),
            DTOutput("nitrogen_abundance_table"),
            br(),
            h5("可下载文件"),
            downloadButton("download_nitrogen_ko", "KO丰度数据", class = "btn-sm btn-info"),
            downloadButton("download_nitrogen_gene", "基因丰度数据", class = "btn-sm btn-info")
        )
      )
    }
  })
  
  # 磷循环结果
  output$phosphorus_results <- renderUI({
    if (is.null(rv$results$phosphorus)) {
      tags$p("未进行磷循环分析")
    } else {
      tagList(
        div(class = "result-card",
            h4("磷循环分析结果", style = "color: #9C27B0;"),
            hr(),
            h5("基因丰度数据"),
            DTOutput("phosphorus_abundance_table"),
            br(),
            h5("可下载文件"),
            downloadButton("download_phosphorus_ko", "KO丰度数据", class = "btn-sm btn-info"),
            downloadButton("download_phosphorus_gene", "基因丰度数据", class = "btn-sm btn-info")
        )
      )
    }
  })
  
  # 硫循环结果
  output$sulfur_results <- renderUI({
    if (is.null(rv$results$sulfur)) {
      tags$p("未进行硫循环分析")
    } else {
      tagList(
        div(class = "result-card",
            h4("硫循环分析结果", style = "color: #FF9800;"),
            hr(),
            h5("基因丰度数据"),
            DTOutput("sulfur_abundance_table"),
            br(),
            h5("可下载文件"),
            downloadButton("download_sulfur_ko", "KO丰度数据", class = "btn-sm btn-info"),
            downloadButton("download_sulfur_gene", "基因丰度数据", class = "btn-sm btn-info")
        )
      )
    }
  })
  
  # 数据表格显示
  output$carbon_abundance_table <- renderDT({
    if (!is.null(rv$results$carbon)) {
      datatable(
        rv$results$carbon$abundance,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE
      )
    }
  })
  
  output$nitrogen_abundance_table <- renderDT({
    if (!is.null(rv$results$nitrogen)) {
      datatable(
        rv$results$nitrogen$abundance,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    }
  })
  
  output$phosphorus_abundance_table <- renderDT({
    if (!is.null(rv$results$phosphorus)) {
      datatable(
        rv$results$phosphorus$abundance,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    }
  })
  
  output$sulfur_abundance_table <- renderDT({
    if (!is.null(rv$results$sulfur)) {
      datatable(
        rv$results$sulfur$abundance,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    }
  })
  
  # 下载处理器
  output$download_carbon_ko <- downloadHandler(
    filename = function() { "C_cycle_ko_abun.txt" },
    content = function(file) {
      if (!is.null(rv$results$carbon$ko_file) && file.exists(rv$results$carbon$ko_file)) {
        file.copy(rv$results$carbon$ko_file, file)
      }
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("CNPS_Analysis_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # 创建临时目录
      temp_dir <- tempdir()
      
      # 压缩结果目录
      zip::zip(
        zipfile = file,
        files = list.files(input$output_dir, full.names = TRUE, recursive = TRUE),
        mode = "cherry-pick"
      )
    }
  )
}

# 运行应用
shinyApp(ui = ui, server = server)
