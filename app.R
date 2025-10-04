library(shiny)
library(shinythemes)
library(phyloseq)
library(microbiomeMarker)  # caporaso 数据集所在包
library(microeco)
library(file2meco)
library(metagMisc)
library(tibble)

options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Welcome to Phylo2Tab!"),
  
  tabsetPanel(
    tabPanel("upload dataset",
             sidebarLayout(
               sidebarPanel(
                 h4("Loading rawdata"),
                 fileInput("upload", NULL, buttonLabel = "Upload...",
                           multiple = FALSE, accept = c(".rds")),
                 hr(),
                 h4("Or select an example dataset"),
                 selectInput("example", "Example dataset:",
                             choices = c("None", "caporaso"),
                             selected = "None")
               ),
               mainPanel(
                 h4("File information / Example dataset"),
                 tableOutput("files1")
               )
             )
    ),
    
    tabPanel("Shiny-Phyloseq",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_ShinyPhyloseq", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files2"),
                 downloadButton("download2", "Download processed RData")
               )
             )
    ),
    tabPanel("animalcules & Namco & MicrobiomeAnalyst 2.0",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_animalcules", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files3"),
                 downloadButton("download3", "Download transfered table")
               )
             )
    ),
    
    tabPanel("wiSDOM",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_wiSDOM", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files4"),
                 downloadButton("download4", "Download transfered table")
               )
             )
    ),
    
    tabPanel("Mian",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_Mian", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files5"),
                 downloadButton("download5", "Download transfered table")
               )
             )
    ),
    
    # tabPanel("MiCloud",
    #          sidebarLayout(
    #            sidebarPanel(
    #              actionButton("run_MiCloud", "Run")
    #            ),
    #            mainPanel(
    #              h4("Table Preview"),
    #              tableOutput("files6"),
    #              downloadButton("download6", "Download transfered table")
    #            )
    #          )
    # ),
    
    tabPanel("Namco",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_Namco", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files7"),
                 downloadButton("download7", "Download transfered table")
               )
             )
    ),
    
    # tabPanel("MicrobiomeAnalyst 2.0",
    #          sidebarLayout(
    #            sidebarPanel(
    #              actionButton("run_MicrobiomeAnalyst", "Run")
    #            ),
    #            mainPanel(
    #              h4("Table Preview"),
    #              tableOutput("files8"),
    #              downloadButton("download8", "Download transfered table")
    #            )
    #          )
    # ),
    # 
    # tabPanel("MiPair",
    #          sidebarLayout(
    #            sidebarPanel(
    #              actionButton("run_MiPair", "Run")
    #            ),
    #            mainPanel(
    #              h4("Table Preview"),
    #              tableOutput("files9"),
    #              downloadButton("download9", "Download transfered table")
    #            )
    #          )
    # ),
    # 
    # tabPanel("misurv",
    #          sidebarLayout(
    #            sidebarPanel(
    #              actionButton("run_misurv", "Run")
    #            ),
    #            mainPanel(
    #              h4("Table Preview"),
    #              tableOutput("files10"),
    #              downloadButton("download10", "Download transfered table")
    #            )
    #          )
    # ),
    
    tabPanel("ampvis2",
             sidebarLayout(
               sidebarPanel(
                 actionButton("run_ampvis2", "Run")
               ),
               mainPanel(
                 h4("Table Preview"),
                 tableOutput("files11"),
                 downloadButton("download11", "Download transfered table")
               )
             )
    ),
  )
)

server <- function(input, output, session) {
  
  # 数据源：优先使用上传的文件，其次是选择的示例 caporaso
  uploadedData <- reactive({
    if (!is.null(input$upload)) {
      # 上传文件
      readRDS(input$upload$datapath)
    } else if (input$example == "caporaso") {
      # 示例数据 caporaso
      data("caporaso", package = "microbiomeMarker")
      ps_phy <- caporaso
      ps_phy
    } else {
      NULL
    }
  })
  
  # 文件或示例数据的信息展示
  output$files1 <- renderTable({
    if (!is.null(input$upload)) {
      input$upload
    } else if (input$example == "caporaso") {
      ps <- uploadedData()
      if (!is.null(ps)) {
        data.frame(
          Samples = nsamples(ps),
          OTUs = ntaxa(ps),
          Sample_variables = length(sample_variables(ps))
        )
      }
    }
  })
  
  # 点击 run_ShinyPhyloseq 后生成处理结果
  filteredData2 <- eventReactive(input$run_ShinyPhyloseq, {
    ps <- uploadedData()
    req(ps)
    ps
  })
  
  # 显示 sample_data 表
  output$files2 <- renderTable({
    req(filteredData2())
    as.data.frame(sample_data(filteredData2()))
  })
  
  # 下载处理后的 RData
  output$download2 <- downloadHandler(
    filename = function() { "ps_phy.RData" },
    content = function(file) {
      ps <- filteredData2()
      save(ps, file = file)
    }
  )
  
  # 点击 run_animalcules 后生成处理结果
  filteredData3 <- eventReactive(input$run_ShinyPhyloseq, {
    ps <- uploadedData()
    req(ps)
    ps
  })
  
  output$files3 <- renderTable({
    req(filteredData3())
    meco<-phyloseq2meco(filteredData3())
    print(class(meco))
  })
  
  output$download3 <- downloadHandler(
    filename = function() {
      paste0("ps_phy_tables_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # 创建一个临时目录（自己指定，避免 tempdir() 不一致）
      tmpdir <- file.path(tempdir(), "meco_tables")
      if (!dir.exists(tmpdir)) dir.create(tmpdir)
      
      # 转换并保存
      meco <- phyloseq2meco(filteredData3())
      meco$save_table(dirpath = tmpdir, sep = "\t", quote = FALSE)
      
      # 列出该目录下的所有文件，不要只限定 .txt
      files <- list.files(tmpdir, full.names = TRUE)
      message("生成的文件：", paste(files, collapse = ", "))
      
      # 打包成 zip
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"
  )
  
  # 点击 run_wiSDOM 后生成处理结果
  filteredData4 <- eventReactive(input$run_wiSDOM, {
    ps <- uploadedData()
    req(ps)
    ps
  })
  
  # 显示 sample_data 表
  output$files4 <- renderTable({
    req(filteredData4())
    as.data.frame(sample_data(filteredData4()))
  })
  
  output$download4 <- downloadHandler(
    filename = function() {
      paste0("wiSDOM_tables_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # 创建一个临时目录（自己指定，避免 tempdir() 不一致）
      tmpdir <- file.path(tempdir(), "wiSDOM_tables")
      if (!dir.exists(tmpdir)) dir.create(tmpdir)
      
      otu_tax_table <- phyloseq_to_df(ps, addtax = TRUE)
      prefixes <- paste0("D_", 0:6, "__")
      cols_to_modify <- 2:8
      
      # exporting otutable and metadata
      # exporting otutable
      otu_tax_table[cols_to_modify] <- Map(function(col, prefix) paste0(prefix, col),
                                           otu_tax_table[cols_to_modify], prefixes)
      otu_tax_table$ID <- apply(otu_tax_table[, 2:8], 1, function(x) paste(x, collapse = ";"))
      otu_tax_table$Taxonomy <- apply(otu_tax_table[, c(ncol(otu_tax_table),1)], 1, function(x) paste(x, collapse = " "))
      otu_tax_table <- otu_tax_table[, c(ncol(otu_tax_table),9:(ncol(otu_tax_table)-2))]
      
      # exporting metadata (only keeping one column)
      metadata<-as.data.frame(as.matrix(sample_data(ps)));metadata
      class(metadata)
      library(tibble)
      metadata<-rownames_to_column(metadata,var = "ID");metadata
      metadata<- metadata[,1:2] #Based on the research objectives, keep one column (variable)
      
      # 转换并保存
      write.table(otu_tax_table,file = file.path(tmpdir, "otu_tax_tab.txt"),quote = F,sep='\t',row.names = F,col.names = T)
      write.table(metadata,file = file.path(tmpdir, "metadata.txt"),quote = F,sep='\t',row.names = F,col.names = F)
      
      # 列出该目录下的所有文件，不要只限定 .txt
      files <- list.files(tmpdir, full.names = TRUE)
      message("生成的文件：", paste(files, collapse = ", "))
      
      # 打包成 zip
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"
  )
  
  # 点击 run_Mian 后生成处理结果
  filteredData5 <- eventReactive(input$run_Mian, {
    ps <- uploadedData()
    req(ps)
    ps
  })

  # 显示 sample_data 表
  output$files5 <- renderTable({
    req(filteredData5())
    as.data.frame(sample_data(filteredData5()))
  })

  output$download5 <- downloadHandler(
    filename = function() {
      paste0("Mian_tables_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # 创建一个临时目录（自己指定，避免 tempdir() 不一致）
      tmpdir <- file.path(tempdir(), "Mian_tables")
      if (!dir.exists(tmpdir)) dir.create(tmpdir)

      ps <- filteredData5()
      otu<-t(as.data.frame((otu_table(ps))))
      otu<-as.data.frame(otu)
      otu<-rownames_to_column(otu,var = "Sample Labels")

      # exporting taxtab
      tax<-as.data.frame(tax_table(ps))

      # exporting metadata
      metadata<-as.data.frame(as.matrix(sample_data(ps)))
      metadata<-rownames_to_column(metadata,var = "SampleID");metadata

      # 转换并保存
      write.table(otu,file = file.path(tmpdir, "otu_tab.txt"),quote = F,sep='\t',row.names = F,col.names = T)
      # adding "OTU	Taxonomy" in first row of file named "taxtab.txt"
      write.table(tax,file = file.path(tmpdir, "taxtab.txt"),quote = F,sep='\t',row.names = T,col.names = F)
      write.table(metadata,file = file.path(tmpdir, "metadata.txt"),quote = F,sep='\t',row.names = F,col.names = T)

      # 列出该目录下的所有文件，不要只限定 .txt
      files <- list.files(tmpdir, full.names = TRUE)
      message("生成的文件：", paste(files, collapse = ", "))

      # 打包成 zip
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"
  )
  
  # 点击 run_ampvis2 后生成处理结果
  filteredData11 <- eventReactive(input$run_ampvis2, {
    ps <- uploadedData()
    req(ps)
    ps
  })
  
  # 显示 sample_data 表
  output$files11 <- renderTable({
    req(filteredData11())
    as.data.frame(sample_data(filteredData11()))
  })

  output$download11 <- downloadHandler(
    filename = function() {
      paste0("ampvis2_tables_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # 创建一个临时目录（自己指定，避免 tempdir() 不一致）
      tmpdir <- file.path(tempdir(), "ampvis2_tables")
      if (!dir.exists(tmpdir)) dir.create(tmpdir)
      
      # ps <- filteredData11()
      otu_tax_table <- phyloseq_to_df(ps, addtax = TRUE)
      class(otu_tax_table)
      otu_tax_table <- otu_tax_table[, c(1, 9:ncol(otu_tax_table), 2:8)]
      
      # exporting metadata
      metadata<-as.data.frame(as.matrix(sample_data(ps)))
      class(metadata)
      library(tibble)
      metadata<-rownames_to_column(metadata,var = "SampleID")
      
      # 转换并保存
      write.table(otu_tax_table,file = file.path(tmpdir, "otu_tab.txt"),quote = F,sep=';',row.names = F,col.names = T)
      write.table(metadata,file = file.path(tmpdir, "metadata.txt"),quote = F,sep='\t',row.names = F,col.names = T)
     
      # 列出该目录下的所有文件，不要只限定 .txt
      files <- list.files(tmpdir, full.names = TRUE)
      message("生成的文件：", paste(files, collapse = ", "))
      
      # 打包成 zip
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"
  )
  
  # insert
}

shinyApp(ui, server)

