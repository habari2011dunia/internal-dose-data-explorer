# InDoseApp Server
library(shiny)
library(Matrix)
library(ggplot2)
source("functions.R")

shinyServer(function(input, output) {
  # 摂取条件
  # inputに対してすべての可能な組み合わせのデータフレームを返す
  ReactiveCondition <- reactive({
    worker <- input$worker == "Worker"
    
    table <- rbind(
      expand.grid( # 経口摂取部分
        nuclide = input$nuclide,
        age = input$age,
        worker = worker,
        intake = grep("^Ing$", input$intake, value = TRUE),
        type = NA,
        size = NA,
        organ = input$organ,
        stringsAsFactors = FALSE
      ),
      expand.grid( # エアロゾル吸入摂取部分
        nuclide = input$nuclide,
        age = input$age,
        worker = worker,
        intake = grep("^Inh$", input$intake, value = TRUE),
        type = grep("Aerosol", input$type, value = TRUE),
        size = input$size,
        organ = input$organ,
        stringsAsFactors = FALSE
      ),
      expand.grid( # エアロゾル以外のヨウ素の吸入摂取部分
        nuclide = grep("I$", input$nuclide, value = TRUE),
        age = input$age,
        worker = worker,
        intake = grep("^Inh$", input$intake, value = TRUE),
        type = grep("Iodi[dn]e", input$type, value = TRUE),
        size = NA,
        organ = input$organ,
        stringsAsFactors = FALSE
      )
    )
    
    table <- subset(table, !(age != "Adult" & worker)) # 作業者かつ子供を除く
    table <- subset( # 作業者のエアロゾル吸入はタイプFのみ
      table,
      !(type %in% c("Aerosol.Type.M", "Aerosol.Type.S") & worker)
    )
    
    # functions.Rの関数に代入するため核種を質量数と元素名に分ける
    table <- cbind(
      mass.number = as.numeric(gsub("[[:alpha:]]", "", table$nuclide)),
      element = gsub("[[:digit:]]", "", table$nuclide),
      table,
      stringsAsFactors = FALSE
    )
    
    table <- subset(table, !(element != "I" & organ == "Thyroid")) # 甲状腺はヨウ素のみ
    
    unique(table) # 重複を除く
  }) # ReactiveConditionの定義ここまで
  
  # パラメータを条件ごとにまとめたリストのリスト
  ReactiveParameters <- reactive({
    cond <- ReactiveCondition()
    parameters <- list()
    for (i in seq(1, nrow(cond))) {
      parameters[[i]] <- ModelParameters(
        mass.number = cond$mass.number[i],
        element = cond$element[i],
        age = cond$age[i],
        intake = cond$intake[i],
        type = cond$type[i],
        size = cond$size[i],
        worker = cond$worker[i]
      )
    }
    parameters
  })
  
  # ReactiveCondition()の各行の条件に対する預託実効線量係数
  ReactiveDoseCoefficient <- reactive({
    cond <- ReactiveCondition()
    mapply(
      DoseCoefficient,
      cond$mass.number, cond$element, cond$age, cond$intake,
      cond$type, cond$size, cond$worker, cond$organ
    )
  })
  
  # 移行係数表データフレームのリスト
  ReactiveTransferCoefficient <- reactive({
    paramList <- ReactiveParameters()
    x <- list()
    j <- c("from", "to", "transfer.coefficient")
    for (i in seq(along=paramList)) {
      x[[i]] <- paramList[[i]][["transfer.coefficient"]][, j]
    }
    x
  })
  
  # 初期値データフレームのリスト
  ReactiveInitialValue <- reactive({
    paramList <- ReactiveParameters()
    x <- list()
    j <- c("compartment", "initial.value")
    for (i in seq(along=paramList)) {
      x[[i]] <- paramList[[i]][["initial.value"]][, j]
    }
    x
  })
  
  # 崩壊定数のベクトル
  ReactiveDecayConstant <- reactive({
    paramList <- ReactiveParameters()
    x <- NULL
    for (i in seq(along=paramList)) {
      x <- c(x, paramList[[i]][["decay.constant"]])
    }
    x
  })
  
  # 全身残留割合の時間関数
  ReactiveRetentionFunction <- reactive({
    param <- ReactiveParameters()
    organ <- ReactiveCondition()$organ
    mapply(RetentionFunction, param, organ)
  })
  
  # 条件と換算係数の表のアウトプット
  output$dc <- renderTable({
    # 条件と換算係数
    table <- ReactiveCondition()
    table <- table[, setdiff(colnames(table), c("mass.number", "element"))]
    table <- cbind(table, as.character(ReactiveDoseCoefficient()))
    table <- cbind(seq(1, nrow(table)), table)
    # 表示用に整形
    table <- transform(
      table,
      worker = ifelse(worker, "Worker", "Public"),
      intake = gsub("Ing", "Ingestion", gsub("Inh", "Inhalation", intake)),
      type = gsub("\\.", " ", type)
    )
    # 日本語化(Winでは文字化け)
    if(input$lang == "jp") {
      table <- transform(
        table,
        age = gsub("mo", "ヶ月", gsub("y", "才", gsub("Adult", "成人", age))),
        worker = gsub("Worker", "作業者", gsub("Public", "公衆", worker)),
        intake = gsub("Inhalation", "吸入", gsub("Ingestion", "経口", intake)),
        type = gsub("Methyl Iodide", "ヨウ化メチル",
                    gsub("Elemental Iodine Vapour", "単体ヨウ素蒸気",
                         gsub("Aerosol Type ", "エアロゾル 吸収タイプ", type))),
        organ = gsub("Thyroid", "甲状腺", gsub("Whole.Body", "全身", organ))
      )
    }
    colnames(table) <- c(
      "No.", "Nuclide", "Age", "Public/Worker", "Ing/Inh",
      "Chemical Form", "Aerosol Size(um)", "Organ", "Dose Coef.(Sv/Bq)"
    )
    if(input$lang == "jp") {
      colnames(table) <- c(
        "番号", "核種", "年齢", "公衆/作業者", "経口/吸入",
        "化学形", "エアロゾル粒径(μm)", "臓器", "線量係数(Sv/Bq)"
      )
    }
    
    table 
  }, include.rownames = FALSE)
  
  # 移行係数表の表示
  output$tc <- renderTable({
    tc <- ReactiveTransferCoefficient()
    table <- NULL
    for(i in seq(along=tc)) {
      table <- rbind(table, cbind(cond = i, tc[[i]]))
    }
    
    if (input$lang == "jp")
      colnames(table) <-  c("条件", "移行元", "移行先", "移行係数(1/日)")
    else if (input$lang == "en")
      colnames(table) <- c("cond.", "from", "to", "trans. coef.(1/d)")
    
    table
  }, include.rownames = FALSE)
  
  # 初期値のアウトプット
  output$iv <- renderPrint({
    lapply(
      ReactiveInitialValue(),
      function(x) {
        v <- x$initial.value
        names(v) <- x$compartment
        v
      }
    )
  })
  
  # グラフのプロット
  output$plot <- renderPlot({
    functionsList <- ReactiveRetentionFunction()
    if (input$xlog) # もし横軸が対数表示なら, 時間の対数をとってから等分する
      time <- exp(seq(log(input$xmin), log(input$xmax), length = 101))
    else
      time <- seq(input$xmin, input$xmax, length = 101)
    
    d <- NULL
    for (i in seq(along = functionsList)) {
      f <- functionsList[[i]]
      d <- rbind(d, data.frame(time = time, condition = i, value = f(time)))
    }
    d <- transform(d, condition = as.factor(condition))
    p <- ggplot(d, aes(x = time, y = value, color = condition)) + geom_line()
    if (input$xlog)
      p <- p + scale_x_log10(limits = c(input$xmin, input$xmax))
    else
      p <- p + scale_x_continuous(limits = c(input$xmin, input$xmax))
    if (input$ylog)
      p <- p + scale_y_log10(limits = c(input$ymin, input$ymax))
    else
      p <- p + scale_y_continuous(limits = c(input$ymin, input$ymax))
    if(input$lang == "jp")
      p <- p + labs(x = "経過時間(日)", y = "残留割合")
    else if (input$lang == "en")
      p <- p + labs(x = "Time after intake(days)", y = "Retention Ratio")
    plot(p)
  })
})