# InDoseApp UI
library(shiny)
refTitle <- '"ICRP Database of Dose Coefficients: Workers and Members of the Public; Ver. 3.0"'
refURL <- "http://www.icrp.org/page.asp?id=145"

shinyUI(bootstrapPage(
  # アプリタイトル
  headerPanel("ICRP内部被曝関連データ"),
  # 条件選択サイドバー
  sidebarPanel(
    h4("摂取条件"),
    selectInput("nuclide", "核種",
                choices = c("131I", "134Cs", "137Cs"),
                selected = "137Cs",
                multiple = TRUE),
    selectInput("age", "年齢",
                choices = c("成人" = "Adult", "15歳" = "15y", "10歳" = "10y",
                            "5歳" = "5y", "1歳" = "1y", "3ヶ月" = "3mo"),
                selected = "成人",
                multiple = TRUE),
    selectInput("worker", "公衆/作業者",
                choices = c("公衆" = "Public", "作業者" = "Worker"),
                selected = "公衆",
                multiple = TRUE),
    selectInput("intake", "経口摂取/吸入摂取",
                choices = c("経口" = "Ing", "吸入" = "Inh"),
                selected = "経口",
                multiple = TRUE),
    selectInput("type", "化学形",
                choices = c(
                  "エアロゾル 吸収タイプF" = "Aerosol.Type.F",
                  "エアロゾル 吸収タイプM" = "Aerosol.Type.M",
                  "エアロゾル 吸収タイプS" = "Aerosol.Type.S",
                  "単体ヨウ素蒸気" = "Elemental.Iodine.Vapour",
                  "ヨウ化メチル" = "Methyl.Iodide"),
                selected = "エアロゾル 吸収タイプF",
                multiple = TRUE),
    selectInput("size", "エアロゾル粒径",
                choices = c(
                  "0.001 μm" = 0.001, "0.003 μm" = 0.003,
                  "0.01 μm" = 0.01, "0.03 μm" = 0.03,
                  "0.1 μm" = 0.1, "0.3 μm" = 0.3,
                  "1 μm" = 1, "3 μm" = 3, "5 μm" = 5, "10 μm" = 10
                ),
                selected = "1 μm",
                multiple = TRUE),
    selectInput("organ", "臓器",
                choices = c("全身" = "Whole.Body", "甲状腺" = "Thyroid"),
                selected = "全身",
                multiple = TRUE),
    radioButtons("lang", "言語", choices = c("英語"="en", "日本語"="jp"),
                 selected = "英語"),
    helpText("Windows使用の場合, データの日本語表示は文字化けする"),
    submitButton("更新する")
  ),
  
  mainPanel(
    tabsetPanel(
      # 全身残留割合のグラフを表示
      tabPanel(
        title = "残留割合",
        h4("残留割合"),
        plotOutput("plot"),
        div(
          class = "row",
          div(
            class = "span4",
            h4("横軸"),
            numericInput("xmin", "最小値", value = 0),
            numericInput("xmax", "最大値", value = 1000),
            checkboxInput("xlog", "対数軸", FALSE)
          ),
          div(
            class = "span4",
            h4("縦軸"),
            numericInput("ymin", "最小値", value = 0),
            numericInput("ymax", "最大値", value = 1),
            checkboxInput("ylog", "対数軸", FALSE)
          )
        )
      ),
      # 移行係数の表を表示
      tabPanel(
        title="移行係数",
        h4("体内動態モデルの移行係数"),
        tableOutput("tc")
      ),
      # 初期値のリストを表示
      tabPanel(
        title = "初期値",
        h4("体内動態モデルの初期値の0でない成分"),
        verbatimTextOutput("iv")
      )
    ),
    h4("摂取条件と実効または等価線量係数"),
    tableOutput("dc"),
    tags$footer("参考:", tags$em(refTitle), tags$a(href=refURL, refURL))
  )
  
))