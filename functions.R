# 放射性核種のコンパートメントモデル解析のための関数
# 作者: @habari2011dunia (twitter)
# E-mail:  habari2011dunia@gmail.com
# 最終更新: 2013年6月30日

#### データ ####
DecayConstant <- function(mass.number, element, decay.file = "decay_data.csv") {
  # 崩壊定数(1/日)
  dd <- read.csv(decay.file)
  dd$decay.constant[dd$mass.number == mass.number & dd$element == element]
}

# 条件に合う移行係数表をファイルから選び出す関数
TransferCoefficient <- function (
  element, age = "Adult", intake = "Ing", type = NA,
  model = "ICRP68_72", tc.file = "transfer_coefficient.csv"
) {
  select.element <- c(element, "All") # 元素
  select.age <- c(sub("Adult", "20y", age), "All") # 年齢
  select.intake <- c(intake, "All") # 経口/吸入
  select.type <- c(type, "All") # タイプ
  
  if (model == "ICRP68_72") {
    select.model <- c("HRTM", "Biokinetic", "GITM", "UBM")
    select.proposed.by <-
      c("ICRP.publ.67", "ICRP.publ.66", "ICRP.publ.56", "ICRP.publ.30")
    if (element == "I" && intake == "Inh" && type == "Methyl.Iodide")
      intake <- "Ing" # ヨウ化メチルの動態は経口摂取と同じ
    select.intake <- c(intake, "All")
    if (element == "I" && intake == "Inh" && type == "Elemental.Iodine.Vapour")
      type <- "Aerosol.Type.F" # 単体ヨウ素の動態はタイプFエアロゾルと同じ
    select.type <- c(type, "All")
    
    tc <- subset(
      read.csv(tc.file, stringsAsFactors = FALSE),
      model %in% select.model &
        proposed.by %in% select.proposed.by &
        element %in% select.element &
        age %in% select.age &
        intake %in% select.intake &
        type %in% select.type
    )
    # 呼吸気道モデルと動態モデルの結合のため, 名前を統一
    tc$to <- sub("^GI.Tract$", "Stomach", tc$to)
  } else if (model == "Leggett2003") {
    # モデル
    select.model <- c("HRTM", "Biokinetic")
    select.proposed.by <- c("ICRP.publ.66", "Leggett")
    select.year <- c(1994, 2003)
    # データ選択
    tc <- subset(
      read.csv(tc.file, stringsAsFactors = FALSE),
      select.model %in% model & select.proposed.by %in% proposed.by &
        select.year %in% year & select.element %in% element &
        select.age %in% age & select.intake %in% intake & select.type %in% type
    )
    # 呼吸気道モデルの名前を揃える
    tc$to <- sub("^GI.Tract$", "Stomach.Contents", tc$to)
    tc$to <- sub("^Blood$", "Plasma", tc$to)
  }
  row.names(tc) <- NULL
  tc    
}

# 摂取条件に対して0でない初期値の表を与える関数
InitialValue <- function (
  element, age = "Adult", intake = "Inh", type = "Aerosol.Type.F", size = 1,
  worker=FALSE, model = "ICRP68_72", df.file = "deposition_fraction.csv"
) {
  # 経口摂取
  if (intake == "Ing") {
    iv <- switch(
      model,
      "ICRP68_72" = data.frame(
        compartment = "Stomach", initial.value = 1
      ),
      "Leggett2003" = data.frame(
        compartment = "Stomach.Contents", initial.value = 1
      )
    ) 
  }
  # エアロゾル吸入
  if (intake == "Inh" && grepl("Aerosol", type)) {
    df <- read.csv(df.file, stringsAsFactors = FALSE)
    iv <- df[df$age == age & df$worker == worker & df$size == size, ]
    # コンパートメント名には粒子の初期状態を表す'.i'をつける
    iv$compartment <- paste(iv$compartment, ".i", sep = "")
    colnames(iv) <- sub("deposition.fraction", "initial.value", colnames(iv))
  }
  # 単体ヨウ素蒸気
  if (element == "I" && intake == "Inh" && type == "Elemental.Iodine.Vapour") {
    iv <- data.frame(
      compartment = c("BB1.i", "BB2.i", "BBseq.i", "ET1.i", "ET2.i", "ETseq.i"),
      initial.value = c(0.2465, 0.25, 0.0035, 0.1, 0.3998, 2e-04)
    )
  }
  # ヨウ化メチル
  if (element == "I" && intake == "Inh" && type == "Methyl.Iodide")
    iv <- data.frame(compartment = "Blood", initial.value = 0.7)
  
  row.names(iv) <- NULL
  iv
}

# 摂取条件に対して預託線量係数を与える関数
DoseCoefficient <- function (
  mass.number, element, age = "Adult", intake = "Ing", type = NA, size = 1,
  worker=FALSE, organ = "Effective", dc.file = "icrp68_72dosecoeff.csv"
) {
  dc <- read.csv(dc.file)
  if (organ == "Whole.Body")
    organ <- "Effective"
  b <- dc$element == element & dc$mass.number == mass.number & dc$age == age &
    dc$worker == worker & dc$intake == intake & dc$organ == organ
  if (!is.na(type))
    b <- b & dc$type == type
  if (intake == "Inh" && grepl("Aerosol", type))
    b <- b & dc$size == size
  if (sum(b) == 1)
    dc[b, "dose"]
  else
    NA
}

# 入力した摂取条件に対して移行係数, 初期値, 崩壊定数, 預託実効線量係数
# からなるリストを返す関数
ModelParameters <- function (
  mass.number, element, age = "Adult", intake = "Ing", type = NA, size = 1,
  worker=FALSE, model = "ICRP68_72",
  tc.file = "transfer_coefficient.csv", df.file = "deposition_fraction.csv",
  decay.file = "decay_data.csv", dc.file = "icrp68_72dosecoeff.csv"
) {
  # mass.number: 放射性核種の質量数
  # element: 放射性核種の元素名("Cs"または"I")
  # age: 年齢("Adult", "15y", "10y", "5y", "1y"または"3mo")
  # intake: 経口/吸入摂取("Ing"または"Inh")
  # type: 化学形("Aerosol.Type.F", "Aerosol.Type.M", "Aerosol.Type.S",
  #              "Methyl.Iodide"または"Elemental.Iodine.Vapour")
  # size: エアロゾル粒径[μm] (0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 5, 10)
  # worker: TRUEなら作業者(成人の場合のみ), FALSEなら公衆
  # model: "ICRP68_72"または"Leggett2003"を選択
  # tc.file: 移行係数データファイル名
  # df.file: 沈着割合データファイル名
  # decay.file: 核種崩壊データファイル名
  # dc.file: 預託実効線量係数データファイル名
  # 値: ICRP publ. 72 コンパートメントモデルの
  #     移行係数, 初期値, 崩壊定数, 預託実効線量係数のリスト

  # 選択可能な核種
  validNuclide <- switch(
    model,
    "ICRP68_72" = data.frame(
      mass.number = c(134, 137, 131), element = c("Cs", "Cs", "I")
    ),
    "Leggett2003" = data.frame(
      mass.number = c(134, 137), element = c("Cs", "Cs")
    )
  )
  # 選択可能な年齢
  validAge <- switch(
    model,
    "ICRP68_72" = c("Adult", "15y", "10y", "5y", "1y", "3mo"),
    "Leggett2003" = "Adult"
  )
  # 選択可能な摂取形態
  validIntake <- c("Ing", "Inh")
  # 選択可能な化学形
  validTypeAerosol <- c("Aerosol.Type.F", "Aerosol.Type.M", "Aerosol.Type.S")
  validTypeIodine <- c("Methyl.Iodide", "Elemental.Iodine.Vapour")
  # 選択可能なエアロゾル粒径
  validSize <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 5, 10)
  # 選択可能なモデル
  validModel <- c("ICRP68_72", "Leggett2003")

  # 変数のチェック
  if (!any(validNuclide$mass.number == mass.number &
             validNuclide$element == element))
    stop(mass.number, element, "は選択可能な核種ではありません\n")
  if (!age %in% validAge) {
    stop("変数ageの値は以下のどれかである必要があります:\n",
         paste(validAge, collapse=" "), "\n")
  }
  if (age != "Adult" && worker) # 作業者は大人のみ
    stop("変数workerの値がTRUEのときは変数ageの値は'Adult'である必要があります\n")
  if (!intake %in% validIntake) {
    stop("変数intakeの値は以下のどれかである必要があります:\n",
         paste(validIntake, collapse=" "), "\n")
  }
  if (intake == "Inh") {
    if (!element == "I") {
      if (!type %in% validTypeAerosol) {
        stop(
          "変数elementの値が'I'以外の場合, ",
          "変数typeの値は以下のどれかである必要があります:\n",
          paste(validTypeAerosol, collapse=" "), "\n"
        )
      }
    } else if (element == "I") {
      if (!type %in% c(validTypeAerosol, validTypeIodine)) {
        stop(
          "変数elementの値が'I'の場合, ",
          "変数typeの値は以下のどれかである必要があります:\n",
          paste(validTypeAerosol, validTypeIodine, collapse=" "), "\n"
        )
      }
    }
    if (type %in% validTypeAerosol && !size %in% validSize) {
      stop("変数sizeの値は以下のどれかである必要があります:\n",
           paste(validSize, collapse=" "), "\n")
    }
  }
  if (!model %in% validModel) {
    stop("変数modelの値は以下のどれかである必要があります:\n",
         paste(validModel, collapse=" "), "\n")
  }
  
  list(
    transfer.coefficient = TransferCoefficient(
      element = element, age = age, intake = intake, type = type,
      model = model, tc.file = tc.file
    ),
    initial.value = InitialValue(
      element = element, age = age, intake = intake, type = type, size = size,
      worker = worker, model = model, df.file = df.file
    ),
    decay.constant = DecayConstant(
      mass.number = mass.number, element = element, decay.file = decay.file
    ),
    dose.coefficient = DoseCoefficient(
      mass.number = mass.number, element = element, age = age, intake = intake,
      type = type, size = size, worker = worker, dc.file = dc.file
    )
  )
}

#### ベクトル・行列の関数 ####
CompartmentNames <- function(
  x, part = "all", external = c("Urine", "Feces", "Excreta", "Environment")
) {
  # x: 移行係数データフレームまたはそれを含むリスト
  # part: "all", "in"または"ex"
  # external: 体外コンパートメントの候補
  # 値: コンパートメント名の文字列ベクトル
  #     part="all"ならすべて, "in"なら体内のみ, "ex"なら体外のみ
  if (is.data.frame(x)) {
    i <- !is.na(x$transfer.coefficient)
    comp <- unique(c(as.character(x$from[i]), as.character(x$to[i])))
    switch(part,
           "all" = comp,
           "in" = setdiff(comp, external),
           "ex" = intersect(comp, external),
           stop("partの値は'all', 'in', 'ex'のどれか\n"))
  } else if (is.list(x)) {
    Recall(x = x$transfer.coefficient, part = part, external = external)
  }
}

TCMatrix <- function(x, decay.constant = 0) {
  # x: 移行係数データフレームまたは
  #    移行係数データフレームと崩壊定数を含むリスト
  # decay.constant: 崩壊定数(xがリストのときは省略可)
  # 値: 移行係数行列
  if (is.data.frame(x)) {
    x <- subset(x, !is.na(transfer.coefficient)) # 欠損値を除く
    comp <- CompartmentNames(x)
    size <- length(comp)
    m <- matrix(0, nrow = size, ncol = size)
    rownames(m) <- colnames(m) <- comp
    for (i in 1:nrow(x)) {
      m[as.character(x$to[i]), as.character(x$from[i])] <-
        x$transfer.coefficient[i]
    }
    m - diag(colSums(m) + decay.constant)
  } else if (is.list(x)) {
    Recall(x = x$transfer.coefficient, decay.constant = x$decay.constant)
  }
}

InitialVector <- function (x, iv = NULL) {
  # x: 移行係数データフレームまたは
  #    移行係数データフレームと初期値データフレームを含むリスト
  # iv: 初期値データフレーム(xがリストのときは省略可)
  # 値: 単位量の核種を急性摂取した時の初期値のベクトル
  if (is.data.frame(x) && is.data.frame(iv)) {
    comp <- CompartmentNames(x)
    iVec <- rep(0, length(comp))
    names(iVec) <- comp
    for (i in 1:nrow(iv)) {
      comp <- as.character(iv$compartment[i])
      iVec[comp] <- iv$initial.value[i]
    }
    iVec
  } else if (is.list(x)) {
    Recall(x = x$transfer.coefficient, iv = x$initial.value)
  } else {
    stop("変数xは移行係数データフレームまたは移行係数データフレームと
         初期値データフレームを含むリストである必要があります\n")
  }
}

#### 時間関数を作る関数 ####
RetentionFunction <- function (
  x, compartment = "Whole.Body", vectorValue = FALSE,
  eigenanalysis = FALSE, derivative = FALSE, integral = FALSE
) {
  # x: 移行係数データフレーム, 初期値データフレーム, 崩壊定数を含むリスト
  # compartment: コンパートメント名(省略すると全体内コンパートメントをとる)
  # vectorValue: TRUEならベクトル値関数, FALSEならコンパートメントの和をとる
  # eigenanalysis: TRUEなら固有値解析によって計算する
  #                FALSEなら'Matrix'パッケージのexpm()を使って計算する
  # derivative: TRUEなら導関数を求める
  # integral: TRUEなら積分を求める
  # 値: 急性摂取後の残留割合の時間関数またはその導関数または積分
  if (derivative && integral)
    stop("変数derivativeと変数integralは同時にTRUE値を取れません\n")
  
  compartment.all <- CompartmentNames(x)
  n <- length(compartment.all)
  if (compartment == "Whole.Body")
    compartment <- CompartmentNames(x, part = "in")
  if (eigenanalysis) { # 固有値解析を用いる解法
    eVal <- eigen(TCMatrix(x))$values # 固有値
    eVec <- eigen(TCMatrix(x))$vectors # 固有ベクトル
    coef <- solve(eVec, InitialVector(x)) # 初期値の固有ベクトルによる展開係数
    rownames(eVec) <- compartment.all
    eVec <- eVec[compartment, , drop=FALSE] # コンパートメントの限定
    # 移行係数行列を対角化する基底での解
    if (!derivative && !integral) { # 微分/積分オプション無し
      q <- function (t) {
        if (t >= 0)
          coef * exp(eVal * t)
        else if (t < 0)
          rep(0, n)
      }
    } else if (derivative && !integral) { # 導関数
      q <- function (t) {
        if (t > 0)
          coef * eVal * exp(eVal * t)
        else if (t < 0)
          rep(0, n)
        else if (t == 0) {
          p <- rep(0, n)
          p[coef > 0] <- Inf
          p[coef < 0] <- -Inf
          p
        }
      }
    } else if (integral && !derivative) { # 積分
      q <- function (t) {
        if (t > 0) {
          coef * sapply(
            eVal, function (a) if (a != 0) (exp(a*t)-1)/a else t
          )
        } else if (t <= 0) {
          rep(0, n)
        }
      }
    }
    # 基底の逆変換
    if (vectorValue) {
      r <- function(t) rowSums(sweep(eVec, 2, q(t), "*"))
    } else if (!vectorValue) {
      r <- function (t) sum(colSums(eVec) * q(t))
    }
  } else if (!eigenanalysis) {
    if (integral)
      stop("integralオプションが使えるのはeigenanalysisがTRUEのときのみです\n")
    library(Matrix)
    a <- TCMatrix(x)
    v <- InitialVector(x)
    r <- function (t) {
      if (t == 0) {
          q <- v
        if (derivative) {
          q[v > 0] <- Inf
          q[v < 0] <- -Inf
          q[v == 0] <- 0
        }
      } else if (t > 0) {
        q <- as.vector(expm(a*t) %*% v)
        if (derivative)
          q <- a %*% q
      } else if (t < 0) {
        q <- rep(0, times = length(compartment.all))
      }
      names(q) <- compartment.all
      if (vectorValue)
        q[compartment]
      else
        sum(q[compartment])
    }
  }
  # 時間変数についてベクトル化した関数
  function (t) sapply(t, r)
}