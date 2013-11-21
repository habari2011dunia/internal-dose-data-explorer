Internal Dose Data Explorer
概要:     内部被曝関連データとグラフを表示するShinyアプリ[参考3]
作者:     habari2011dunia
最終更新: 2013年11月21日
twitter:  https://twitter.com/habari2011dunia
E-mail:   habari2011dunia@gmail.com
GitHub:   https://github.com/habari2011dunia

起動方法:
Rおよびshiny, Matrix, ggplot2パッケージをインストールした状態でrun.Rを実行してください.
(必要ならsetwd関数を使ってこのファイルreadme.txtやrun.Rのある場所を作業ディレクトリにしてください.)
Windowsの場合, OpenShiny.exe(hoxo_m様[参考4])を実行することでも起動できます.

RのインストールはCRAN(http://cran.r-project.org/)[参考2]から. パッケージのインストールはRを起動して
> install.packages("shiny")
> install.packages("Matrix")
> install.packages("ggplot2")
と入力すればOKです.

使用方法:
左のサイドバーパネルで摂取条件を選び, 下部にある"更新する"ボタンを押して確定させてください.
右のメインパネル上部のタブで表示する項目を選びます.
対応核種はCs-134, Cs-137およびI-131です.
選択できる項目は急性摂取後の全身残留割合のグラフ, 体内動態モデルの移行係数および初期値の0でない成分です.
またメインパネル下部に選択した摂取条件のリストとそれに対する預託実効線量または等価線量係数の表が表示されます.

出典:
各種データはICRPのデータベースCD[参考1](無料ダウンロード可)に基づいています.

注意:
Windowsの場合, データの日本語表示はうまく行かないようです.

参考:
[1] ICRP Database of Dose Coefficients: Workers and Members of the Public; Ver. 3.0
http://www.icrp.org/page.asp?id=145
[2] The Comprehensive R Archive Networ
http://cran.r-project.org/
[3] RStudio - Shiny
http://shiny.rstudio.org/
[4] Shiny アプリをワンクリックで起動するやつ作った - ほくそ笑む
http://d.hatena.ne.jp/hoxo_m/20121122/p1
