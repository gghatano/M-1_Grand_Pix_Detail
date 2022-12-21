---
editor: visual
title: M1データの前処理
toc-title: Table of contents
---

## M1審査の課題

審査員の点数の付け方に基準がありません。そのため、審査員の１人が、極端に分散が大きな評価点の付け方をすると、その人の評価で順位が決まってしまいます。

簡単のために、出場者が2組で、審査員5名とします。審査員Aが1組目に90点、2組目に10点、審査員B,C,D,Eが1組目に90点、2組目に91点だった場合を考えます。

  コンビ名   合計   審査員A   審査員B   審査員C   審査員D   審査員E
  ---------- ------ --------- --------- --------- --------- ---------
  コンビX    450    90        90        90        90        90
  コンビY    374    10        91        91        91        91

5人中4人がコンビYを高く評価しているのに、審査員Aが極端な点数の付け方をしているため、総合得点で評価した場合の優勝は、コンビXになってしまいます。

## 課題への対応案：「審査員ごとの順位点」による順位づけ

そこで、点数ではなく、審査員ごとの順位ベースにを集計することで、分散の影響を消すことができます。
先の例だと、審査員Aの点数だと、1組目が1位で2組目が2位。他の4人それぞれ点数だと、1組目が2位で2組目が1位です。順位を利用して総合得点をつけることで、審査員ごとの点数の幅を補正することができます。

  ---------------------------------------------------------------------------------------------
  コンビ名   順位合計点   審査員A順位   審査員B順位   審査員C順位   審査員D順位   審査員E順位
  ---------- ------------ ------------- ------------- ------------- ------------- -------------
  コンビX    9            1             2             2             2             2

  コンビY    6            2             1             1             1             1
  ---------------------------------------------------------------------------------------------

順位の数値は低い方が嬉しいことに注意しつつ計算してみると、1組目が9点(=1+2+2+2+2)で2組目が6点(=2+1+1+1+1)となり、2組目が優勝になります。

低い方が嬉しいことがちょっと気持ち悪い場合は、例えば下から何番目か、などの数値を利用すれば、高い方が嬉しくなります。この辺はなんとでもなります。

## 実験

実際に過去のM1グランプリのデータを利用して、総合得点と提案手法とで、ファイナルラウンド進出者(上位3名)の変化を見てみます。
審査員ごとの評価点は[Wikipedia](https://ja.wikipedia.org/wiki/M-1%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%97%E3%83%AA)にありました。これを前処理して、所望のデータを作成します。

## M1データの前処理

[とりあえずspreadsheet](https://docs.google.com/spreadsheets/d/12JcpUWepZ6qqvxiT_-6eswfHO4pbxc_HvThU2P2ztNY/edit#gid=0)に、生データを配置しました。

集計できるように、(コンテスト番号,コンビ名,審査員,点数)というデータに変形していきます。

### パッケージ読み込み

::: cell
``` {.r .cell-code}
# install.packages('googlesheets4')
# library("googlesheets4")
library("dplyr")
library("tidyr")
library("readr")
library("xtable")
```
:::

### 各回データの整形(初回のみ)

::: cell
:::

### 審査結果まとめ

既に作成したものを[gistに配置](https://gist.githubusercontent.com/gghatano/092a9f7089451de743f39a53dda24423/raw/b4d8e86381542e5adf802514f7f7d09aa309a6e5/m1_score_detail.csv)しています。

``` {.r .cell-code}
dat_all = read_csv("https://gist.githubusercontent.com/gghatano/092a9f7089451de743f39a53dda24423/raw/b4d8e86381542e5adf802514f7f7d09aa309a6e5/m1_score_detail.csv")
```

    Rows: 1213 Columns: 4
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (2): unit, judge
    dbl (2): contest_num, score

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` {.r .cell-code}
dat_all %>% head() %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit     judge      score
  ------------- -------- -------- -------
              1 中川家   きよし        91
              1 中川家   青島          90
              1 中川家   小朝          90
              1 中川家   石井          90
              1 中川家   鴻上          85
              1 中川家   松本          70
:::

所望の形式のデータができました。

## 集計

では、集計していきます。普通の合計点と、順位点での合計点を算出していきます。

::: cell
``` {.r .cell-code}
## 審査員ごとの順位を算出：「順位点」とする
dat_all %>% 
  group_by(contest_num, judge) %>% 
  mutate(rank_score = rank(-score, ties.method = "min")) %>% 
  select(-score) %>% 
  ungroup() -> dat_rank


## 順位点をマージ
dat_all_rank = 
  dat_all %>% merge(dat_rank, by = c("contest_num", "unit", "judge"))

## 総合得点と総合順位点を算出
dat_all_rank %>% 
  group_by(contest_num, unit) %>% 
  summarise(score_sum = sum(score),
            rank_score_sum = sum(rank_score)) %>% 
  ungroup() -> dat_all_summarized
```

::: {.cell-output .cell-output-stderr}
    `summarise()` has grouped output by 'contest_num'. You can override using the
    `.groups` argument.
:::

``` {.r .cell-code}
## 総合得点と総合順位点で、それぞれ順位を算出
dat_all_summarized %>% 
  group_by(contest_num) %>% 
  mutate(normal_rank = rank(-score_sum, ties.method = "min")) %>% 
  mutate(rank_score_rank = rank(rank_score_sum, ties.method = "min")) %>% 
  ungroup() -> dat_all_summarized

## ファイナルラウンド進出者を、2つの点数の両方で抽出する
dat_finalround = 
  dat_all_summarized %>% 
  filter((normal_rank <= 3) | (rank_score_rank <= 3))

dat_finalround %>% select(contest_num, unit, normal_rank, rank_score_rank) %>% head %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                   normal_rank   rank_score_rank
  ------------- -------------------- ------------- -----------------
              1 アメリカザリガニ                 3                 3
              1 ハリガネロック                   2                 2
              1 中川家                           1                 1
              2 フットボールアワー               1                 1
              2 ますだおかだ                     2                 2
              2 笑い飯                           3                 3
:::
:::

できました。

## 結果の確認

さて、結果を見てみましょう。

普通の得点での順位(normal_rank)と、順位点での順位(rank_score_rank)で、
ファイナルラウンド進出者の顔ぶれを確認してみます。
顔ぶれが大きく変わっていると面白そうですが、どうでしょう。

3回ごとに見ていきます \### 第1-3回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num <= 3) %>% 
  arrange(contest_num, normal_rank) %>%
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                   normal_rank   rank_score_rank
  ------------- -------------------- ------------- -----------------
              1 中川家                           1                 1
              1 ハリガネロック                   2                 2
              1 アメリカザリガニ                 3                 3
              2 フットボールアワー               1                 1
              2 ますだおかだ                     2                 2
              2 笑い飯                           3                 3
              3 フットボールアワー               1                 1
              3 笑い飯                           2                 2
              3 アンタッチャブル                 3                 3
:::
:::

そういえば、第1回は2組でファイナルラウンドをやっていましたね。忘れていました。とはいえ、顔ぶれは変わらず。

### 第4-6回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num >= 4 & contest_num <= 6) %>% 
  arrange(contest_num, normal_rank) %>%
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                   normal_rank   rank_score_rank
  ------------- -------------------- ------------- -----------------
              4 アンタッチャブル                 1                 1
              4 南海キャンディーズ               2                 2
              4 麒麟                             3                 3
              5 ブラックマヨネーズ               1                 1
              5 麒麟                             2                 2
              5 笑い飯                           3                 3
              6 チュートリアル                   1                 1
              6 フットボールアワー               2                 2
              6 麒麟                             3                 3
:::
:::

変わっていません。

### 第7-9回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num >= 7 & contest_num <= 9) %>% 
  arrange(contest_num, normal_rank) %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                   normal_rank   rank_score_rank
  ------------- -------------------- ------------- -----------------
              7 サンドウィッチマン               1                 1
              7 キングコング                     2                 2
              7 トータルテンボス                 3                 2
              8 オードリー                       1                 1
              8 NON STYLE                        2                 2
              8 ナイツ                           3                 3
              9 笑い飯                           1                 1
              9 パンクブーブー                   2                 2
              9 NON STYLE                        3                 3
:::
:::

はい

### 第10-12回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num >= 10 & contest_num <= 12) %>% 
  arrange(contest_num, normal_rank) %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                     normal_rank   rank_score_rank
  ------------- ---------------------- ------------- -----------------
             10 パンクブーブー                     1                 1
             10 笑い飯                             1                 2
             10 スリムクラブ                       3                 3
             11 ジャルジャル                       1                 1
             11 トレンディエンジェル               2                 2
             11 銀シャリ                           3                 5
             11 スーパーマラドーナ                 5                 3
             12 銀シャリ                           1                 1
             12 和牛                               2                 2
             12 スーパーマラドーナ                 3                 3
:::
:::

お？第11回は、スーパーマラドーナと銀シャリの順位が変わっていますね。
順位点ベースだったら、銀シャリよりもスーパーマラドーナが高評価です。
最終的にはトレンディエンジェルが優勝しました。

### 第13-15回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num >= 13 & contest_num <= 15) %>% 
  arrange(contest_num, normal_rank) %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit             normal_rank   rank_score_rank
  ------------- -------------- ------------- -----------------
             13 和牛                       1                 1
             13 ミキ                       2                 2
             13 とろサーモン               3                 3
             14 霜降り明星                 1                 1
             14 和牛                       2                 2
             14 ジャルジャル               3                 3
             15 ミルクボーイ               1                 1
             15 かまいたち                 2                 2
             15 ぺこぱ                     3                 3
:::
:::

変わらず

### 第16-18回

::: cell
``` {.r .cell-code}
dat_finalround %>% 
  select(contest_num, unit, normal_rank, rank_score_rank) %>% 
  filter(contest_num >= 16 & contest_num <= 18) %>% 
  arrange(contest_num, normal_rank) %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
    contest_num unit                   normal_rank   rank_score_rank
  ------------- -------------------- ------------- -----------------
             16 おいでやすこが                   1                 1
             16 マヂカルラブリー                 2                 2
             16 見取り図                         3                 3
             17 オズワルド                       1                 1
             17 インディアンス                   2                 3
             17 錦鯉                             2                 2
             18 さや香                           1                 1
             18 ロングコートダディ               2                 3
             18 ウエストランド                   3                 2
:::
:::

顔ぶれは変わらず、でした。

## 考察

第11回のM1グランプリ2015では、順位に変動がありました。銀シャリ(3位)とスーパーマラドーナ(5位)の順位が入れ替わります。
[特点の詳細](https://ja.wikipedia.org/wiki/M-1%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%97%E3%83%AA2015#%E3%83%95%E3%82%A1%E3%83%BC%E3%82%B9%E3%83%88%E3%83%A9%E3%82%A6%E3%83%B3%E3%83%89)を見てみると、
多くの審査員が1点程度の差をつけている中、チュートリアル徳井さんが6点差をつけています。

::: cell
``` {.r .cell-code}
dat_all_rank %>% 
  filter(judge=="徳井") %>% 
  select(-contest_num) %>% 
  arrange(rank_score) %>% 
  knitr::kable(format="markdown")
```

::: cell-output-display
  unit                   judge     score   rank_score
  ---------------------- ------- ------- ------------
  ジャルジャル           徳井         96            1
  銀シャリ               徳井         95            2
  メイプル超合金         徳井         91            3
  和牛                   徳井         90            4
  スーパーマラドーナ     徳井         89            5
  ハライチ               徳井         89            5
  馬鹿よ貴方は           徳井         89            5
  タイムマシーン3号      徳井         88            8
  トレンディエンジェル   徳井         88            8
:::
:::

ジャルジャルと銀シャリを特に高く評価していました。こういう評価も面白いですね。

## まとめ

点数をそのまま使うと不公平かも？と思い、順位で正規化した新しい評価点を提案してみました。
過去の審査結果に適用してみましたが、結果は現状と殆ど変わりませんでした。

審査員の方々はすごいですね...。

以上です。
