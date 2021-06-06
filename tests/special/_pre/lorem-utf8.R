lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
Encoding(lorem) <- "UTF-8"

# From the PRC chinese wiki page:
#
# <https://zh.wikipedia.org/w/index.php?title=%E4%B8%AD%E5%8D%8E%E4%BA%BA%E6%B0%91%E5%85%B1%E5%92%8C%E5%9B%BD&oldid=8944743>
#
# Licensed under GNU Free Documentation License since it is from before 2009

lorem.cn <- "中華人民共和國是單一制的多民族國家。全國劃分為23個省（其中台灣省并沒有實際管辖）、5個自治區、4個直轄市和2個根據一國兩制設立的特別行政區，均直屬於中央人民政府。中华人民共和国跨越五个地理时区，但全国均使用北京时间（UTC+8，东八区）作为标准时间。中華人民共和國官方認定的民族現有56個，其中最大民族汉族佔總人口的91.59%，其餘55族統稱為少数民族，所有民族統稱為中華民族；除回族外，其他54個少数民族如壮族、维吾尔族、滿族、蒙古族、藏族、朝鲜族等也多使用自己的語言與文字。主要宗教有佛教、道教、基督教（多指新教）、天主教和伊斯兰教等，但過半人口無特定宗教信仰。中华人民共和国的通用语言是汉语普通話，當中在中國大陸通行的漢字為簡體字，而在港澳台地區通行的漢字則為繁體字。"
Encoding(lorem.cn) <- "UTF-8"
# cn.split <-  "[、。]\\K"
# Encoding(cn.split) <- "UTF-8"
# lorem.cn.phrases <- unlist(strsplit(lorem.cn, cn.split, perl=TRUE))
# saveRDS(lorem.cn.phrases, 'unitizer/_pre/lorem.data/lorem.cn.phrases.RDS')
lorem.cn.phrases <- readRDS('unitizer/_pre/lorem.data/lorem.cn.phrases.RDS')

# mix of wide and normal characters and colors and styles

lorem.cn.phrases.2 <- gsub("\\d", "", lorem.cn.phrases)
lorem.cn.words <- substr(
  lorem.cn.phrases.2, 1, pmin(nchar(lorem.cn.phrases.2) - 1, c(2,3,4,5))
)
# not a grep split, so okay
lorem.words <- unlist(strsplit(tolower(lorem), "[^a-z]+"))
p0 <- function(...) paste0(..., collapse=' ')
lorem.mix <- paste0(
  c(
    p0(lorem.words[1:3]), " ", '\033[32m', p0(lorem.cn.words[4:6]), " ",
    '\033[7;1m', p0(lorem.words[4:10]),'\n\n\033[38;5;105m',
    p0(lorem.words[11:20]),  '\n\n',
    '\033[m', p0(lorem.cn.words[3:1]), " ", '\033[34;43m',
    p0(lorem.words[21:25]), "\033[49m", p0(lorem.words[26:30]),
    '\033[39;4m', " ",
    p0(lorem.cn.words[7:9]), "\n\n", p0(lorem.cn.words[10:12]), " ",
    p0(lorem.words[31:36]), "\033[0m", "\n"
  ),
  collapse=""
)
