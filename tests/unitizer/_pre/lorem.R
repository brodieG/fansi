
lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
Encoding(lorem) <- "UTF-8"
# lorem.phrases <- unlist(strsplit(lorem, "[.,]\\K ", perl=TRUE))

# From the Stalin russian wiki page:
#
# <https://ru.wikipedia.org/w/index.php?title=%D0%A1%D1%82%D0%B0%D0%BB%D0%B8%D0%BD,_%D0%98%D0%BE%D1%81%D0%B8%D1%84_%D0%92%D0%B8%D1%81%D1%81%D0%B0%D1%80%D0%B8%D0%BE%D0%BD%D0%BE%D0%B2%D0%B8%D1%87&oldid=12832996>
#
# Licensed under GNU Free Documentation License since it is from before 2009

# lorem.ru <- "Родился 6 (18) декабря 1878 года (по записи в метрической книге Горийской Успенской соборной церкви[6]Это подлинная дата рождения Сталина. Позже она подтверждалась в уведомлении Санкт-Петербургского жандармского управления и самим Сталиным в ответе на анкету шведской газеты «Folkets Dagblad Politiken» в 1920 г.[7]) в Грузии в городе Гори, хотя начиная с 1929 года[источник?] днём его рождения официально считалось 9 (21) декабря 1879. Был третьим сыном в семье, первые двое умерли в младенчестве. Его родным языком был грузинский, русский язык Сталин выучил позже, но всегда говорил с заметным грузинским акцентом [8]. Согласно утверждениям дочери Светланы, Сталин, однако, пел по-русски практически без акцента."
# Encoding(lorem.ru) <- "UTF-8"
# lorem.ru.phrases <- unlist(strsplit(lorem.ru, "[.,]\\K ", perl=TRUE))

# From the Stalin turkish wiki page:
#
# <https://tr.wikipedia.org/w/index.php?title=Josef_Stalin&oldid=4719847>
#
# Licensed under GNU Free Documentation License since it is from before 2009

# lorem.tr <- "Bu tartışmalı tarihsel dönemle ilgili olarak, Stalin'e düşman veya Stalin'den yana olan her iki tarafın da farklı tezleri vardır. Stalin karşıtlarının tezlerine göre, Hitlerle aralarındaki açıklanmayan gizli protokole bağlı olarak Finlandiya, Estonya, Letonya, Litvanya, Romanya ve Polonya'nin Naziler veya Sovyetler tarafından işgalinin yolu açılmıştır. Stalin'in doğru yaptığını savunanlara göre ise, 1937'deki Münih görüşmelerinde açıkça ortaya çıktığı gibi, İngiliz ve Fransız emperyalistleri ve dolaylı olarak da Amerikalılar, Nazileri kışkırtıyorlardı ve onların Sovyetler Birliği'ne saldırısının önünü açmaya çalışıyorlardı. Bu amaçla Avusturya'nın Almanya'ya katılmasına (Anschluss) ve Çekoslovakya'nın işgaline göz yummuş ve onaylamışlardı.Ne var ki, özellikle Çekoslovakya'nın işgalinden sonra Sovyetler Birliği'nin İngiltere ve Fransa ile ilişki kurma çabalarına rağmen bu iki ülke Nazi tehdidini birlikte ortadan kaldırma girişimini reddetti. Böylece Sovyetler Birliği, kendi sınırlarını güvence altına almak için bu protokolü imzaladı. Stalin'in amaçlarına göre, Polonya ve Baltık ülkelerinde oluşturulacak tampon bölgeler, Nazilerin Sovyetler Birliği'ne ulaşmasını engelleyecekti. Böylece 1939 yılında Nazi işgalinden sonra Sovyetler Polonya'nın kalan yarısını işgal edip Estonya, Litvanya ve Letonya'yı sınırlarına kattı. Finlandiya'ya saldırdı ve büyük kayıplar vermesine rağmen Mart 1940'da \"kış savaşı' olarak bilinen bu savaşı da kazandı. 1941'de Hitler'in Sovyetlere saldırması üzerine Stalin bu sefer müttefiklerin yanında yer aldı. II. Dünya Savaşı'nın en ağır bedeli ödeyen güç olarak (24 milyon ölü) müttefiklerin yanında Nazi Almanyası'na karşı kazandığı zafer uluslararası alanda gücünü artırdı."
# Encoding(lorem.tr) <- "UTF-8"
# lorem.tr.phrases <- unlist(strsplit(lorem.tr, "[.,]\\K ", perl=TRUE))

# Emoji from Unicode Site <http://unicode.org/emoji/charts/full-emoji-list.html>

# emoji <- c(
#   "\U0001f600", "\U0001f619", "\U0001f61a", "\U0001f642", "\U0001f92f",
#   "\U0001f62c", "\U0001f630", "\U0001f631", "\U0001f633", "\U0001f92a",
#   "\U0001f635", "\U0001f637", "\U0001f912", "\U0001f915", "\U0001f922",
#   "\U0001f92e", "\U0001f927", "\U0001f607", "\U0001f920", "\U0001f921",
#   "\U0001f925", "\U0001f92b", "\U0001f92d", "\U0001f9d0", "\U0001f913",
#   "\U0001f608", "\U0001f4a9", "\U0001f63e",
#   "\U0001f469\U0001f3ff\U0000200d\U0001f3eb",
#   "\U0001f468\U0001f3fb\U0000200d\U00002696\U0000FE0F", "\U0001f46b",
#   "\U0001f469\U0000200D\U00002764\U0000200d\U0001f48b\U0000200d\U0001f468",
#   "\U0001f468\U0000200d\U0001f468\U0000200d\U0001f467",
#   "\U0001f468\U0000200d\U0001f468\U0000200d\U0001f467\U0000200d\U0001f466",
#   "\U0001f984", "\U0001f36b", "\U0001f1e6\U0001f1f7", "\U0001f1ed\U0001f1f0",
#   "\U0001f1ef\U0001f1f5", "\U0001f1f0\U0001f1f5", "\U0001f1fa\U0001f1f8",
#   "\U0001f3f4"
# )
# Encoding(emoji) <- "UTF-8"
# lorem.emo <- paste(
#   sample(
#     c(emoji, rep(", ", 3), rep(". ", 3), rep(" ", 40)), 450, replace=TRUE
#   ),
#   collapse=""
# )
# lorem.emo.phrases <- unlist(strsplit(lorem.emo, "[,.]\\K", perl=TRUE))

# From readLines(file.path(R.home("doc"), "THANKS"))

lorem.r.thanks <- "\033[31mMany\033[39m more, too numerous to mention here, have contributed by sending bug reports and suggesting various improvements.\n\n\033[7m\033[31mSimon\033[39m \033[31mDavies\033[39m whilst at the \033[31mUniversity\033[39m of \033[31mAuckland\033[39m wrote the original version of glm().\033[27m\n\n\033[31mJulian\033[39m \033[31mHarris\033[39m and \033[31mWing\033[39m \033[31mKwong\033[39m (\033[31mTiki\033[39m) \033[31mWan\033[39m whilst at the \033[31mUniversity\033[39m of \033[31mAuckland\033[39m assisted \033[31mRoss\033[39m \033[31mIhaka\033[39m with the original \033[31mMacintosh\033[39m port.\n\n\033[7m\033[31mR\033[39m was inspired by the \033[31mS\033[39m environment which has been principally developed by \033[31mJohn\033[39m \033[31mChambers\033[39m, with substantial input from \033[31mDouglas\033[39m \033[31mBates\033[39m, \033[31mRick\033[39m \033[31mBecker\033[39m, \033[31mBill\033[39m \033[31mCleveland\033[39m, \033[31mTrevor\033[39m \033[31mHastie\033[39m, \033[31mDaryl\033[39m \033[31mPregibon\033[39m and \033[31mAllan\033[39m \033[31mWilks\033[39m.\033[27m\n\n\033[31mA\033[39m special debt is owed to \033[31mJohn\033[39m \033[31mChambers\033[39m who has graciously contributed advice and encouragement in the early days of \033[31mR\033[39m and later became a member of the core team.\n\n\033[7m\033[31mThe\033[39m \033[31mR\033[39m \033[31mFoundation\033[39m may decide to give out <first.lastname>@\033[31mR\033[39m-project.org email addresses to contributors to the \033[31mR\033[39m \033[31mProject\033[39m (even without making them members of the \033[31mR\033[39m \033[31mFoundation\033[39m) when in the view of the \033[31mR\033[39m \033[31mFoundation\033[39m this would help advance the \033[31mR\033[39m project.\033[27m\n\n\033[31mThe\033[39m \033[31mR\033[39m \033[31mCore\033[39m \033[31mGroup\033[39m, \033[31mRoger\033[39m \033[31mBivand\033[39m, \033[31mJennifer\033[39m \033[31mBryan\033[39m, \033[31mDi\033[39m \033[31mCook\033[39m, \033[31mDirk\033[39m \033[31mEddelbuettel\033[39m, \033[31mJohn\033[39m \033[31mFox\033[39m, \033[31mBettina\033[39m \033[31mGrün\033[39m, \033[31mFrank\033[39m \033[31mHarrell\033[39m, \033[31mTorsten\033[39m \033[31mHothorn\033[39m, \033[31mStefano\033[39m \033[31mIacus\033[39m, \033[31mJulie\033[39m \033[31mJosse\033[39m,  \033[31mBalasubramanian\033[39m \033[31mNarasimhan\033[39m, \033[31mMarc\033[39m \033[31mSchwartz\033[39m, \033[31mHeather\033[39m \033[31mTurner\033[39m, \033[31mBill\033[39m \033[31mVenables\033[39m, \033[31mHadley\033[39m \033[31mWickham\033[39m and \033[31mAchim\033[39m \033[31mZeileis\033[39m are the ordinary members of the \033[31mR\033[39m \033[31mFoundation\033[39m. \033[31mIn\033[39m addition, \033[31mDavid\033[39m \033[31mMeyer\033[39m and \033[31mSimon\033[39m \033[31mWood\033[39m are also e-addressable by <\033[31mFirstname\033[39m>.<\033[31mLastname\033[39m>@\033[31mR\033[39m-project.org."
Encoding(lorem.r.thanks) <- "UTF-8"

# From readLines(file.path(R.home("doc"), "THANKS"))

lorem.r.thanks.2 <- c(
  "Many more, too numerous to mention here, have contributed by sending bug",
  "reports and suggesting various improvements.", "",
  "Simon Davies whilst at the University of Auckland wrote the original",
  "version of glm().", "",
  "Julian Harris and Wing Kwong (Tiki) Wan whilst at the University of",
  "Auckland assisted Ross Ihaka with the original Macintosh port.", "",
  "R was inspired by the S environment which has been principally",
  "developed by John Chambers, with substantial input from Douglas Bates,",
  "Rick Becker, Bill Cleveland, Trevor Hastie, Daryl Pregibon and",
  "Allan Wilks.", "",
  "A special debt is owed to John Chambers who has graciously contributed",
  "advice and encouragement in the early days of R and later became a",
  "member of the core team.", "", "", "",
  "The R Foundation may decide to give out <first.lastname>@R-project.org",
  "email addresses to contributors to the R Project (even without making them",
  "members of the R Foundation) when in the view of the R Foundation this",
  "would help advance the R project.", "",
  "The R Core Group, Roger Bivand, Jennifer Bryan, Di Cook, Dirk Eddelbuettel,",
  "John Fox, Bettina Grün, Frank Harrell, Torsten Hothorn, Stefano Iacus,",
  "Julie Josse,  Balasubramanian Narasimhan, Marc Schwartz, Heather Turner,",
  "Bill Venables, Hadley Wickham and Achim Zeileis are the ordinary members of",
  "the R Foundation.",
  "In addition, David Meyer and Simon Wood are also e-addressable by",
  "<Firstname>.<Lastname>@R-project.org."
)
Encoding(lorem.r.thanks.2) <- "UTF-8"





