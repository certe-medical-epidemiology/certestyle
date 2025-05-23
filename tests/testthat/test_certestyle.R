# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

test_that("colourpicker works", {
  expect_identical(as.character(colourpicker(NULL)), "#FFFFFF00")
  expect_identical(as.character(colourpicker(NA)), "#FFFFFF00")
  
  expect_identical(colourpicker("viridis", 10, opacity = TRUE),
                   colourpicker(viridisLite::viridis(10, alpha = 0)))
  expect_identical(grDevices::palette.colors(8, "R3"),
                   as.character(colourpicker("R3", 8)))
  expect_identical(grDevices::palette.colors(8, "R4"),
                   as.character(colourpicker("R4", 8)))
  expect_warning(colourpicker("R", 20))
    
  expect_identical(as.character(colourpicker(c("certeblauw", "red", "tan1", "#ffa", "FFAA00"))),
                   c("#4A647D", "#FF0000", "#FFA54F", "#FFFFAA", "#FFAA00"))
  
  expect_identical(as.character(colourpicker("certe", 12)),
                   unname(c(certe.colours[1:6], certe.colours[13:18])))
  expect_identical(as.character(colourpicker("certe2", 12)),
                   unname(c(certe.colours[7:12], certe.colours[19:24])))
  expect_identical(as.character(colourpicker("certe3", 12)),
                   unname(c(certe.colours[13:18], certe.colours[25:30])))
  expect_identical(as.character(colourpicker("certe0", 6)), unname(certe.colours[names(certe.colours) %like% "0$"]))
  expect_identical(as.character(colourpicker("certe1", 6)), unname(certe.colours[names(certe.colours) %like% "[a-z]$"]))
  expect_identical(as.character(colourpicker("certe2", 6)), unname(certe.colours[names(certe.colours) %like% "2$"]))
  expect_identical(as.character(colourpicker("certe3", 6)), unname(certe.colours[names(certe.colours) %like% "3$"]))
  expect_identical(as.character(colourpicker("certe4", 6)), unname(certe.colours[names(certe.colours) %like% "4$"]))
  expect_identical(as.character(colourpicker("certe5", 6)), unname(certe.colours[names(certe.colours) %like% "5$"]))
  expect_identical(as.character(colourpicker("certe6", 6)), unname(certe.colours[names(certe.colours) %like% "6$"]))
  
  expect_identical(as.character(colourpicker("certeblauw")), unname(certe.colours[names(certe.colours) == "certeblauw"]))
  expect_identical(as.character(colourpicker("certegroen")), unname(certe.colours[names(certe.colours) == "certegroen"]))
  expect_identical(as.character(colourpicker("certeroze")), unname(certe.colours[names(certe.colours) == "certeroze"]))
  expect_identical(as.character(colourpicker("certegeel")), unname(certe.colours[names(certe.colours) == "certegeel"]))
  expect_identical(as.character(colourpicker("certelila")), unname(certe.colours[names(certe.colours) == "certelila"]))
  expect_identical(as.character(colourpicker("certebruin")), unname(certe.colours[names(certe.colours) == "certebruin"]))
  
  expect_identical(colourpicker("certe_sir"),
                   c(S = colourpicker("certegroen"),
                     SI = colourpicker("certegroen"),
                     I = colourpicker("certegeel"),
                     IR = colourpicker("certeroze"),
                     R = colourpicker("certeroze")))
  expect_identical(colourpicker("certe_sir2"),
                   c(S = colourpicker("certegroen2"),
                     SI = colourpicker("certegroen2"),
                     I = colourpicker("certegeel2"),
                     IR = colourpicker("certeroze2"),
                     R = colourpicker("certeroze2")))
  expect_identical(colourpicker("certe_rsi"),
                   c(S = colourpicker("certegroen"),
                     SI = colourpicker("certegroen"),
                     I = colourpicker("certegeel"),
                     IR = colourpicker("certeroze"),
                     R = colourpicker("certeroze")))
  expect_identical(colourpicker("certe_rsi2"),
                   c(S = colourpicker("certegroen2"),
                     SI = colourpicker("certegroen2"),
                     I = colourpicker("certegeel2"),
                     IR = colourpicker("certeroze2"),
                     R = colourpicker("certeroze2")))
  
  expect_identical(as.character(colourpicker("red")), "#FF0000")
  expect_identical(as.character(colourpicker("red", opacity = 0.5)), "#FF000080")
  
  expect_identical(as.character(colourpicker("viridis", 4)), substr(viridis(4), 1, 7))
  expect_identical(as.character(colourpicker("magma", 4)), substr(viridis(4, option = "magma"), 1, 7))
  expect_identical(as.character(colourpicker("inferno", 4)), substr(viridis(4, option = "inferno"), 1, 7))
  expect_identical(as.character(colourpicker("plasma", 4)), substr(viridis(4, option = "plasma"), 1, 7))
  expect_identical(as.character(colourpicker("cividis", 4)), substr(viridis(4, option = "cividis"), 1, 7))
  expect_identical(as.character(colourpicker("rocket", 4)), substr(viridis(4, option = "rocket"), 1, 7))
  expect_identical(as.character(colourpicker("mako", 4)), substr(viridis(4, option = "mako"), 1, 7))
  expect_identical(as.character(colourpicker("turbo", 4)), substr(viridis(4, option = "turbo"), 1, 7))
  
  if (getRversion() >= "4") {
    for (palette in grDevices::palette.pals()) {
      # this will test for "R3", "R4", "Okabe-Ito", etc.
      expect_identical(as.character(colourpicker(palette, 8)), grDevices::palette.colors(8, palette = palette))
    }
  }
  
  expect_identical(as.character(colourpicker("topo", 8)),
                   c("#4C00FF", "#0019FF", "#0080FF", "#00E5FF", "#00FF4D", "#E6FF00", "#FFFF00", "#FFE0B3"))
  expect_identical(as.character(colourpicker("heatmap", 8)),
                   c("#FF0000", "#FF3300", "#FF6600", "#FF9900", "#FFCC00", "#FFFF00", "#FFFF40", "#FFFFBF"))
  expect_identical(as.character(colourpicker("rainbow", 8)),
                   c("#FF0000", "#FFBF00", "#80FF00", "#00FF40", "#00FFFF", "#0040FF", "#8000FF", "#FF00BF"))
  expect_identical(as.character(colourpicker("terrain", 8)),
                   c("#00A600", "#3EBB00", "#8BD000", "#E6E600", "#E9BD3A", "#ECB176", "#EFC2B3", "#F2F2F2"))
  expect_identical(as.character(colourpicker("greyscale", 8)),
                   c("#4D4D4D", "#737373", "#8E8E8E", "#A4A4A4", "#B7B7B7", "#C8C8C8", "#D7D7D7", "#E6E6E6"))
  expect_identical(as.character(colourpicker("grayscale", 8)),
                   c("#4D4D4D", "#737373", "#8E8E8E", "#A4A4A4", "#B7B7B7", "#C8C8C8", "#D7D7D7", "#E6E6E6"))
  
  expect_identical(as.character(colourpicker("chocolate")), "#D2691E")
  expect_identical(as.character(colourpicker("coral")), "#FF7F50")
  expect_identical(as.character(colourpicker("firebrick1")), "#FF3030")
  expect_identical(as.character(colourpicker("lightgoldenrod")), "#EEDD82")
  expect_identical(as.character(colourpicker("yellow")), "#FFFF00")
  
  expect_output(print(rep(colourpicker("certe_rsi"), 10)))
  expect_s3_class(c(colourpicker("red"), colourpicker("red")), "colourpicker")
  expect_s3_class(rev(c(colourpicker("red"), colourpicker("blue"))), "colourpicker")
  expect_length(unique(c(colourpicker("red"), colourpicker("red"))), 1)
  expect_length(unique(c(col1 = colourpicker("red"), col2 = colourpicker("red"))), 2)
  expect_s3_class(unique(c(colourpicker("red"), colourpicker("red"))), "colourpicker")
  
  expect_warning(colourpicker("qwerty"))
})

test_that("adding white to a colour works", {
  expect_identical(as.character(add_white("red", 0.1)), "#FF1919")
  expect_identical(as.character(add_white("red", 0.2)), "#FF3333")
  expect_identical(as.character(add_white("red", 0.3)), "#FF4C4C")
  expect_identical(as.character(add_white("red", 0.4)), "#FF6666")
  expect_identical(as.character(add_white("red", 0.5)), "#FF7F7F")
  expect_identical(as.character(add_white("red", 0.95)), "#FFF2F2")
})

test_that("font colours works", {
  options(crayon.enabled = TRUE)
  
  expect_identical(font_stripstyle(font_black("test")), "test")
  expect_identical(font_stripstyle(font_blue("test")), "test")
  expect_identical(font_stripstyle(font_green("test")), "test")
  expect_identical(font_stripstyle(font_magenta("test")), "test")
  expect_identical(font_stripstyle(font_red("test")), "test")
  expect_identical(font_stripstyle(font_red_bg("test")), "test")
  expect_identical(font_stripstyle(font_silver("test")), "test")
  expect_identical(font_stripstyle(font_white("test")), "test")
  expect_identical(font_stripstyle(font_yellow("test")), "test")
  expect_identical(font_stripstyle(font_subtle("test")), "test")
  expect_identical(font_stripstyle(font_grey("test")), "test")
  expect_identical(font_stripstyle(font_bold("test")), "test")
  expect_identical(font_stripstyle(font_italic("test")), "test")
  expect_identical(font_stripstyle(font_underline("test")), "test")
  expect_identical(font_stripstyle(font_blue("test")), "test")
  expect_identical(font_stripstyle(font_blue("test1", "test2")),
                   "test1 test2")
  expect_identical(font_stripstyle(font_blue("test1", "test2", collapse = NULL)),
                   c("test1", "test2"))
})

test_that("dec_mark and big_mark work", {
  expect_true(dec_mark() %in% c(",", "."))
  expect_true(big_mark() %in% c(",", ".", " ", ""))
  
  options(dec_mark = ",", big_mark = NULL)
  expect_identical(dec_mark(), ",")
  expect_identical(big_mark(), ".")
  
  options(dec_mark = ".", big_mark = NULL)
  expect_identical(dec_mark(), ".")
  expect_identical(big_mark(), " ")
  
  options(dec_mark = ".", big_mark = ",")
  expect_identical(dec_mark(), ".")
  expect_identical(big_mark(), ",")
  
  options(dec_mark = "|", big_mark = NULL)
  expect_warning(dec_mark())
  options(dec_mark = NULL, big_mark = "|")
  expect_warning(big_mark())
})

# for the rest of the unit tests
options(dec_mark = ",", big_mark = ".")

test_that("format2 works", {
  
  expect_identical(format2(mean), format(mean))
  expect_identical(format2("test"), "test")
  
  dt <- Sys.Date()
  expect_identical(format2(dt, "dd-mm-yyyy"),
                   format(dt, "%d-%m-%Y"))
  expect_true(is.factor(format2(dt, "mmm")))
  expect_true(is.factor(format2(dt, "mmmm")))
  # should return Dutch month:
  expect_true(as.character(format2(dt, "mmmm")) %in% c("januari", "februari", "maart",
                                                       "april", "mei", "juni",
                                                       "juli", "augustus", "september",
                                                       "oktober", "november", "december"))
  
  expect_identical(format2(c("2021-05-10T11:00:00.0000000", "2021-05-10T11:00:00")),
                   as.character(as.POSIXct(c("2021-05-10 11:00:00", "2021-05-10 11:00:00"))))
  expect_identical(format2("2022-01-01"), "1 januari 2022")
  
  expect_identical(format2("2022-01-01", "yyyy-Q"), "2022-1")
  expect_identical(format2("2022-01-01", "yyyy-QQ"), "2022-Q1")
  expect_identical(format2("2022-01-01", "yyyy-qq"), "2022-Q1")
  expect_identical(format2(rep("2022-01-01", 10), "yyyy-qq"), rep("2022-Q1", 10))
  
  tm <- Sys.time()
  expect_identical(format2(tm), format(tm))
  
  expect_identical(format2(0.123), "0,12")
  expect_identical(format2("0.123"), "0,12")
  expect_identical(format2(0.123, percent = TRUE), "12,3%")
  expect_identical(format2("0.123", percent = TRUE), "12,3%")
  expect_identical(format2(cleaner::as.percentage(0.123)), "12,3%")
  
  expect_identical(format2(123, min_length = 6), "000123")
  expect_identical(format2(123, force_decimals = TRUE), "123,00")
  
  expect_equal(format2(hms::as_hms(tm)), format(tm, "%H:%M:%S"))
  expect_equal(format2(hms::hms(3, 2, 1)), "01:02:03")
  
  expect_equal(format2(difftime(tm, tm - 10)), "10")
  
  expect_identical(class(format2_scientific(1)), "expression")
  expect_identical(as.character(format2_scientific(c(1e3, 1e4))),
                   as.character(expression(10^3, 10^4)))
  
  expect_equal(format2(object.size(iris)), "7,1 kB")
})

test_that("is.double2 works", {
  expect_identical(as.double2(2), as.double(2))
  expect_identical(is.double2(2), is.double(2))
  expect_identical(as.double2("2"), as.double(2))
  expect_identical(is.double2("2"), is.double(2))
  expect_identical(as.double2("2.0"), as.double(2))
  expect_identical(is.double2("2.0"), is.double(2))
  expect_identical(as.double2("2,0"), as.double(2))
  expect_identical(is.double2("2,0"), is.double(2))
  expect_identical(as.double2(c(0.1, "0.1", "0,1")), c(0.1, 0.1, 0.1))
})

test_that("plain HTML works", {
  expect_identical(plain_html_table(mtcars[1, ]),
                   "<table><thead><td><strong> </strong></td><td><strong>mpg</strong></td><td><strong>cyl</strong></td><td><strong>disp</strong></td><td><strong>hp</strong></td><td><strong>drat</strong></td><td><strong>wt</strong></td><td><strong>qsec</strong></td><td><strong>vs</strong></td><td><strong>am</strong></td><td><strong>gear</strong></td><td><strong>carb</strong></td></thead><tr><td>Mazda RX4</td><td align=\"right\">21</td><td align=\"right\">6</td><td align=\"right\">160</td><td align=\"right\">110</td><td align=\"right\">3,9</td><td align=\"right\">2,62</td><td align=\"right\">16,46</td><td align=\"right\">0</td><td align=\"right\">1</td><td align=\"right\">4</td><td align=\"right\">4</td></tr></table>")
  expect_identical(plain_html_table(mtcars[1, ], max_col = 2),
                   "<table><thead><td><strong> </strong></td><td><strong>mpg</strong></td></thead><tr><td>Mazda RX4</td><td align=\"right\">21</td></tr></table>")
  # expect_identical(plain_html_table(cleaner::freq(iris$Species)),
  #                  "<table><thead><td><strong>Item</strong></td><td><strong>Count</strong></td><td><strong>Percent</strong></td><td><strong>Cum. Count</strong></td><td><strong>Cum. Percent</strong></td></thead><tr><td>setosa</td><td align=\"right\">50</td><td align=\"right\">33,33%</td><td align=\"right\">50</td><td align=\"right\"> 33,33%</td></tr><tr><td>versicolor</td><td align=\"right\">50</td><td align=\"right\">33,33%</td><td align=\"right\">100</td><td align=\"right\"> 66,67%</td></tr><tr><td>virginica</td><td align=\"right\">50</td><td align=\"right\">33,33%</td><td align=\"right\">150</td><td align=\"right\">100,00%</td></tr></table>")
})

test_that("toproper works", {
  expect_identical(toproper("this"), "This")
  expect_identical(toproper("this is a sentence."), "This is a sentence.")
  expect_identical(toproper("this is a sentence.", every_word = TRUE), "This is a Sentence.")
})

test_that("Certe styler works", {
  expect_identical(as.character(styler::style_text("example_isolates %>% count(hospital_id, gender) %>% plot2(x.title = \"Hospital\", y.title = \"Count\", title = \"Count isolates per hospital/gender\")",
                                                   style = certe_style_transformer)),
                   c("example_isolates %>%",
                     "  count(hospital_id,",
                     "        gender) %>%",
                     "  plot2(x.title = \"Hospital\",",
                     "        y.title = \"Count\",",
                     "        title = \"Count isolates per hospital/gender\")"))
})

test_that("tibble printing works", {
  options(print.data.frame_as_tibble = TRUE)
  expect_output(print(mtcars))
  expect_output(print(tibble::as_tibble(mtcars)))
  expect_output(print(data.frame(a = 123, b = TRUE, c = "text")))
})

test_that("rmd template work", {
  expect_false(rmarkdown_date() == "")
  expect_false(rmarkdown_template("latex") == "")
  expect_false(rmarkdown_template("word") == "")
  expect_error(rmarkdown_template("other") == "")
  expect_false(rmarkdown_logo() == "")
})
