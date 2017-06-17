read_csv("test.csv",
         col_types = cols(
             名称 = col_character(),
                大小 = col_character(),
                价格 = col_integer(),
                日期 = col_date(format = "%m,%d,%y")
         ),
         locale = locale(encoding = "gbk"))
