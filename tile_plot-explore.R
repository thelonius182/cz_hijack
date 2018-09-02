library(ggplot2)

mydata <- read.table(
  header = TRUE,
  text = "
  dataID  time    value
  session.1.a    0   g
  session.1.a    1   a
  session.1.a    2   g
  session.1.a    3   i
  session.1.a    4   g
  session.1.a    5   g
  session.1.a    6   i
  session.1.a    7   a
  session.1.a    8   g
  session.1.a    9   a
  session.1.a    10  a
  session.1.a    11  a
  session.1.a    12  a
  session.1.a    13  g
  session.1.a    14  g
  session.1.a    15  a
  session.1.a    16  a
  session.1.a    17  a
  session.1.a    18  a
  session.1.a    19  g
  session.1.a    20  g
  session.1.a    21  a
  session.1.a    22  g
  session.1.a    23  a
  session.1.a    24  a
  session.1.a    25  a
  session.1.a    26  a
  session.1.a    27  a
  session.1.a    28  a
  session.1.a    29  a
  session.1.a    30  a
  session.1.a    31  a
  session.1.a    32  a
  session.1.a    33  a
  session.1.a    34  a
  session.1.b    0   b
  session.1.b    1   b
  session.1.b    2   b
  session.1.b    3   j
  session.1.b    4   j
  session.1.b    5   j
  session.1.b    6   j
  session.1.b    7   b
  session.1.b    8   h
  session.1.b    9   h
  session.1.b    10  b
  session.1.b    11  b
  session.1.b    12  b
  session.1.b    13  b
  session.1.b    14  h
  session.1.b    15  b
  session.1.b    16  b
  session.1.b    17  b
  session.1.b    18  b
  session.1.b    19  b
  session.1.b    20  h
  session.1.b    21  b
  session.1.b    22  h
  session.1.b    23  b
  session.1.b    24  b
  session.1.b    25  b
  session.1.b    26  b
  session.1.b    27  b
  session.1.b    28  b
  session.1.b    29  b
  session.1.b    30  b
  session.1.b    31  b
  session.1.b    32  b
  session.1.b    33  b
  session.1.b    34  b
  session.2.a    0   a
  session.2.a    1   a
  session.2.a    2   g
  session.2.a    3   g
  session.2.a    4   g
  session.2.a    5   a
  session.2.a    6   g
  session.2.a    7   a
  session.2.a    8   a
  session.2.a    9   a
  session.2.a    10  a
  session.2.a    11  a
  session.2.a    12  a
  session.2.a    13  a
  session.2.a    14  a
  session.2.a    15  g
  session.2.a    16  a
  session.2.a    17  a
  session.2.a    18  a
  session.2.a    19  a
  session.2.a    20  g
  session.2.a    21  a
  session.2.a    22  a
  session.2.a    23  a
  session.2.a    24  a
  session.2.a    25  a
  session.2.b    0   j
  session.2.b    1   b
  session.2.b    2   j
  session.2.b    3   h
  session.2.b    4   h
  session.2.b    5   h
  session.2.b    6   h
  session.2.b    7   NA
  session.2.b    8   b
  session.2.b    9   b
  session.2.b    10  b
  session.2.b    11  b
  session.2.b    12  b
  session.2.b    13  b
  session.2.b    14  b
  session.2.b    15  b
  session.2.b    16  b
  session.2.b    17  b
  session.2.b    18  b
  session.2.b    19  b
  session.2.b    20  h
  session.2.b    21  b
  session.2.b    22  b
  session.2.b    23  b
  session.2.b    24  b
  session.2.b    25  b
  session.3.a    0   i
  session.3.a    1   a
  session.3.a    2   a
  session.3.a    3   a
  session.3.a    4   a
  session.3.a    5   g
  session.3.a    6   a
  session.3.a    7   a
  session.3.a    8   a
  session.3.a    9   a
  session.3.a    10  a
  session.3.a    11  a
  session.3.a    12  a
  session.3.a    13  a
  session.3.a    14  a
  session.3.a    15  i
  session.3.a    16  i
  session.3.a    17  g
  session.3.a    18  g
  session.3.a    19  g
  session.3.a    20  g
  session.3.a    21  a
  session.3.a    22  a
  session.3.a    23  a
  session.3.a    24  a
  session.3.a    25  g
  session.3.a    26  a
  session.3.a    27  a
  session.3.a    28  a
  session.3.a    29  a
  session.3.a    30  a
  session.3.a    31  a
  session.3.a    32  g
  session.3.a    33  g
  session.3.a    34  a
  session.3.a    35  a
  session.3.a    36  a
  session.3.a    37  a
  session.3.a    38  g
  session.3.a    39  a
  session.3.a    40  a
  session.3.a    41  a
  session.3.b    0   j
  session.3.b    1   b
  session.3.b    2   b
  session.3.b    3   b
  session.3.b    4   j
  session.3.b    5   h
  session.3.b    6   b
  session.3.b    7   b
  session.3.b    8   b
  session.3.b    9   b
  session.3.b    10  b
  session.3.b    11  b
  session.3.b    12  b
  session.3.b    13  b
  session.3.b    14  b
  session.3.b    15  b
  session.3.b    16  b
  session.3.b    17  b
  session.3.b    18  b
  session.3.b    19  h
  session.3.b    20  h
  session.3.b    21  b
  session.3.b    22  b
  session.3.b    23  b
  session.3.b    24  h
  session.3.b    25  b
  session.3.b    26  b
  session.3.b    27  b
  session.3.b    28  b
  session.3.b    29  b
  session.3.b    30  b
  session.3.b    31  b
  session.3.b    32  b
  session.3.b    33  h
  session.3.b    34  b
  session.3.b    35  b
  session.3.b    36  b
  session.3.b    37  b
  session.3.b    38  b
  session.3.b    39  b
  session.3.b    40  b
  session.3.b    41  b
  "
)

desiredLevels = c("a",
                  "b",
                  "c",
                  "d",
                  "e",
                  "f",
                  "g",
                  "h",
                  "i",
                  "j",
                  "NA")

desiredLabels = c("a",
                  "b",
                  "c",
                  "d",
                  "e",
                  "f",
                  "g",
                  "h",
                  "i",
                  "j",
                  "NA")

mydata$value = factor(mydata$value, levels = desiredLevels, labels = desiredLabels)

mydata$group = gsub("session.", "", as.character(mydata$dataID))
mydata$group = gsub("\\..", "", mydata$group)
mydata$group = paste0("Session ", mydata$group)
mydata$group = factor(mydata$group)

mydata$dataID = gsub("\\.[1|2|3]", "", as.character(mydata$dataID))
mydata$dataID = gsub("session.", "", as.character(mydata$dataID))

ggplot(mydata, aes(x = time, y = dataID, fill = value)) +
  geom_tile(stat = "identity",
            width = 1,
            height = .9) +
  facet_grid(group ~ .)