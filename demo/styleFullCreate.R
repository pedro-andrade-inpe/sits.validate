require(sits.validate)

read_csv2("inst/extdata/sits-brazil-legend.csv", col_types = cols(
  Value = col_double(),
  Label = col_character(),
  Short = col_character(),
  Color = col_character()
))->csv

buildStyle(csv, "myfile2.qml")
