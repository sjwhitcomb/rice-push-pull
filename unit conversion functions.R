# convert umol/g DW --> g/kg DW
convert1 <- function (umol, MW) {
  umol*MW/10^3
}


# convert g/kg DW --> umol/g DW
convert2 <- function (g, MW) {
  g/MW*10^3
}