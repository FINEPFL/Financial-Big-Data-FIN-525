getComData = function(){

    natural_gas = data.frame(read.csv("../datasets/commodities/naturalgas.csv",
                      header = TRUE, sep = ",", quote = "\"",
                                dec = ".", fill = TRUE))[,c("Date","Settle")]
    rice = data.frame(read.csv("../datasets/commodities/rice.csv",
                                      header = TRUE, sep = ",", quote = "\"",
                                      dec = ".", fill = TRUE))[,c("Date","Settle")]
    coffe = data.frame(read.csv("../datasets/commodities/coffe.csv",
                               header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE))[,c("Date","Settle")]
    sugar = data.frame(read.csv("../datasets/commodities/sugar.csv",
                                header = TRUE, sep = ",", quote = "\"",
                                dec = ".", fill = TRUE))[,c("Date","Settle")]
    plat = data.frame(read.csv("../datasets/commodities/plat.csv",
                                header = TRUE, sep = ",", quote = "\"",
                                dec = ".", fill = TRUE))[,c("Date","Settle")]
    gold = data.frame(read.csv("../datasets/commodities/gold.csv",
                               header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE))[,c("Date","Settle")]
    corn = data.frame(read.csv("../datasets/commodities/corn.csv",
                               header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE))[,c("Date","Settle")]
    shanghai_al = data.frame(read.csv("../datasets/commodities/shanghai_al.csv",
                               header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE))[,c("Date","Settle")]

    euronext = data.frame(read.csv("../datasets/commodities/euronext.csv",
                                      header = TRUE, sep = ",", quote = "\"",
                                      dec = ".", fill = TRUE))[,c("Date","Settle")]

    min_len = min(length(natural_gas$Date), length(rice$Date), length(coffe$Date),
                  length(sugar$Date), length(plat$Date), length(gold$Date),
                  length(corn$Date), length(shanghai_al$Date), length(euronext$Date))

    retVal = data.frame(natural_gas$Settle[1:min_len], rice$Settle[1:min_len],
                        coffe$Settle[1:min_len], sugar$Settle[1:min_len],
                        plat$Settle[1:min_len], gold$Settle[1:min_len],
                        corn$Settle[1:min_len], shanghai_al$Settle[1:min_len],
                        euronext$Settle[1:min_len])

     setnames(retVal,c("natrual_gas","rice","coffe", "sugar", "plat", "gold", "corn",
                      "shanghai_al", "euronext"))

    # retVal = xts(retVal[, -1], order.by=as.POSIXct(retVal$Date))

    selector = !apply(retVal, 1, function(row) any(is.na(row)))
    retVal = data.frame(retVal[selector, ])

    return (retVal)
}
