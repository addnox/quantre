#' Create an ExposureRating module
#' @examples
#' x <- data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, Inf), N = 10, TSI = c(50, 150, 250) * 10, Premium = c(50, 150, 250) * 10 * .001)
#' RP <- RiskProfile(x)
#' PropExpoRating <- ExposureRating(RP, ExpectLossRatio = .45, SubjectPremium = 9, RiskCurve(b = 0.0218339035107131, g = 1096.98894971097))
#'
#' ## Mean Loss
#' ## ... is passed to XL function
#' fMean(PropExpoRating, Limit = c(50, 200), Deductible = c(50, 100))
#' fMean(PropExpoRating, c("50 xs 50", "200 xs 100"))
#'
#' ## CDF
#' build_CDF(PropExpoRating, mseq(50, 250, n = 200))
#' @export
ExposureRating <- function(RiskProfile, ExpectLossRatio, RiskCurve, SubjectPremium = NULL) {
  # NB: RiskProfile has 3 fields: N, TSI and Premium
  if (!inherits(RiskProfile, "RiskProfile")) stop("`RiskProfile` must be a RiskProfile object.")
  if (is.null(SubjectPremium)) SubjectPremium <- RiskProfile[, sum(Premium)]
  curve_type <- attr(RiskCurve, "type", exact = TRUE)

  res <- structure(
    list(
      RiskProfile = RiskProfile,
      SubjectPremium = SubjectPremium,
      ExpectLossRatio = ExpectLossRatio,
      RiskCurve = RiskCurve,
      Type = curve_type
    ),
    class = "ExposureRating"
  )
}

#' @export
#' @rdname ExposureRating
#' @usage
#' ## Loss in Layer
#' fMean(ExposureRating)
fMean.ExposureRating <- function(x, ...) {
  .RP <- data.table::as.data.table(x$RiskProfile) ## stripe out RiskProfile class
  Curve_Calc <- x$RiskCurve
  SubjectPrem <- x$SubjectPremium
  ELR <- x$ExpectLossRatio
  .Contract <- parse_XL(...)
  Limit <- .Contract$Limit
  Deductible <- .Contract$Deductible

  total_prem <- sum(.RP$Premium, na.rm = TRUE)

  .RP[, idx := .I]
  .Layer <- data.table::data.table(layerid = seq_along(Limit), Limit = Limit, Deductible = Deductible)

  RPFull <- data.table::CJ(idx = .RP[["idx"]], layerid = .Layer[["layerid"]], sorted = FALSE) |>
    merge(.RP, by = "idx", all.x = TRUE) |>
    merge(.Layer, by = "layerid", all.x = TRUE)

  if (x$Type == "Property") {
    res_calc <- RPFull[, {
      AvgSI = TSI / N
      stepSize = pmax(pmin(Deductible * 0.00000001, Limit), 0.01)
      AdjPrem = Premium * SubjectPrem / total_prem
      FLS_fguLimit = Curve_Calc(pmin(1, (Limit + Deductible) / AvgSI))
      FLS_Ded = Curve_Calc(pmin(1, Deductible / AvgSI))
      FLS_DedAddStep = Curve_Calc(pmin(1, (Deductible + stepSize) / AvgSI))
      Perc_LossExposed = FLS_fguLimit - FLS_Ded
      E_LossCount = (FLS_DedAddStep - FLS_Ded) * ELR * AdjPrem / stepSize
      E_LossSev = ELR * Perc_LossExposed * AdjPrem / E_LossCount

      list(layerid = layerid, E_LossCount = E_LossCount, E_LossSev = E_LossSev)
    }]
  } else {
    stop("Casualty Expo Rating to be implemented.", call. = FALSE)
  }

  res <- res_calc[
    , by = "layerid",
    {
     ExpectedLayerCount = sum(E_LossCount, na.rm = TRUE)
     ExpectedLayerLoss = sum(E_LossCount * E_LossSev, na.rm = TRUE)
     ExpectedLayerSeverity = ExpectedLayerLoss / ExpectedLayerCount
     list(
       ExpectedLayerLoss = ExpectedLayerLoss,
       ExpectedLayerCount = ExpectedLayerCount,
       ExpectedLayerSeverity = ExpectedLayerSeverity
     )
    }]

  res[, layerid := NULL]
  return(res[])
}

#' @export
#' @rdname ExposureRating
build_CDF <- function(x, q = NULL) {
  if (!inherits(x, "ExposureRating")) stop("`x` must be an ExposureRating object.", call. = FALSE)
  qMin <- min(q)

  # Loss increments
  Arr_LossAmount <- q
  Arr_Limit <- c(diff(q), .01)

  # Calculation
  temp_Count <- fMean(x, Limit = Arr_Limit, Deductible = Arr_LossAmount)$ExpectedLayerCount

  temp_CDF <- 1 - temp_Count / temp_Count[1]
  temp_CDF[length(temp_CDF)] <- 1 ## make last CDF equal 1

  severity <- data.table(Loss = Arr_LossAmount, CDF = temp_CDF)
  frequency <- temp_Count[1]
  return(list(LargeSeverity = severity, LargeFrequency = frequency))
}

#' @export
mseq <- function(from = 1, to = 1, n = 100, length.out = n) {
  by <- (to / pmax(from, 1)) ^ (1 / length.out)
  exp(log(from) + (0:length.out) * log(by))
}

#' @export
RiskCurve <- function(...) {
  prm <- dots_list(...)
  input_names <-  names(prm)
  input_length <- length(prm)

  if (setequal(input_names, c("b", "g"))) {
    # MB Curve
    f <- function(x) ecMBBEFD(x, b = prm[["b"]], g = prm[["g"]])
    curve_type <- "Property"
  } else if (setequal(input_names, c("base", "z"))) {
    # Riebesell
    f <- function(x) ecRiebesell(x, base = prm[["base"]], z = prm[["z"]])
    curve_type <- "Casualty"
  } else if (input_length == 1 & is.data.frame(..1)) {
    # table format Risk Curve
    RiskCurveDT <- as.data.table(..1)
    if (setequal(names(RiskCurveDT), c("Percent", "FLS"))) {

      curve_type <- "Property"
    } else if (setequal(names(RiskCurveDT), c("Limit", "ILF"))) {

      curve_type <- "Casualty"
    } else {
      ## error message
    }
  } else if (input_length == 1 & as.character(..1)) {
    #
  }

  attr(f, "type") <- curve_type
  f
}

#' @export
ecMBBEFD = function(x, b, g) {
  if (!(g >= 1 && b >= 0))
    return(rep(NaN, length(x)))
  if (g == 1 || b == 0) {
    res <- x
  }
  else if (g == 1/b && b < 1) {
    res <- (1 - b^x)/(1 - b)
  }
  else if (g > 1 && b == 1) {
    res <- log(1 + (g - 1) * x)/log(g)
  }
  else {
    res <- log((g - 1) * b/(1 - b) + (1 - g * b) * b^x/(1 - b))/log(g * b)
  }
  res[x < 0] <- 0
  res[x > 1] <- 1
  res
}

#' @export
ecRiebesell = function(x, base, z) {
  res <- (x / base) ^ log2(1 + z)
  res
}

