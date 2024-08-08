#' Common contract terms as functionals
#'
#' @description
#' `ct_Excess` calculates LossInLayer based on specified Limit and Deductible structure
#' `ct_Commission` calculates commission figures based on given sliding scale table
#' `ct_Reinstatement` returns a list of two vectors, one being reinstated loss and the other is reinstated premium
#' `ct_ProfitCommission` calculates profit commission
#' `ct_LossParcipation` calculates loss participation (i.e. loss corridor) from cedent's perspective
#' @export
#' @param Loss,Premium Numeric vector for loss and premium
#' @param Limit,Deductible Limit and Deductible terms
#' @param is.FranchiseLimit Logical, franchise limit means there is no reimbursement when LossAboveDeducible is greater than Limit
#' @param is.FranchiseDed Logical, franchise deductible means when Loss is greater than Deductible, full f.g.u. loss is applicable for reimbursement
#' @section `Term` parameter for different functions:
#' * Commission:  parameter `Term` can either be a value (for fixed commission), or a `data.frame` to lookup
#' * LossParticipation: parameter `Term` is 3-column `data.frame`, with the sequence of `LowerLR`, `UpperLR` and `Participation`
#' * ProfitCommission: parameter `Term` either a number a 3-column data.frame, each representing `LowerProfit`, `UpperProfit` and `ProfitShare`.
#'
#' @examples
#' ## ct_Excess
#' ct_Excess(100, 200) ## Call `ct_Excess` returns a function
#' ct_Excess(100, 200)(c(250, 400))
ct_Excess <- function(Limit, Deductible, is.FranchiseLimit = FALSE, is.FranchiseDed = FALSE) {
  f <- function(Loss) {
    # above Ded
    if (is.FranchiseDed) {
      x1 <- Loss * (Loss > Deductible) ## same as MR behavior: Loss == Deductible won't trigger
    } else {
      x1 <- pmax(0, Loss - Deductible)
    }

    # cap by Limit
    # Franchise Limit means if beyond fguLimit, loss to layer will be 0
    if (is.FranchiseLimit) {
      x2 <- pmin(Limit, x1) * (x1 <= Limit) ## same as MR: LossInLayer == Limit will still be reimbursed
    } else {
      x2 <- pmin(Limit, x1)
    }

    x2
  }

  ContractTerm(
    f,
    term = list(
      Limit = Limit,
      Deductible = Deductible,
      is.FranchiseLimit = is.FranchiseLimit,
      is.FranchiseDed = is.FranchiseDed
    )
  )
}

#' @export
#' @rdname ct_Excess
#' @param is.CedentPerspective Logical
#' @examples
#' ## ct_LossParticipation
#' ct_LossParticipation(list(.8, Inf, .2))(c(.6, .9, 1)) ## a list, list(Low, High, LPC)
#' ct_LossParticipation(data.frame(c(0, .8, .9), c(.8, .9, Inf), c(0, .5, 0)))
ct_LossParticipation <- function(Term, is.CedentPerspective = TRUE) {
  # Term is 3-col DF, LowerLR, UpperLR, Participation
  # MR is RI's persp
  if (is.list(Term) & length(Term) == 3 & all(lapply(Term, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(Term) |> data.table::setnames(c("LowerLR", "UpperLR", "Participation"))
    if (!is.CedentPerspective) tmpDT[, Participation := 1 - Participation]
  } else {
    stop("`Term` must be 3-column data.frame, each representing LowerLR, UpperLR and Participation", call. = FALSE)
  }

  f <- function(Loss, Premium = 1) {
    tmpLR <- Loss / Premium
    LPC_ratio <- vapply(
      tmpLR,
      function(.LR) tmpDT[, sum(pmin(UpperLR - LowerLR, pmax(0, .LR - LowerLR)) * Participation)],
      numeric(1L)
    )

    res <- - LPC_ratio * Premium
    res
  }

  ContractTerm(
    f,
    term = list(
      LossParticipation = tmpDT
    )
  )
}

#' @export
#' @rdname ct_Excess
#' @examples
#' ## ct_ProfitCommission
#' ct_ProfitCommission(Term = .2, ExpenseAllowance = .2)(c(.8, .4, .2))
#' ct_ProfitCommission(Term = data.frame(c(0, .2), c(.2, Inf), c(.1, .5)), ExpenseAllowance = .2)(c(.8, .4, .2))
ct_ProfitCommission <- function(Term, ExpenseAllowance) {
  if (is.numeric(Term) && length(Term) == 1L) {
    tmpDT <- data.table::data.table(LowerProfit = 0, UpperProfit = Inf, ProfitShare = Term)
  } else if (is.data.frame(Term) & ncol(Term) == 3 & all(lapply(Term, class) == "numeric")) {
    tmpDT <- data.table::as.data.table(Term) |> data.table::setnames(c("LowerProfit", "UpperProfit", "ProfitShare"))
  } else {
    stop("`Term` must be either a number, or a 3-column data.frame, each representing LowerProfit, UpperProfit and ProfitShare")
  }

  f <- function(Profit, Premium = 1) {
    tmpProfitRate <- pmax(0, Profit / Premium - ExpenseAllowance)
    PC_ratio <- vapply(
      tmpProfitRate,
      function(.Profit) tmpDT[, sum(pmin(UpperProfit - LowerProfit, pmax(0, .Profit - LowerProfit)) * ProfitShare)],
      numeric(1L)
    )

    PC_ratio * Premium
  }

  ContractTerm(
    f,
    list(
      ProfitCommission = tmpDT,
      ExpenseAllowance = ExpenseAllowance
    )
  )
}

#' @export
#' @rdname ct_Excess
#' @param Term See Details
#' @param right.closed Logical, default to be TRUE.  RightBound inclusive
#' @examples
#' ## ct_Commission
#' SS <- data.table::data.table(LowerLR = c(0, .15, .2, .25), UpperLR = c(.15, .2, .25, Inf), Commission = c(.3, .25, .2, .15))
#' ct_Commission(SS)(c(.19, .2, .21))
ct_Commission <- function(Term, right.closed = TRUE) {
  # Term can be a number, then fixed Comm
  # Term can be a 2 or 3 col DF, with name (LowerLR,) UpperLR and Commission

  if (is.data.frame(Term) && all(lapply(Term, class) == "numeric")) {
    lookupDT <- data.table::as.data.table(Term)

    if (ncol(lookupDT) == 3) lookupDT <- lookupDT[, 2:3]

    data.table::setnames(lookupDT, c("LossRatio", "Commission"))
    if (lookupDT[, min(LossRatio) != 0]) lookupDT <- rbind(lookupDT[1, ][, LossRatio := 0], lookupDT)
    if (lookupDT[, is.finite(max(LossRatio))]) lookupDT <- rbind(lookupDT, lookupDT[.N, ][, LossRatio := Inf])

    is.Sliding <- TRUE
  } else if (is.numeric(Term) && length(Term) == 1) {
    lookupDT <- Term
    is.Sliding <- FALSE
  } else {
    stop("`Commission` must be a single value or a data.frame to look up.", call. = FALSE)
  }

  f <- function(Loss, Premium = 1) {
    if (length(Premium) == 1) Premium <- rep(Premium, times = length(Loss))

    if (is.Sliding) {
      band_idx <- cut(Loss, breaks = lookupDT[[1]], labels = FALSE, include.lowest = TRUE, right = right.closed)
      res <- lookupDT[[2]][band_idx + 1] * Premium
    } else {
      res <- Term * Premium
    }

    res
  }

  ContractTerm(
    f,
    list(
      Commission = lookupDT,
      right.closed = right.closed
    )
  )
}

#' @export
#' @rdname ct_Excess
#' @examples
#' ## ct_Reinstatement
#' ct_Reinstatement("2@100%", 100)(c(50, 800))
#' ct_Reinstatement(c(1.2, 1), 100)(c(50, 800))
#'
ct_Reinstatement <- function(Term, Limit) {
  if (is.data.frame(Term) && ncol(Term) == 2) {
    Reinstat <- data.table::as.data.table(Term) |> data.table::setnames(c("Reinstatement", "Percentage"))
  } else {
    Reinstat <- parse_reinstatement(Term)
  }

  Reinstat[, `:=`(Lower = Limit * (Reinstatement - 1), Upper = Limit * Reinstatement)]
  Reinstat[is.infinite(Lower), Lower := 0]

  f <- function(Loss) {
    AAL <- Reinstat[, max(Upper) + Limit]
    LossToLayer <- pmin(Loss, AAL)

    DTLoss <- data.table::data.table(idL = seq_along(Loss), Loss = Loss)

    ReinsFactor <- data.table::CJ(idL = DTLoss$idL, Reinstatement = Reinstat$Reinstatement)[DTLoss, on = "idL"][Reinstat, on = "Reinstatement"][
      j =  .(x = sum(pmin(Upper - Lower, pmax(0, Loss - Lower)) * Percentage) / Limit),
      by = "idL"
    ]

    list(Loss = LossToLayer, ReinstatementFactor = ReinsFactor$x)
  }
}


ContractTerm <- function(f, term) {
  structure(f, class = "ContractTerm", term = term)
}

#' @export
print.ContractTerm <- function(x) {
  cat("<ContractTerm>: functional\n")

  print_Param(attr(x, "term"))

  invisible(x)
}

#' @export
get_term <- function(x) {
  if (!inherits(x, "ContractTerm")) stop("`x` must be a ContractTerm object.", call. = FALSE)
  attr(x, "term")
}

#' Helper function to create a XL contract structure
#' @examples
#' ## parse various XL terms
#' parse_XL(c("100M xs 200M", "200M xs 300M"))
#' parse_XL(c("100M xs 200M xs 50M", "200M xs 300M"))
#' parse_XL(Limit = 100, Deductible = 200, Reinstatement = "2@100%")
#' parse_XL(Limit = 100, Deductible = c(100, 200))
#' @export

parse_XL <- function(x = NULL, Limit = Inf, Deductible = 0, AAL = Inf, AAD = 0, Reinstatement = NA_character_, Premium = 0) {
  if (is.character(x)) {
    xx <- stringi::stri_split_regex(x, "x|xs", case_insensitive = TRUE)
    x1 <- data.table::transpose(lapply(xx, parse_numeric))
    data.table::setDT(x1)
    .name <- c("Limit", "Deductible", "AAD")
    data.table::setnames(x1, seq_along(x1), .name[seq_along(x1)])
    res <- cbind(x1, data.table::data.table(AAL = AAL, Reinstatement = Reinstatement, Premium = Premium))

    if (!"AAD" %in% names(res)) res <- cbind(res, data.table::data.table(AAD = AAD))

  } else {
    res <- data.table::data.table(Limit = Limit, Deductible = Deductible, AAL = AAL, AAD = AAD, Reinstatement = Reinstatement, Premium = Premium)
  }

  data.table::setcolorder(res, c("Limit", "Deductible", "AAL", "AAD", "Reinstatement", "Premium"))
  data.table::setnafill(res, fill = 0, cols = c("Deductible", "AAD", "Premium"))
  data.table::setnafill(res, fill = Inf, cols = c("Limit", "AAL"))
  res[]
}

#' Parse reinstatement terms into data.frame
#' @param reinstat a character atomic variable, e.g. "2@100\%"
#' @rdname parse_XL
#' @export
#' @examples
#' ## parse various reinstatement terms
#' parse_reinstatement("1@120%, 2@100%")
#' parse_reinstatement("1@120%;2@100%")
#' parse_reinstatement(c(1.2, 1))
#' parse_reinstatement(0) ## note this mean 1@0%, rather than Nil reinstatement
#' parse_reinstatement("2@120%")
#' parse_reinstatement("2@free")
#' parse_reinstatement("uf")
#' parse_reinstatement("nil")
#' parse_reinstatement("0@100%")
parse_reinstatement <- function(reinstat) {
  # numeric input
  if (is.numeric(reinstat)) {
    res <- data.table::data.table(Reinstatement = seq_along(reinstat), Percentage = reinstat)
    return(res)
  }

  # char input
  re2 <- stringi::stri_replace_all_fixed(reinstat, " ", "")

  if (tolower(re2) == "uf") re2 <- "unlimited@free"
  if (tolower(re2) == "nil") re2 <- "0@100%"

  re2 <- stringi::stri_replace_all_fixed(re2, "free", "0", case_insensitive = TRUE)
  re2 <- stringi::stri_replace_all_fixed(re2, "unlimited", "Inf", case_insensitive = TRUE)

  reinstat_split <- unlist(stringi::stri_split_regex(re2, "[,;]")) |> stringi::stri_split_fixed("@") |> data.table::transpose() |> lapply(parse_numeric)

  if ((length(reinstat_split[[1]]) == 1) && (is.infinite(reinstat_split[[1]]) | reinstat_split[[1]] == 0)) {
    res <- data.table::data.table(
      Reinstatement = reinstat_split[[1]],
      Percentage = reinstat_split[[2]]
    )
  } else {
    res <-   data.table::data.table(
      Reinstatement = 1:max(reinstat_split[[1]]),
      Percentage = reinstat_split[[2]]
    )
  }

  res
}
