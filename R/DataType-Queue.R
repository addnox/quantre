#' Create a queue
#'
#' @export

queue <- function (init = 20, missing_default = NULL) {
  force(missing_default)
  q <- vector("list", init)
  head <- 0L
  tail <- 0L
  count <- 0L
  add <- function(x) {
    force(x)
    capacity <- length(q)
    if (count + 1L > capacity) {
      capacity <- .resize_at_least(count + 1L)
    }
    if (capacity - head >= 1L) {
      head <<- head + 1L
    }
    else {
      head <<- 1L
    }
    if (is.null(x)) {
      q[head] <<- list(NULL)
    }
    else {
      q[[head]] <<- x
    }
    if (tail == 0L) {
      tail <<- 1L
    }
    count <<- count + 1L
    invisible()
  }
  madd <- function(..., .list = NULL) {
    if (is.null(.list)) {
      args <- list(...)
    }
    else {
      args <- c(list(...), .list)
    }
    n_args <- length(args)
    if (n_args == 0L) {
      return(invisible())
    }
    capacity <- length(q)
    if (count + n_args > capacity) {
      capacity <- .resize_at_least(count + n_args)
    }
    n_until_wrap <- capacity - head
    if (n_until_wrap >= n_args) {
      q[head + seq_along(args)] <<- args
      head <<- head + n_args
    }
    else {
      if (n_until_wrap > 0) {
        q[seq.int(head + 1, capacity)] <<- args[seq_len(n_until_wrap)]
      }
      n_after_wrap <- n_args - n_until_wrap
      q[seq_len(n_after_wrap)] <<- args[seq.int(n_until_wrap +
                                                  1, n_args)]
      head <<- head + n_args - capacity
    }
    if (tail == 0L) {
      tail <<- 1L
    }
    count <<- count + n_args
    invisible()
  }
  remove <- function(missing = missing_default) {
    if (count == 0L)
      return(missing)
    capacity <- length(q)
    value <- q[[tail]]
    q[tail] <<- list(NULL)
    if (tail == head) {
      tail <<- head <<- 0L
    }
    else {
      tail <<- tail + 1L
      if (tail > capacity)
        tail <<- tail - capacity
    }
    count <<- count - 1L
    if (capacity > init && count <= capacity/4) {
      .resize_at_least(count + 1L)
    }
    value
  }
  mremove <- function(n, missing = missing_default) {
    n <- as.integer(n)
    if (n < 1) {
      stop("`n` must be at least 1.")
    }
    capacity <- length(q)
    values <- vector("list", n)
    run_length <- min(n, capacity - tail + 1L)
    if (head >= tail) {
      run_length <- min(run_length, head - tail + 1L)
    }
    run_idxs <- seq.int(tail, tail + run_length - 1)
    values[seq_len(run_length)] <- q[run_idxs]
    q[run_idxs] <<- list(NULL)
    total_filled <- run_length
    remaining_n <- n - run_length
    count <<- count - run_length
    tail <<- tail + run_length
    if (count == 0L) {
      head <<- 0L
      tail <<- 0L
    }
    else if (tail > capacity) {
      stopifnot(tail == capacity + 1L)
      tail <<- 1L
    }
    if (remaining_n > 0 && count != 0 && tail <= head) {
      stopifnot(tail == 1L)
      run_length <- min(remaining_n, head)
      run_idxs <- seq_len(run_length)
      values[seq.int(total_filled + 1, total_filled +
                       run_length)] <- q[run_idxs]
      q[run_idxs] <<- list(NULL)
      total_filled <- total_filled + run_length
      remaining_n <- remaining_n - run_length
      count <<- count - run_length
      tail <<- tail + run_length
      if (count == 0L) {
        stopifnot(tail == head + 1L)
        head <<- 0L
        tail <<- 0L
      }
    }
    if (remaining_n > 0) {
      stopifnot(count == 0)
      values[seq(total_filled + 1, n)] <- list(missing)
    }
    if (capacity > init && count <= capacity/4) {
      .resize_at_least(count + 1L)
    }
    values
  }
  peek <- function(missing = missing_default) {
    if (count == 0L) {
      return(missing)
    }
    q[[tail]]
  }
  reset <- function() {
    q <<- vector("list", init)
    head <<- 0L
    tail <<- 0L
    count <<- 0L
    invisible()
  }
  size <- function() {
    count
  }
  as_list <- function() {
    if (count == 0L)
      return(list())
    .as_list()
  }
  .as_list <- function(.size = count) {
    if (.size < count) {
      stop("Can't return list smaller than number of items.")
    }
    capacity <- length(q)
    low_tail <- tail
    if (head < tail)
      low_tail <- tail - capacity
    new_q <- vector("list", .size)
    old_idx <- (seq(low_tail, head) - 1L)%%capacity + 1L
    new_q[seq_len(length(old_idx))] <- q[old_idx]
    new_q
  }
  .resize <- function(n) {
    if (n < count) {
      stop("Can't shrink smaller than number of items (",
           count, ").")
    }
    if (n <= 0) {
      stop("Can't shrink smaller than one.")
    }
    if (length(q) == n) {
      return(n)
    }
    if (count == 0L) {
      q <<- vector("list", n)
      return(n)
    }
    q <<- .as_list(n)
    tail <<- 1L
    head <<- count
    n
  }
  .resize_at_least <- function(n) {
    doublings <- ceiling(log2(n/init))
    doublings <- max(0, doublings)
    new_capacity <- init * 2^doublings
    .resize(new_capacity)
  }
  list(add = add, madd = madd, remove = remove, mremove = mremove,
       peek = peek, reset = reset, size = size, as_list = as_list)
}

