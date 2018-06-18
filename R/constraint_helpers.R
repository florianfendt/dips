
makeConstraint = function(indices, n, type, eps = TRUE) {
  const = rep(0, n)
  n.const = n

  if (length(indices) == 0L) {
    return(NULL)
  }

  if (type == 1L) {
    const[indices] = c(1, -1)
    const.dir = "="
  } else {
    if (type == 2L) {
      const[indices] = c(-1, 1)
      if (eps) {
        const[n.const] = 1
      } # else {
      #  const[n.const] = 0
      # }

      const.dir = "<="
    } else {
      const2 = const
      if (type == 3L) {
        stopifnot(length(indices) == 4L)
        const[indices[1:2]] = c(1, -1)
        const2[indices[3:4]] = c(-1, 1)
        const = const + const2
        const.dir = "="
      } else {
        if (type == 4L) {
          const[indices[1:2]] = c(-1, 1)
          const2[indices[3:4]] = c(1, -1)
          const = const + const2
          if (eps) {
            const[n.const] = 1
          }#  else {
          #   const[n.const] = 0
          # }
          const.dir = "<="
        } else {
          stop("wrong type")
        }
      }
    }
  }
  const = as.data.frame(t(const))
  names(const) = 1:length(const)
  res = cbind(const, data.frame(rhos = 0, const.dir = const.dir,
    stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  res
}
