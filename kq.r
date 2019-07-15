outer.multi = function(f, L) {
  args = Reduce(function(x, y) outer(x, y, function(a,b) mapply(c, a, b,
                                                           SIMPLIFY=FALSE)), L)         
  apply(args, 1:length(dim(args)), function(a) do.call(f, as.list(a[[1]])))            
}

                                                                               
C = function(n,k) ifelse(n < 0, 0, choose(n,k))


kq = function(cards = 52, kings = 4, queens = 4, gap = 1, cyclic = FALSE) {
  f = function(...) {
    v = c(...)
    lv = length(v)
    hv = lv/2
    free = cards - cyclic - kings*lv + sum(v[1:(lv-1)]*((lv-1):1))
    kpos = kings - cyclic - sum(v)
    if (!cyclic) {
      if (gap) r = v[1:(hv-1)] else r = NULL
      prod(choose(kings - 1 - cumsum(c(0, r)), v[1:hv])) *
        prod(choose(kings + 1 - sum(r) - cumsum(v[hv:(lv-1)]), v[(hv+1):lv])) *
        C(free, kpos) * C(free - kpos, queens)
    } else {
      prod(choose(kings - cumsum(c(0, v[1:(lv-1)])), v[1:lv])) *
        C(free, kpos) * C(free - kpos, queens) * cards/kings
    }
  }
  small = kings*2*(gap+1)
  L = c(rep(list(0:(kings - (cards >= small))), gap+1), 
        rep(list(0:(kings + (cards < small) - cyclic)), gap+1))
  1 - sum(outer.multi(f, L)) / choose(cards, kings) / choose(cards - kings, queens)
}
