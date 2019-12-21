def intersect(a: List[Any], b: List[Any]): List[Any] = {
    return for (x <- a; y <- b; if x == y) yield x; 
}

def symmetric_difference(a: List[Any], b: List[Any]): List[Any] = {
    return (for (x <- a; if !b.contains(x)) yield x) ++ (for(y <- b; if !a.contains(y)) yield y);
}