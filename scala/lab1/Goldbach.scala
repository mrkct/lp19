def is_prime(n: Int): Boolean = {
    def prime(n: Int, x: Int): Boolean = {
        if (x > n/2) return true;
        if (n % x == 0) return false;
        
        return prime(n, x+1);
    };
    return prime(n, 2);
}

def goldbach(n: Int): (Int, Int) = {
    def gb(x: Int): (Int, Int) = {
        if (is_prime(x) && is_prime(n-x))
            return (x, n-x);
        else return gb(x+1);
    }

    return gb(1);
}

def goldbach_list(n: Int, m: Int): List[(Int, Int)] = {
    return {for (x <- List.range(n, m); if x % 2 == 0) yield goldbach(x)};
}