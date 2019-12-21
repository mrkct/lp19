def is_palindrome(s: String): Boolean = {
    if (s.length() <= 1)
        return true;
    else
        return ( 
            s.charAt(0) == s.charAt(s.length() -1) && 
            is_palindrome(s.substring(1, s.length()-1))
        );
}

def is_anagram(word: String, dictionary: Array[String]): Boolean = {
    var sortedDict = dictionary.map(_.sorted);
    var sortedWord = word.sorted;
    return sortedDict.exists(_ == sortedWord);
}

def factors(x: Int, y: Int): List[Int] = {
    if (x <= 1) {
        return List();
    }
    if (x % y == 0) {
        return y::factors(x / y, 2);
    } else {
        return factors(x, y+1);
    }
}

def factors(x: Int): List[Int] = {
    return factors(x, 2);
}