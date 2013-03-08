object ProblemSet1 {
    def drop[A](n: Int, xs: List[A]): List[A] = {
    	var acc: List[A] = Nil
    	var m = 1
    	for (it <- xs) {
    		if(m < n) 
    		{
    			acc ::= it
    			m += 1
    		}
    		else m = 1
    	}
    	acc.reverse
    }

    def rotate[T](n: Int, xs: List[T]):List[T] = {
    	val l = xs.size
    	val j = xs span { _ == xs.head }
  		val i = if (xs.nonEmpty) n % l else 0
 	 	val (a, b) = xs.splitAt(if (i < 0) l + i else i)
  		b ::: a
    }
    
    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = 
        throw new UnsupportedOperationException
}