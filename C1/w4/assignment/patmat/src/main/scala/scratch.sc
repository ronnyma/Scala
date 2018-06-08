import patmat.Huffman._


import scala.reflect.macros.whitebox

trait TestTrees {
  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
}

def times(chars: List[Char]): List[(Char, Int)] = {
  def loop(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = chars match {
    case Nil => acc
    case x :: xs => {
      loop(xs, condense((x, 1), acc))
    }
  }

  loop(chars, Nil)
}

def condense(tuple: (Char, Int), list: List[(Char, Int)]): List[(Char, Int)] = list match {
  case Nil => tuple :: list
  case x :: xs => {
    if (x._1 == tuple._1) (tuple._1, x._2 + 1) :: xs
    else x :: condense(tuple, xs)
  }
}


val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)

def insert(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
  case Nil => List(x)
  case y :: ys =>
    if (x._2 <= y._2) x :: xs else y :: insert(x, ys)
}

def insert2(x: Leaf, xs: List[Leaf]): List[Leaf] = xs match {
  case Nil => List(x)
  case y :: ys =>
    if (x.weight <= y.weight) x :: xs else y :: insert2(x, ys)
}

//def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
//  case Nil => Nil
//  case x :: xs => insert(x, makeOrderedLeafList(xs))
//}


//def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
//  def makeLeaf(tuple: List[(Char, Int)]): List[Leaf] = tuple match {
//    case Nil => Nil
//    case x :: xs => Leaf(x._1, x._2) :: makeLeaf(xs)
//  }
//
//  def makeOrderedTupleList(freqs: List[(Char, Int)]): List[(Char, Int)] = freqs match {
//    case Nil => Nil
//    case x :: xs => insert(x, makeOrderedTupleList(xs))
//  }
//
//  makeLeaf(makeOrderedTupleList(freqs))
//}


val l = times(List('a', 'd', 'b', 'a', 'b', 'c', 'b', 'd', 'f', 'd', 'k'))

val o = times(List('a', 'd', 'b', 'a', 'a', 'c', 'c', 'c', 'c', 'c', 'c', 'c'))




//insert2(Leaf('a', 2), s)


times(List('a', 'b', 'a', 'b', 'c'))


val chars = List('a', 'b', 'a', 'b', 'c', 'd', 'd')

chars.groupBy(c => c).mapValues(_.size).toList

val p = List(('b', 2), ('a', 1))

condense(('c', 1), p)

val s = makeOrderedLeafList(l)
val t = makeOrderedLeafList(o)
//
//
//def insert3(x: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
//  case Nil => List(x)
//  case y :: ys =>
//    if (weight(x) <= weight(y)) x :: xs else y :: insert3(x, ys)
//}
//def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
//
//  case Nil => Nil
//  case x :: Nil => x :: Nil
//  case x :: y :: xs => insert3(makeCodeTree(x, y), xs)
//}

combine(s)

//def until(singleton: List[CodeTree] => Boolean, codetree: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
//  if (singleton(trees)) trees
//  else until(singleton, combine)(combine(trees))
//
//
//until(singleton, combine)(makeOrderedLeafList(times(chars)))

//decode(frenchCode, secret)

//new TestTrees {decode(t1, List(0,1))}
