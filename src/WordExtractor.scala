/**
  * Created by kumar on 5/18/17.
  */


import scala.collection.mutable.Set

case class Word(word: String){
  require(word != null && word.length > 0)

}

trait Extractor {

  def extract(input: List[List[Char]]): Set[Word]
}


class WordExtractor extends Extractor{

  private val dictionary: List[Word] = Word("aaax") :: Word("aaab") :: Word("xyz") :: Word("pepms") :: Word("abcfed"):: Nil

  private val maxLength: Int = dictionary.maxBy(_.word.length()).word.length()

  var result: Set[Word] = Set.empty

  def extract(input: List[List[Char]]): Set[Word] = {
    require(input != null && input.size > 0)
    require(input.size > 0)

    result = Set.empty

    val rows: Int = input.size
    val cols: Int = input.head.size

    input.foreach{
      case inner: List[Char] => {

        inner.foreach{
          case c: Char => {
            startAt(c, input.indexOf(inner), inner.indexOf(c), rows, cols, input)
          }
        }
      }
    }

    result
  }

  private def startAt(c: Char, row: Int, column: Int, maxRows: Int, maxCols: Int, matrix:List[List[Char]]) = {

      val foundWords: Set[Word] = findWords("", row,column, maxRows, maxCols, matrix)
      result ++= foundWords
  }

  private def findWords(cur:String, row: Int, column: Int, maxRows: Int, maxCols: Int, matrix:List[List[Char]]): Set[Word] ={

      if (row >= maxRows || column >= maxCols || row < 0 || column < 0 || cur.length > maxLength)
        return Set.empty

      val curResult: Set[Word] = Set.empty

      val soFar: String = cur + matrix(row)(column)
      if(dictionary.contains(Word(soFar))){
        curResult.add(Word(soFar))
      }

      val loopSelf: Set[Word] = findWords(soFar , row, column, maxRows, maxCols, matrix)
      val moveRight: Set[Word] = findWords(soFar, row, column + 1, maxRows, maxCols, matrix)
      val moveLeft: Set[Word] = findWords(soFar , row, column - 1, maxRows, maxCols, matrix)

      val moveUp: Set[Word] = findWords(soFar , row - 1, column, maxRows, maxCols, matrix)
      val moveDown: Set[Word] = findWords(soFar , row + 1, column, maxRows, maxCols, matrix)

      curResult ++ loopSelf ++ moveRight ++ moveLeft ++ moveUp ++ moveDown
  }

}

object WordExtractor extends App{

  override def main(args: Array[String]): Unit ={

    val extractor: Extractor = new WordExtractor

    val array1: List[List[Char]] = List('a','g','o','i','c') :: List('d','e','r','u','f') :: List('m','p','n','b','h') :: List('s','t','e','f','w') :: Nil
    val result: Set[Word] = extractor.extract(array1)
    assert(result == Set(Word("pepms")))

    val array2: List[List[Char]] = List('a','b','c') :: List('d', 'e', 'f') :: List('g','h','i') :: Nil
    val result2: Set[Word] = extractor.extract(array2)
    assert(result2 == Set(Word("aaab"), Word("abcfed")))


    val array3: List[List[Char]] = List('a') :: List('x')  :: Nil
    val result3: Set[Word] = extractor.extract(array3)
    assert(result3 == Set(Word("aaax")))


  }


}


