package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  // the  groupby funciton takes a mapper p => key
  def wordOccurrences(w: Word): Occurrences = w.groupBy((p: Char) => p.toLower).toList.map(p => (p._1, p._2.length)).sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatMap(p => p).mkString).sortBy(_._1)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((p: String) => wordOccurrences(p))

  def printDictionaryByOccurences(): Unit = dictionaryByOccurrences.map(p => (println(p._1, p._2)))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val wordOccur: Occurrences = wordOccurrences(word)
    occurAnagrams(wordOccur)
  }

  def occurAnagrams(occur: Occurrences): List[Word] = {
    dictionaryByOccurrences get occur match {
      case Some(listWord)    => listWord
      case None              => Nil
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val tmpListSubKeyCount: List[List[(Char, Int)]] = occurrences.map(p => subKeyCount(p))
    val listStart: List[Occurrences] = Nil

    def twoListCombination(restList: List[List[(Char, Int)]],
                           currentList: List[Occurrences]): List[Occurrences] = restList match {
      case Nil => {
        currentList
      }
      case x :: xs => currentList match {
        case Nil => {
          var nextCurrentList: List[Occurrences] = List()
          val head: List[(Char, Int)] = restList.head
          for (i <- Range(0, head.length)) {
            if (head(i)._2 != 0) {
              val tempElement: List[(Char, Int)] = List(head(i))
              nextCurrentList = tempElement.sortBy(_._1) :: nextCurrentList
            }
          }
          nextCurrentList = Nil :: nextCurrentList
          twoListCombination(restList.tail, nextCurrentList)
        }
        case y :: ys => {
          var nextCurrentList: List[Occurrences] = List()
          val head: List[(Char, Int)] = restList.head
          for (i <- Range(0, head.length)) {
            for (j <- Range(0, currentList.length)) {
              if (head(i)._2 == 0) {
                val tempElement: List[(Char, Int)] = currentList(j)
                nextCurrentList = tempElement.sortBy(_._1) :: nextCurrentList
              } else {
                val tempElement: List[(Char, Int)] = head(i) :: currentList(j)
                nextCurrentList = tempElement.sortBy(_._1) :: nextCurrentList
              }
            }
          }
          twoListCombination(restList.tail, nextCurrentList)
        }
      }
    }

    if (occurrences == Nil)  List(Nil)
    else twoListCombination(tmpListSubKeyCount, listStart)
  }

  /*
  (char, count) => (char, 0) (char, 1) (char,2) ...
   */
    private def subKeyCount(keyCount: (Char, Int)): List[(Char, Int)] = Range(0,keyCount._2+1).map(p => (keyCount._1, p)).toList

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {

    def subtractHelper(current: Occurrences, rest: Occurrences): Occurrences = rest match {
      case Nil                 => current
      case x :: xs             => {
        val head: (Char, Int) = rest.head

        def subtractOneElement(z: Occurrences, one: (Char, Int)): Occurrences = {
          if (z.isEmpty) z
          else if (z.head._1 == one._1) {
            if (z.head._2 - one._2 == 0) z.tail
            else if (z.head._2 - one._2 > 0) (z.head._1, z.head._2 - one._2) :: z.tail
            else throw new Error("Subtractor has char count larger than x")
          }
          else z.head :: subtractOneElement(z.tail, one)
        }

        val afterCurrentHead: Occurrences = subtractOneElement(current, head)
        subtractHelper(afterCurrentHead, rest.tail)
      }
    }

    if (y.isEmpty) x
    else subtractHelper(x, y)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val sentenceOccur: Occurrences = sentenceOccurrences(sentence)
    if (sentence.isEmpty) List(Nil)
    else sentenceAnagramsHelper(sentenceOccur)
  }

    /*
    基本思路就是把问题拆解，我把问题拆成  多个words 和 剩余的句子组合，生成剩余的句子，上一层再做这样的组合，就可以通过recurssion 实现
     */
    private def wordSentenceCombiner(words: List[Word], sentences: List[Sentence]): List[Sentence] = {
      var emptySentences: List[Sentence] = List()
      for ( i <- Range(0, words.length) ) {
        for ( j <- Range(0, sentences.length) ) {
          val newSentence: Sentence = words(i) :: sentences(j)
          emptySentences = newSentence :: emptySentences
        }
      }
      emptySentences
    }

    private def sentenceAnagramsHelper(restOccur: Occurrences): List[Sentence] = {
      if (restOccur.isEmpty) List(Nil)

      val subOccur: List[Occurrences] = combinations(restOccur)
      var resultContainer: List[Sentence] = List()

      for ( i<- Range(0, subOccur.length)) {
        val words: List[Word] = occurAnagrams(subOccur(i))
        val tmpRestOccr: Occurrences = subtract(restOccur, subOccur(i))
        var tmpList: List[Sentence] = List()
        if (words.isEmpty) {
        } else if (tmpRestOccr.isEmpty) {
          tmpList = wordSentenceCombiner(words, List(Nil))
        } else {
          tmpList = wordSentenceCombiner(words, sentenceAnagramsHelper(tmpRestOccr))
        }
        resultContainer = tmpList ++ resultContainer
      }
      resultContainer
    }

  def main(args: Array[String]): Unit = {
    val a = List("hello","hi")
    println(getClass)
    println(a.mkString("/"))
    val testStr: String = "hellOworld"
    //println(wordOccurrences(testStr))
    val sentence: Sentence = List("hello", "World")
    //println(sentenceOccurrences(sentence))
    //println(sentence.flatMap(p => wordOccurrences(p)))
    //printDictionaryByOccurences()

    //println(wordAnagrams("elHlo0ooooo"))
    //println(subKeyCount(('a', 5)))
    val or1: Occurrences = wordOccurrences("hellomoy")
    val tmp = or1.map(p => subKeyCount(p))
    //println(tmp)

    //println(combinations(or1))

    // combination test
    val testCom = List(('a', 2), ('b', 2))
    val testCom1 = List(('a', 2), ('b', 2))
    println(subtract(testCom, testCom1))

    // wordSentenceCombiner test
    val words: List[Word] = List("helo", "my")
    val sents: List[Sentence] = List(List("you","ol"), List("U", "kniw"))
    //println(wordSentenceCombiner(words, sents))

    // sentenceAnagramHelper test
    val testOccur: Occurrences = List(('r',1), ('e',1), ('x',1))
    //println(sentenceAnagramsHelper(testOccur))

    val sentenceTest = List("Linux", "rulez")
    //println(sentenceAnagrams(sentenceTest))
  }
}
