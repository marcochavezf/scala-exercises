package forcomp

object Anagrams extends AnagramsInterface {

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
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val occurrencesPerChar = w.toLowerCase.toList.groupBy((c: Char) => c).toList.sortBy(_._1)
    occurrencesPerChar map { case (c: Char, list: List[Char]) => (c, list.length) }
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => List()
    case head :: next => next.foldLeft(wordOccurrences(head))((acc: Occurrences, w: Word) => {
      val getAccOcc = acc.toMap withDefaultValue 0
      val wordOcc = wordOccurrences(w)
      val sumLeftWordAcc = (for { (char, occ) <- wordOcc } yield (char, occ + getAccOcc(char)))
      (getAccOcc ++ sumLeftWordAcc).toList.sorted
    })
  }
  
  

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences(_))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences get wordOccurrences(word) match {
    case None => List()
    case Some(value) => value
  }

  def wordAnagrams(occurrences: Occurrences): Sentence = (dictionaryByOccurrences withDefaultValue List())(occurrences)

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
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case (char, occ) :: next => {
      val nextCombinations = combinations(next)
      (for {
        x <- 0 to occ
        list <- nextCombinations
      } yield if (x == 0) list else (char, x) :: list).toList
    }
  }
  
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
    // println(s"x: ${ x }, y: ${ y }")
    def subtractOcc(z: Occurrences, y: (Char, Int)): Occurrences = {
      val (char, occ) = y
      val zMap = z.toMap withDefaultValue 0
      // println(s"zMap: ${ zMap }, char: ${ char }")
      if (!zMap.contains(char)) {
        println(s"not contained! zMap: ${ zMap }, char: ${ char }")
        Nil
      }
      val newOcc = zMap.apply(char) - occ
      (if (newOcc == 0) {
        zMap - char
      } else if(newOcc < 0) {
        println(s"newOcc is less than zero ${ newOcc }")
        Nil
      } else {
        zMap.updated(char, zMap.apply(char) - occ)
      }).toList
    }
    y.foldLeft(x)(subtractOcc).sortBy(_._1)
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
   *```
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
    def getPossibleAnagrams(occ: Occurrences): List[Sentence] = {
      occ match {
        case Nil => List(Nil)
        case _ => (for {
          comb <- combinations(occ)
          anagram <- wordAnagrams(comb)
          nextAnagrams <- getPossibleAnagrams(subtract(occ, comb))
        } yield anagram :: nextAnagrams).toList
      }
    }
    val ocurrences = sentenceOccurrences(sentence)
    getPossibleAnagrams(ocurrences).filter(sentenceOccurrences(_).toSet == ocurrences.toSet)
  }
  
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}

object Main extends App {
  import Anagrams._

  // println(s"test: ${ Nil.toMap }")
  // println(s"test: ${ Nil.toList }")
  // println(s"test empty list: ${ for(a <- Nil) yield a }")
  // println(s"test empty list: ${ List(1,2,3).foldLeft(List(3,4,5))((z, x) => Nil) }")
  // println(s"test same sets: ${ List(1,2,3).toSet == List(3,2,1).toSet }")

  val sentence = List("Yes", "man")

  // val anagrams = getAnagrams(filteredCombs)
  // val anagrams = getAnagrams(combs)

  // println(s"wordAnagrams: ${ wordAnagrams("Yes") }")
  // println(s"occurences: ${ ocurrences }")
  // println(s"anagrams: ${ sentence map wordAnagrams }")
  // println(s"occurences: ${ wordAnagrams(ocurrences) }")
  // println(s"combs length: ${ combs.length }")
  // println(s"filtered combs length: ${ filteredCombs.length }")
  // println(s"combs: ${ combs mkString s"\n" }")
  // println(s"filtered combs:\n ${ filteredCombs mkString s"\n" }")
  // println(s"sentences filtered: ${ filteredCombs map wordAnagrams mkString s"\n" }")
  // println(s"sentenceAnagrams: ${ sentenceAnagrams(sentence) mkString s"\n" }")

  // occurences: List((a,1), (e,1), (m,1), (n,1), (s,1), (y,1))
  // List((a,1), (e,1), (m,1), (n,1), (s,1)) -> List(manes, means, names) // can't be an anagram of sentence
  val anas = List(
    List("en", "as", "my"),
    List("en", "my", "as"),
    List("man", "yes"),
    List("men", "say"),
    List("as", "en", "my"),
    List("as", "my", "en"),
    List("sane", "my"),
    List("Sean", "my"),
    List("my", "en", "as"),
    List("my", "as", "en"),
    List("my", "sane"),
    List("my", "Sean"),
    List("say", "men"),
    List("yes", "man")
  )

  // println(s"are the same: ${ Set(1,2,3) == Set(3,2,1) }") // true
  println(s"are the same: ${ anas.toSet == sentenceAnagrams(sentence).toSet }")
}