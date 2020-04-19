package patmat

import javafx.util.Pair

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    def weightAcc(acc: Int, tree: CodeTree): Int =
      tree match {
        case Leaf(char, weight) => acc + weight
        case Fork(left, right, chars, weight) => acc + weightAcc(0, left) + weightAcc(0, right)
      }

    weightAcc(0, tree)
  }

  def chars(tree: CodeTree): List[Char] = {
    def charsAcc(acc: List[Char], tree: CodeTree): List[Char] = tree match {
      case Leaf(char, weight) => char :: acc
      case Fork(left, right, chars, weight) => charsAcc(charsAcc(acc, left), right)
    }

    charsAcc(Nil, tree).reverse
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   * times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   * List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def times(acc: List[(Char, Int)], chars: List[Char]): List[(Char, Int)] = chars match {
      case Nil => acc
      case x :: xs =>
        val option = acc.find(p => p._1 == x)
        if (option.isDefined) {
          times((x, option.get._2 + 1) :: acc.filter(p => p != option.get), xs)
        } else {
          times((x, 1) :: acc, xs)
        }
    }

    times(List(), chars)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def isort(charWithWeight: (Char, Int), acc: List[Leaf]): List[Leaf] = {
      acc match {
        case Nil => List(Leaf(charWithWeight._1, charWithWeight._2))
        case x :: xs => if (x.weight < charWithWeight._2)
          x :: isort(charWithWeight, xs)
        else
          Leaf(charWithWeight._1, charWithWeight._2) :: acc
      }
    }

    def sort(freqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = {
      freqs match {
        case Nil => acc
        case x :: xs => sort(xs, isort(x, acc))
      }
    }

    sort(freqs, Nil)
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x :: Nil => true
    case default => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def combineTwoLeaves(first: Leaf, second: Leaf): Fork = {
      if (first.weight < second.weight) {
        Fork(first, second, List(first.char, second.char), first.weight + second.weight)
      } else {
        Fork(second, first, List(second.char, first.char), first.weight + second.weight)
      }
    }

    def combineTwoForks(first: Fork, second: Fork): Fork = {
      if (first.weight < second.weight) {
        Fork(first, second, first.chars ::: second.chars, first.weight + second.weight)
      } else {
        Fork(second, first, second.chars ::: first.chars, first.weight + second.weight)
      }
    }

    def combineLeafAndFork(leaf: Leaf, fork: Fork): Fork = {
      if (leaf.weight < fork.weight) {
        Fork(leaf, fork, leaf.char :: fork.chars, leaf.weight + fork.weight)
      } else {
        Fork(fork, leaf, fork.chars.appended(leaf.char), leaf.weight + fork.weight)
      }
    }

    def combine(acc: List[CodeTree], trees: List[CodeTree]): List[CodeTree] = {
      trees match {
        case Nil => acc
        case x :: Nil => acc.appended(x)
        case (x: Leaf) :: (y: Leaf) :: xs => combine(acc appended combineTwoLeaves(x, y), xs)
        case (x: Fork) :: (y: Fork) :: xs => combine(acc appended combineTwoForks(x, y), xs)
        case (x: Fork) :: (y: Leaf) :: xs => combine(acc appended combineLeafAndFork(y, x), xs)
        case (x: Leaf) :: (y: Fork) :: xs => combine(acc appended combineLeafAndFork(x, y), xs)
      }
    }

    combine(Nil, trees)
  }

  /**
   * This function will be called in the following way:
   *
   * until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) {
      trees
    } else {
      until(done, merge)(merge(trees))
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decode(acc: List[Char], treeHead: CodeTree, node: CodeTree, remainder: List[Bit], pointer: List[Bit]): List[Char] = {
      node match {
        case Leaf(char, weight) => decode(acc appended char , treeHead, treeHead, remainder, Nil)
        case Fork(left, right, chars, weight) => remainder match {
          case Nil => acc
          case ::(head, next) => if (head == 0)
            decode(acc, treeHead, left, next, pointer appended head)
          else
            decode(acc, treeHead, right, next, pointer appended head)
        }
      }
    }

    decode(Nil, tree, tree, bits, Nil)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   **/
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def nodeContains(node: CodeTree, char: Char): Boolean = node match {
      case leaf: Leaf => leaf.char == char
      case fork: Fork => fork.chars.contains(char)
    }
    def encodeNext(acc: List[Bit], node: CodeTree)(char: Char): List[Bit] = {
      node match {
        case l: Leaf => if (l.char == char) acc else throw new IllegalStateException("Dig to leaf that isn't matched")
        case f: Fork => if (nodeContains(f.left, char)) encodeNext(acc appended 0, f.left)(char)
          else if (nodeContains(f.right, char)) encodeNext(acc appended 1, f.right)(char)
          else acc
      }
    }

    def encode(acc: List[Bit], tree: CodeTree, text: List[Char]): List[Bit] = text match {
      case Nil => acc
      case x::xs => encode(encodeNext(acc, tree)(x), tree, xs)
    }
    encode(Nil, tree, text)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(p => p._1 == char).get._2


  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def convert(acc: CodeTable, bits: List[Bit], tree: CodeTree): CodeTable = {
      tree match {
        case leaf: Leaf => acc appended (leaf.char, bits)
        case f: Fork => convert(convert(acc, bits appended 0, f.left), bits appended 1, f.right)
      }
    }
    convert(Nil, Nil, tree)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def mergeCodeTables(acc: CodeTable, merging: CodeTable): CodeTable = merging match {
      case Nil => acc
      case ::(head, next) => mergeCodeTables(head::acc, next)
    }
    mergeCodeTables(a, b)
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncode(acc: List[Bit], table: CodeTable, text: List[Char]): List[Bit] = text match {
      case ::(head, next) => quickEncode(acc ::: codeBits(table)(head), table, next)
      case Nil => acc
    }
    quickEncode(Nil, convert(tree), text)
  }
}

object Huffman extends Huffman
