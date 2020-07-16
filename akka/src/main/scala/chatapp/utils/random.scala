package chatapp.utils.random

import scala.collection.mutable


class SimpleRand(seed: Long) {
  var value: Long = seed

  def next(): Long = {
    nextLong()
  }

  def nextLong(): Long = {
    value = ((value * 1309) + 13849) & 65535
    value
  }

  def nextInt(max: Int = 0) = {
    if (max == 0) {
      nextLong().asInstanceOf[Int]
    } else {
      nextLong().asInstanceOf[Int] % max
    }
  }

  def shuffle[A](array: mutable.ArrayBuffer[A]) = {
    var i = array.length

    while (i > 1) {
      i -= 1
      val ceil = i
      val randI = nextInt(ceil)

      val tmp = array(ceil)
      array(ceil) = array(randI)
      array(randI) = tmp
    }
  }
}

class DiceRoll(rand: SimpleRand) {
  def apply(): Int =
    rand.nextInt(100)
}
