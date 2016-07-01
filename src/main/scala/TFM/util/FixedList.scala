package TFM.util

/**
  * Created by diego on 1/07/16.
  */
import scala.collection._
import mutable.ListBuffer

class FixedList[A](max: Int) extends Traversable[A] {

  val list: ListBuffer[A] = ListBuffer()

  def append(elem: A) {
    if (list.size == max) {
      list.trimStart(1)
    }
    list.append(elem)
  }

  def foreach[U](f: A => U) = list.foreach(f)

  def full: Boolean = list.size == max

}
