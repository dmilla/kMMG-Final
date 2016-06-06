package TFM

/**
  * Created by diego on 31/05/16.
  */


class StringSplitToTuple(s: String) {
  def splitToTuple(regex: String): (String, String) = {
    s.split(regex) match {
      case Array(str1, str2) => (str1, str2)
    }
  }
}

object SplitToTuple {
  implicit def splitToTuple(regex: String) = new StringSplitToTuple(regex)
}