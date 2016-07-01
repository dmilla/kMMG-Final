package TFM.util

import scala.collection.mutable.ListBuffer

/**
  * Created by diego on 7/05/16.
  */

class MarkovChain[S](transitionMap: Map[ListBuffer[S], MarkovTransitionSet[S]]) {

  /**
    * Default constructors creates an empty Markov Chain
    */

  def this() = this(Map[ListBuffer[S], MarkovTransitionSet[S]]())

  // Create a new MarkovChain.
  def addTransition(prevState: ListBuffer[S], nextState: S) = {
    val transitions =
      if (transitionMap.contains(prevState)) transitionMap(prevState)
      else new MarkovTransitionSet[S]()

    val newTransitions = transitions.addTransition(nextState)
    new MarkovChain(transitionMap.updated(prevState, newTransitions))
  }

  def transitionProbability(prevStates: ListBuffer[S], nextState: S) = {
    transitionMap.get(prevStates) match {
      case Some(transitionSet) => transitionSet.probabilityFor(nextState)
      case None => 0.toDouble
    }
  }

  def transitionsFor(state: ListBuffer[S]) = {
    transitionMap.get(state) match {
      case Some(transitionSet) => transitionSet.toList
      case None => List[(S, Double)]()
    }
  }

  def states() = {
    transitionMap.keys
  }

}

class MarkovTransitionSet[S](transitionCounter: Map[S, Int]) {

  def this() = this(Map[S, Int]())

  def apply(state: S) = probabilityFor(state)

  // Create a new MarkovTransitionSet.
  def addTransition(state: S) = {
    val count = countFor(state)
    new MarkovTransitionSet[S](
      transitionCounter.updated(state, count+1)
    );
  }

  def countFor(state: S) = {
    if(transitionCounter.contains(state))
      transitionCounter(state);
    else
      0
  }

  def totalCount(): Double = {
    val counts = transitionCounter.values
    val i = counts.foldLeft(0)((a, b) => a + b)
    i
  }

  def probabilityFor(state: S) = {
    countFor(state).toDouble / this.totalCount
  }

  def toList() = {
    transitionCounter.toList.map(tup => (tup._1, tup._2.toDouble / totalCount))
  }

}