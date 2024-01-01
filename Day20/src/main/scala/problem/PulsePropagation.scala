package problem

import extensions.SeqExtensions.*
import extensions.StringExtensions.*
import problem.Pulse.*
import problem.Status.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

private enum Pulse:
  case Low
  case High

private enum Status:
  case On
  case Off

trait Component {

  def name: String

  def destinations: Seq[String]
}

final case class PulseWithSource(pulse: Pulse, source: String)

final case class FlipFlop(name: String, destinations: Seq[String], status: Status) extends Component

final case class Conjunction(name: String, destinations: Seq[String], sourceWithStatus: mutable.Map[String, Pulse]) extends Component


final case class Broadcaster(name: String, destinations: Seq[String]) extends Component

final case class Button() extends Component {
  def name = "button"

  def destinations: Seq[String] = Seq("broadcaster")
}

object PulsePropagation {


  private val FlipFlopPattern = "(%[a-z]+)".r
  private val ConjunctionPattern = "(&[a-z]+)".r
  private val BroadcasterPattern = "(broadcaster)".r


  private def readFile(filePath: String): Seq[(String, Component)] = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split(" -> ") match
        case Array(left, right) =>
          left match
            case FlipFlopPattern(value) => value.tail -> FlipFlop(value.tail, right.split(",").map(_.trim).toSeq, Off)
            case ConjunctionPattern(value) => value.tail -> Conjunction(value.tail, right.split(",").map(_.trim).toSeq, mutable.Map.empty)
            case BroadcasterPattern(value) => value -> Broadcaster(value, right.split(",").map(_.trim).toSeq)
      ).toSeq
  }.get

  private val pulseCount = mutable.Map(High -> 0, Low -> 0)
  private var round = 0
  private var rxLowCount = 0
  private var rxCount = 0
  private var lastStatus: mutable.Map[(String, String), Pulse] = _
  private var lastHighCount: mutable.Map[(String, String), Seq[Long]] = _

  private var sourceDestinationPairs: mutable.Set[(String, String)] = _
  private var workQueue: mutable.Queue[(PulseWithSource, String)] = _
  private var latestState: mutable.Map[String, Component] = _
  private var initialState: Map[String, Component] = _

  private def toggleFlipFlop(ff: FlipFlop, pulse: Pulse): Seq[Pulse] = {
    val (updatedComponent, outputPulse) = (ff.status, pulse) match
      case (On, Low) => (ff.copy(status = Off), Some(Low))
      case (Off, Low) => (ff.copy(status = On), Some(High))
      case _ => (ff, None)
    latestState += ff.name -> updatedComponent
    outputPulse.toSeq.flatMap(op => {
      ff.destinations.map(d => {

        if (sourceDestinationPairs.contains((ff.name, d))) {
          lastStatus((ff.name, d)) = op
        }
        if (latestState.keySet.contains(d)) {
          workQueue.enqueue((PulseWithSource(op, ff.name), d))
        }
        op
      })
    })
  }

  private def toggleConjunction(con: Conjunction, pws: PulseWithSource): Seq[Pulse] = {
    val lastStateOfTheConjunction = latestState(con.name).asInstanceOf[Conjunction]
    lastStateOfTheConjunction.sourceWithStatus += (pws.source -> pws.pulse)
    val outputPulse = if (lastStateOfTheConjunction.sourceWithStatus.exists(_._2 == Low)) High else Low
    con.destinations.map(d => {
      if (d == "rx") {
        rxCount += 1
      }
      if (d == "rx" && outputPulse == Low) {
        rxLowCount += 1
      }
      if (sourceDestinationPairs.contains((con.name, d))) {
        lastStatus((con.name, d)) = outputPulse
      }
      if (latestState.keySet.contains(d)) {
        workQueue.enqueue((PulseWithSource(outputPulse, con.name), d))
      }
      outputPulse
    })

  }

  private def toggleBroadcaster(brd: Broadcaster, pulse: Pulse): Seq[Pulse] = {
    brd.destinations.map(d => {
      if (latestState.keySet.contains(d)) {
        if (sourceDestinationPairs.contains((brd.name, d))) {
          lastStatus((brd.name, d)) = pulse
        }
        workQueue.enqueue((PulseWithSource(pulse, brd.name), d))
      }
      pulse
    })
  }

  private def toggleButton(btn: Button): Seq[Pulse] = {
    btn.destinations.map(d => {
      if (latestState.keySet.contains(d)) {
        workQueue.enqueue((PulseWithSource(Low, btn.name), d))
      }
      Low
    })
  }

  @tailrec
  private def processQueueForCount(): Unit = {
    workQueue.dequeueFirst(_ => true) match
      case Some(value) =>
        val newStatus = (value._1, latestState(value._2)) match
          case (p: PulseWithSource, v: FlipFlop) => toggleFlipFlop(v, p.pulse)
          case (p: PulseWithSource, v: Conjunction) => toggleConjunction(v, p)
          case (p: PulseWithSource, v: Broadcaster) => toggleBroadcaster(v, p.pulse)
          case (_: PulseWithSource, v: Button) =>
            toggleButton(v)
          case (_: PulseWithSource, _) => Seq.empty
        newStatus.foreach(np => pulseCount += (np -> (pulseCount(np) + 1)))
        processQueueForCount()
      case None =>

  }


  @tailrec
  private def processQueue(): Unit = {
    workQueue.dequeueFirst(_ => true) match
      case Some(value) =>
        (value._1, latestState(value._2)) match
          case (p: PulseWithSource, v: FlipFlop) => toggleFlipFlop(v, p.pulse)
          case (p: PulseWithSource, v: Conjunction) => toggleConjunction(v, p)
          case (p: PulseWithSource, v: Broadcaster) => toggleBroadcaster(v, p.pulse)
          case (_: PulseWithSource, v: Button) =>
            toggleButton(v)
          case (_: PulseWithSource, _) =>
        processQueue()
      case None =>

  }


  def problem1(filePath: String): Long = {
    val inputMap = readFile(filePath).toMap
    workQueue = mutable.Queue.empty[(PulseWithSource, String)]
    latestState = mutable.Map.from(inputMap + ("button" -> Button()))
    initialState = Map.empty[String, Component]
    latestState.filter(_._2.isInstanceOf[Conjunction]).map({ case (_, c: Conjunction) =>
      c.sourceWithStatus ++= inputMap.filter(_._2.destinations.contains(c.name)).map(_._1 -> Low)
    })
    initialState = latestState.toMap
    (1 to 1000).foreach(* => {
      workQueue.enqueue((PulseWithSource(Low, ""), "button"))
      processQueueForCount()
    })
    pulseCount(High) * pulseCount(Low)
  }


  def problem2(filePath: String): Long = {
    val inputMap = readFile(filePath).toMap
    workQueue = mutable.Queue.empty[(PulseWithSource, String)]
    latestState = mutable.Map.from(inputMap + ("button" -> Button()))
    initialState = Map.empty[String, Component]
    latestState.filter(_._2.isInstanceOf[Conjunction]).map({ case (_, c: Conjunction) =>
      c.sourceWithStatus ++= inputMap.filter(_._2.destinations.contains(c.name)).map(_._1 -> Low)
    })
    val cntToCheck = 100
    initialState = latestState.toMap
    sourceDestinationPairs = mutable.Set("hh", "ck", "kz", "ns").flatMap(dest => latestState.filter(_._2.destinations.contains(dest)).map(s => (s._1, dest)))
    lastHighCount = mutable.Map.empty
    while (sourceDestinationPairs.nonEmpty) {
      rxLowCount = 0
      rxCount = 0
      lastStatus = mutable.Map.empty
      workQueue.enqueue((PulseWithSource(Low, ""), "button"))
      processQueue()
      round += 1
      lastStatus.filter(p => p._2 == High).keys.foreach(p => {
        lastHighCount(p) = lastHighCount.getOrElse(p, Seq.empty) :+ round
        lastStatus.remove(p)
      })

      sourceDestinationPairs --= lastHighCount.filter(p => p._2.length == cntToCheck).keySet
    }

    (0 until cntToCheck).foreach(i => println(lastHighCount.toSeq.sortBy(_._1).map(e => e._2(i).toString.lpad(' ', 5)).mkString(",, ")))

    final case class SeriesDetails(offset: Long, cycleValue: Long, modValue: Long)
    val rules = lastHighCount.view.mapValues(s => {
      val first = s.head
      val diffList = s.sliding(2).map { case Seq(left, right) => right - left }.toSeq
      val indexOfFirstDiff = diffList.sliding(2).indexWhere({ case Seq(left, right) => right != left })
      val cycle = diffList.take(indexOfFirstDiff + 2)
      SeriesDetails(first, cycle.sum, cycle.head)
    }).values.toSeq
    rules.filter(_.cycleValue % 2 != 0).map(_.cycleValue).lcm
  }
}