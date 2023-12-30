package problem

import problem.Status.*
import problem.Pulse.*

import javax.swing.text.Highlighter.Highlight
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


  val FlipFlopPattern = "(%[a-z]+)".r
  val ConjunctionPattern = "(&[a-z]+)".r
  val BroadcasterPattern = "(broadcaster)".r


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

  val cache = mutable.Map.empty[Int, mutable.Map[Pulse, Int]]
  val pulseCount = mutable.Map(High -> 0, Low -> 0)
  var round = 1
  private var workQueue: mutable.Queue[(PulseWithSource, String)] = _
  private var latestState: mutable.Map[String, Component] = _
  private var initialState: Map[String, Component] = _

  def toggleFlipFlop(ff: FlipFlop, pulse: Pulse): Seq[Pulse] = {
    val (updatedComponent, outputPulse) = (ff.status, pulse) match
      case (On, Low) => (ff.copy(status = Off), Some(Low))
      case (Off, Low) => (ff.copy(status = On), Some(High))
      case _ => (ff, None)
    latestState += ff.name -> updatedComponent
    outputPulse.toSeq.flatMap(op => {
      ff.destinations.map(d => {
        if (latestState.keySet.contains(d)) {
          workQueue.enqueue((PulseWithSource(op, ff.name), d))
        }
        op
      })
    })
  }

  def toggleConjunction(con: Conjunction, pws: PulseWithSource): Seq[Pulse] = {
    val lastStateOfTheConjunction = latestState(con.name).asInstanceOf[Conjunction]
    lastStateOfTheConjunction.sourceWithStatus += (pws.source -> pws.pulse)
    val outputPulse = if (lastStateOfTheConjunction.sourceWithStatus.exists(_._2 == Low)) High else Low
    con.destinations.map(d => {
      if (latestState.keySet.contains(d)) {
        workQueue.enqueue((PulseWithSource(outputPulse, con.name), d))
      }
      outputPulse
    })

  }

  def toggleBroadcaster(brd: Broadcaster, pulse: Pulse): Seq[Pulse] = {
    brd.destinations.map(d => {
      if (latestState.keySet.contains(d)) {
        workQueue.enqueue((PulseWithSource(pulse, brd.name), d))
      }
      pulse
    })
  }

  def toggleButton(btn: Button): Seq[Pulse] = {
    btn.destinations.map(d => {
      if (latestState.keySet.contains(d)) {
        workQueue.enqueue((PulseWithSource(Low, btn.name), d))
      }

      Low
    })
  }


  @tailrec
  def processQueue(): Unit = {
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
        processQueue()
      case None =>

  }


  def problem1(filePath: String): Long = {
    val input = readFile(filePath)
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
      processQueue()
    })
    pulseCount(High) * pulseCount(Low)
  }

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    0L
  }
}