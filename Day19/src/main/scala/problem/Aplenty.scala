package problem

import scala.collection.mutable
import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.{break, breakable}

final case class Rule(partCategory: Char = 0, sign: Char = 0, boundary: Int = 0, action: String)

object Aplenty {

  private def extractRuleDetails(str: String) = {
    str.split(Array('<', '>')) match
      case Array(partCategory, boundary) => (partCategory.head, boundary.toInt)
  }

  private def extractRules(str: String) = {
    str.split(",").map(_.split(":") match {
      case Array(actionStr) => Rule(action = actionStr)
      case Array(ruleStr, actionStr) =>
        val (partCategory, boundary) = extractRuleDetails(ruleStr)
        Rule(partCategory, "<".r.findFirstIn(ruleStr).getOrElse(">").head, boundary, actionStr)
    }).toSeq
  }

  private def readFile(filePath: String) = {
    Using(Source.fromFile(filePath)) { file =>
      val input = file
        .getLines().mkString("\n").split("\n\n")
      val workflows =
        input.head.split("\n").map(s => {
          s.split(Array('{', '}')) match {
            case Array(workFlowName, rulesString) => workFlowName -> extractRules(rulesString)
          }
        }).toMap
      val parts = input.last.split("\n").map(_.stripPrefix("{").stripSuffix("}").split(',')
        .map(_.split('=') match
          case Array(partCategory, value) => partCategory.head -> value.toInt).toMap
      ).toSeq
      (workflows, parts)
    }.get

  }

  private def executeRule(r: Rule, part: Map[Char, Int]) = {
    r.sign match
      case '>' if part(r.partCategory) > r.boundary => Some(r.action)
      case '>' => None
      case '<' if part(r.partCategory) < r.boundary => Some(r.action)
      case '<' => None
      case _ => Some(r.action)
  }

  private def applyWorkFlowFunc(workflows: Map[String, Seq[Rule]])(part: Map[Char, Int]) = {
    var nextAction = "in"
    while (nextAction != "R" && nextAction != "A") {
      val nextWorkflow = workflows(nextAction)
      breakable {
        nextWorkflow.foreach(rule => {
          executeRule(rule, part) match
            case Some(value) =>
              nextAction = value
              break
            case None =>
              nextAction = ""

        })
      }
    }
    if (nextAction == "A") true else false
  }

  def problem1(filePath: String): Int = {
    val (workflows, parts) = readFile(filePath)
    val applyWorkFlow = applyWorkFlowFunc(workflows)
    parts.filter(applyWorkFlow).map(_.values.sum).sum
  }

  def applyFilterFunction(r: Rule)(item: Int) = {
    r.sign match
      case '>' => item > r.boundary
      case '<' => item < r.boundary
      case _ => true
  }

  def problem2(filePath: String): Long = {
    val (workflows, _) = readFile(filePath)
    val start = Map(
      'x' -> Seq.range(1, 4001),
      'm' -> Seq.range(1, 4001),
      'a' -> Seq.range(1, 4001),
      's' -> Seq.range(1, 4001)
    )
    val stack = mutable.Stack[(Map[Char, Seq[Int]], String)]((start, "in"))
    val finalList = mutable.Stack.empty[(Map[Char, Seq[Int]], String)]
    while (stack.nonEmpty) {
      var remaining = stack.pop()
      val workflow = workflows(remaining._2)
      workflow.foreach(r => {
        val applyFilter = applyFilterFunction(r)
        val next = remaining.copy(_1 = remaining._1.updatedWith(r.partCategory)(v => v.map(_.filter(applyFilter))), _2 = r.action)
        r.action match
          case "A" => finalList.push(next)
          case "R" =>
          case _ => stack.push(next)
        remaining = remaining.copy(_1 = remaining._1.updatedWith(r.partCategory)(v => v.map(_.filterNot(applyFilter))))
      })

    }
    finalList.map(_._1.values.map(_.size.toLong).product).sum
  }
}