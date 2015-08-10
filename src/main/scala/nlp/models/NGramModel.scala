package nlp.models

/**
 * Created by sarangis on 7/29/15.
 */

import akka.actor.{Actor, ActorRef, Props, ActorSystem}

class UnigramModel(txt: String) extends LanguageModel {
  val model = txt.split(" ")
}

class BigramModel(txt: String) extends LanguageModel {
  private val split_txt = txt.split(" ")
  val model = split_txt.view.zip(split_txt.view.takeRight(split_txt.length - 1))
}

class TrigramModel(txt: String) extends LanguageModel {
  private val split_txt = txt.split(" ")
  private val split_txt_one_shifted = split_txt.takeRight(split_txt.length - 1)
  private val split_txt_two_shifted = split_txt.takeRight(split_txt.length - 2)
  val model = split_txt.zip(split_txt_one_shifted.zip(split_txt_two_shifted)).map { case (x, (y, z)) => (x, y, z) }
}

case class ProcessStringMsg(string: String)
case class StringProcessedMsg(words: Integer)

class StringCounterActor extends Actor {
  def receive = {
    case ProcessStringMsg(string) => {
      val wordsInLine = string.split(" ").length
      sender ! StringProcessedMsg(wordsInLine)
    }

    case _ => println("Error: message not recognized")
  }
}

case class StartProcessFileMsg()

class WordCounterActor(filename: String) extends Actor {
  private var running = false
  private var totalLines = 0
  private var linesProcessed = 0
  private var result = 0
  private var fileSender: Option[ActorRef] = None

  def receive = {
    case StartProcessFileMsg() => {
      if (running) {
        println("Warning: duplicate start message received")
      }
      else {
        running = true
        fileSender = Some(sender)
        import scala.io.Source._
        fromFile(filename).getLines.foreach { line =>
          context.actorOf(Props[StringCounterActor]) ! ProcessStringMsg(line)
          totalLines += 1
        }
      }
    }

    case StringProcessedMsg(words) => {
      result += words
      linesProcessed += 1
      if (linesProcessed == totalLines) {
        fileSender.map(_ ! result)
      }
    }

    case _ => println("message not recognized")
  }
}

