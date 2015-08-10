import akka.actor.{Props, ActorSystem}
import nlp.tokenize.Regex

/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Satyajit Sarangi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

/**
 * Created by sarangis on 7/12/15.
 */

import nlp.models.{StartProcessFileMsg, WordCounterActor, TrigramModel}

import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.ask
import akka.dispatch.ExecutionContexts._


object scalaLearnApp extends App {
  implicit val ec = global

  def main(): Unit = {
    /*
    val txt = "This is a test. Please do not disregard this message"
    val trigram_model = new TrigramModel(txt)
    println(trigram_model.model.mkString)
    */

    val filename = "/Volumes/Data/tmp/pg74.txt"
    val system = ActorSystem("System")
    val actor = system.actorOf(Props(new WordCounterActor(filename)))
    implicit val timeout = Timeout(25 seconds)
    val future = actor ? StartProcessFileMsg()
    future.map { result =>
      println("Total number of words: " + result)
      system.shutdown
    }
  }

  main()
}
