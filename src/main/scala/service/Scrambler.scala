package com.chrisworks
package service

import util.FileUtil.ObjectRepresentation

import java.util.UUID
import scala.util.Random

trait Scrambler {

  /**
   * This method takes a filePath, reads it's json and scrambles some sensitive data like name and id, in it
   *
   * @param filePath This is the path to the json file whose sensitive data need to be scrambled
   */
  def scramble(filePath: String): Unit
}

object Scrambler {

  val liveScrambler: Scrambler = ScramblerImpl()

  //These are the keys of interest, whose value we plan to scramble in a consistent manner
  val keysOfInterest = List("name", "id")
  val matchRegex: String = keysOfInterest.mkString(".*(", "|", ")$") //sample => ".*(name|id)$"

  /**
   * This is the main logic of the scrambler/anonymize util. We will follow the logic that:
   * 1. Any key ending with name must be scrambled to something else which must be unique
   * 2. Anything ending with the word 'id' must be replaced with a consistent UUID, so as to maintain relationship
   */
  val anonymizeInput: ObjectRepresentation => Option[ObjectRepresentation] = input => {

    /* Here I store ids and name, so as to track them to maintain consistent
      Hence a name "Chris Eteka" within a file that is replaced once by "RFHYRT YHGFH" will stay consistent round the file
      Also, an id = 1, which has been replaced once by "ABC" will stay consistent round the file
     */
    var store: Map[String, String] = Map.empty

    def someRandomName: String = {
      val characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      val random = Random
      var name = ""
      var surname = ""
      (1 to 5).foreach(_ => {
        name = name.appended(characters.charAt(random.nextInt(characters.length)))
        surname = surname.appended(characters.charAt(random.nextInt(characters.length)))
      })
      s"$surname-$name"
    }

    val produceAnonymousValue: (String, Boolean) => String = (oldValue, isName) => {
      val substituteExist = store.keys.exists(_.equalsIgnoreCase(oldValue))
      if (!substituteExist)
        store = store.updated(oldValue, if (isName) someRandomName else UUID.randomUUID().toString)

      store(oldValue)
    }

    //This is not tail recursive, hence for a large file this may blow up our stack
    def anonymizeHelper(dataToAnonymize: ObjectRepresentation): ObjectRepresentation = {

      var res = dataToAnonymize

      for (key <- dataToAnonymize.keys) {
        val data: Any = dataToAnonymize(key)
        if (!data.isInstanceOf[ObjectRepresentation]) {
          val lowerCasedKey = key.toLowerCase
          if (lowerCasedKey.matches(matchRegex))
            res = res.updated(key, produceAnonymousValue(data.asInstanceOf[String], lowerCasedKey.endsWith("name")))
          else res = res.updated(key, data)
        }
        else res = res.updated(key, anonymizeHelper(data.asInstanceOf[ObjectRepresentation]))
      }

      res
    }

    Some(anonymizeHelper(input))
  }
}
