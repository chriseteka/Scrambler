package com.chrisworks
package service

import io.circe.optics.JsonPath._
import io.circe.optics.all.jsonPlated
import io.circe.{Json, JsonObject}
import monocle.function.Plated

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
  val INPUT_KEY = "input"

  //These are the keys of interest, whose value we plan to scramble in a consistent manner
  val keysOfInterest = List("name", "id")
  val matchRegex: String = keysOfInterest.mkString(".*(", "|", ")$") //sample => ".*(name|id)$"

  /**
   * This is the main logic of the scrambler/anonymize util. We will follow the logic that:
   * 1. Any key ending with name must be scrambled to something else which must be unique
   * 2. Anything ending with the word 'id' must be replaced with a consistent UUID, so as to maintain relationship
   */
  val anonymizeInput: Json => Option[Json] = input => {

    val preparedInput = Json.fromJsonObject(JsonObject(INPUT_KEY -> input))

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

    /**
     * This does the job of taking an object, inspecting it and doing modification on it.
     * Is it the best I could have done? I have seen modifyF and traverseF, could they have been better?
     */
    val modifyIfKeyEndsWithIdOrName: Json => Json =
      root.obj.modify { obj =>
        JsonObject.fromIterable(obj.toIterable.map { case (key, json) =>
          if (key.toLowerCase.matches(matchRegex) && json.isString) {
            (key, Json.fromString(produceAnonymousValue(json.noSpaces, key.toLowerCase.endsWith("name"))))
          } else (key, json)
        })
      }

    val recursivelyTransform: Json => Json = Plated.transform[Json](modifyIfKeyEndsWithIdOrName)

    Some(recursivelyTransform(preparedInput)).map(_.\\(INPUT_KEY).head)
  }
}
