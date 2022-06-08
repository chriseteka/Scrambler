package com.chrisworks
package service

import io.circe.optics.JsonPath._
import io.circe.optics.all.jsonPlated
import io.circe.{Json, JsonObject}
import monocle.function.Plated

import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

trait Scrambler {

  /**
   * This method takes a filePath, reads it's json and scrambles some sensitive data like name and id, in it
   *
   * @param filePath This is the path to the json file whose sensitive data need to be scrambled
   */
  def scramble(filePath: String): Unit
}

object Scrambler {

  val ID = "id"
  val NAME = "name"
  val INPUT_KEY = "input"
  val liveScrambler: Scrambler = ScramblerImpl()

  //These are the keys of interest, whose value we plan to scramble in a consistent manner
  val keysOfInterest = List(ID, NAME)
  val matchRegex: String = keysOfInterest.mkString(".*(", "|", ")$") //sample => ".*(name|id)$"

  /**
   * This is the main logic of the scrambler/anonymize util. We will follow the logic that:
   * 1. Any key ending with name must be scrambled to something else which must be unique
   * 2. Anything ending with the word 'id' must be replaced with a consistent UUID, so as to maintain relationship
   */
  val anonymizeInput: Json => Option[Json] = input => {

    def someRandomUUID(id: String): String = UUID.nameUUIDFromBytes(id.getBytes).toString

    def someRandomName(oldValue: String): String = Base64.getEncoder.encodeToString(oldValue.getBytes(StandardCharsets.UTF_8)).substring(8, 15)

    val produceAnonymousValue: (String, Boolean) => String = (oldValue, isName) => {
      if (isName) someRandomName(oldValue) else someRandomUUID(oldValue)
    }

    /**
     * This does the job of taking an object, inspecting it and doing modification on it.
     * Is it the best I could have done? I have seen modifyF and traverseF, could they have been better?
     */
    val modifyIfKeyEndsWithIdOrName: Json => Json =
      root.obj.modify { obj =>
        JsonObject.fromIterable(
          obj.toIterable.map { case (key, json) =>
            val lowerKey = key.toLowerCase
            if (lowerKey.matches(matchRegex) && json.isString)
              (key, Json.fromString(produceAnonymousValue(json.noSpaces, lowerKey.endsWith(NAME))))
            else (key, json)
          }
        )
      }

    Some(Plated.transform[Json](modifyIfKeyEndsWithIdOrName)(input))
  }
}
