package com.chrisworks

import TestUtils.sampleFilePath
import util.FileUtil

import org.json4s.native._

object FileUtilTest extends App {

  FileUtil.readFileFrom(sampleFilePath) foreach {
    case (outputPath, jsonValue) =>
      println(JsonMethods.parse(jsonValue.values.toString))
      FileUtil.turnScrambledDataToFile(outputPath, jsonValue)
  }

}
