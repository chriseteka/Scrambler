package com.chrisworks

import TestUtils.sampleFilePath
import util.FileUtil

object FileUtilTest extends App {

  FileUtil.readFileFrom(sampleFilePath) foreach {
    case (outputPath, jsonValue) =>
      println(jsonValue.spaces2)
      FileUtil.turnScrambledDataToFile(outputPath, jsonValue)
  }

}
