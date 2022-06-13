package com.chrisworks
package service

import service.Scrambler.anonymizeInput
import util.FileUtil

case class ScramblerImpl() extends Scrambler {

  override def scramble(filePath: String): Unit = for {
    (outputPath, rawData) <- FileUtil.readFileFrom(filePath)
    (scrambledData, graphBuilder) <- anonymizeInput(rawData)
  } yield {
    FileUtil.turnScrambledDataToFile(outputPath, scrambledData)
    FileUtil.writeAsGraphvizFile(outputPath, graphBuilder.buildGraph())
  }
}
