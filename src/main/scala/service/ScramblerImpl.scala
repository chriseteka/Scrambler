package com.chrisworks
package service

import service.Scrambler.anonymizeInput
import util.FileUtil

case class ScramblerImpl() extends Scrambler {

  override def scramble(filePath: String, produceGraph: Boolean): Unit = for {
    (outputPath, rawData) <- FileUtil.readFileFrom(filePath)
    (scrambledData, graphBuilder) <- anonymizeInput(rawData)
  } yield {
    FileUtil.turnScrambledDataToFile(outputPath, scrambledData)
    if (produceGraph) FileUtil.writeAsGraphvizFile(outputPath, graphBuilder.buildGraph())
  }
}
