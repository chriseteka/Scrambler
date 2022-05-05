package com.chrisworks
package service

import service.Scrambler.anonymizeInput
import util.FileUtil

case class ScramblerImpl() extends Scrambler {

  override def scramble(filePath: String): Unit = for {
    (outputPath, rawData) <- FileUtil.readFileFrom(filePath)
    scrambledData <- anonymizeInput(rawData)
  } yield FileUtil.turnScrambledDataToFile(outputPath, scrambledData)
}
