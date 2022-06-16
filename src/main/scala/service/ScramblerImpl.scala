package com.chrisworks
package service

import domain.SyncGraph
import service.Scrambler.anonymizeInput
import util.FileUtil

case class ScramblerImpl() extends Scrambler {

  override def scramble(filePath: String, produceGraph: Boolean): Unit = for {
    (outputPath, rawData) <- FileUtil.readFileFrom(filePath)
    graph <- SyncGraph.fromJson(rawData)
    (scrambledData, graphBuilder) <- anonymizeInput(rawData)
  } yield {
    val g = graph.buildGraph()
    println(g)
    FileUtil.turnScrambledDataToFile(outputPath, scrambledData)
    if (produceGraph) FileUtil.writeAsGraphvizFile(outputPath, graphBuilder.buildGraph())
  }
}
