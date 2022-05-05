package com.chrisworks
package util

import org.json4s.DefaultFormats
import org.json4s.native.{Json, JsonParser}

import java.io.{BufferedWriter, File, FileWriter}
import javax.swing.JFileChooser
import scala.util.{Failure, Success, Try}

object FileUtil {

  type ObjectRepresentation = Map[String, Any]

  val FILE_EXT = ".json"
  val validateFile: String => Try[Boolean] = filePath => {
    println(s"Validating file...: $filePath")
    if (filePath.toLowerCase.endsWith(FILE_EXT)) Success(true)
    else Failure(new RuntimeException(s"Invalid file supplied, please supply a file that ends with '$FILE_EXT'"))
  }

  def openFileChooser(): Option[String] = {
    val chooser = new JFileChooser()
    chooser.setCurrentDirectory(new File("../scrambler/src/main/resources/"))
    chooser.setDialogTitle("Choose a JSON file to scramble")
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
    chooser.setAcceptAllFileFilterUsed(true)
    if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      Some(chooser.getSelectedFile.getAbsolutePath)
    } else None
  }

  /**
   * This method reads a filePath, gets a json file and turns it into a JValue
   *
   * @param filePath This is the path to the json file of interest
   * @return Option[JValue], just in case the result doesn't exist, we are guaranteed not to fail
   */
  def readFileFrom(filePath: String): Option[(String, ObjectRepresentation)] = {
    println("Operation started...")
    (for {
      _ <- validateFile(filePath)
      _ <- Success(println("Reading file..."))
      bufferedResource <- Try(scala.io.Source.fromFile(filePath))
      _ <- Success(println("Parsing file..."))
      jsonValue <- Try(JsonParser.parse(bufferedResource.bufferedReader()))
      result = jsonValue.toSome.map { value =>
        val outputPath = filePath.replace(FILE_EXT, "")
        outputPath -> value.values.asInstanceOf[ObjectRepresentation]
      }
    } yield result) match {
      case Failure(exception) =>
        println(
          s"""An exception occurred while trying to read file.
             |Error Message: ${exception.getMessage}""".stripMargin)
        None
      case Success(value) =>
        println("Completed reading and parsing...")
        value
    }
  }

  /**
   * This function does a reverse, it takes the path and scrambled JValue to produce a json file
   *
   * @param outputPath This is the path where the output json file will be written into
   * @param data       This is the scrambled data
   */
  def turnScrambledDataToFile(outputPath: String, data: ObjectRepresentation): Unit = {
    println("Started writing output...")
    val file = new File(outputPath + FILE_EXT)
    val bw = new BufferedWriter(new FileWriter(file))
    Json(DefaultFormats).writePretty(data, bw)
    println("Completed writing output...")
    bw.close()
  }
}
