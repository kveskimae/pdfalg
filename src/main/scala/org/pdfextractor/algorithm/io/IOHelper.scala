package org.pdfextractor.algorithm

import java.awt._
import java.io.{File, InputStream}
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import java.util.Locale

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import org.apache.commons.io.{FileUtils, IOUtils}
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection.immutable.Map.Map2
import scala.collection.{Map, mutable}

package object io {

  type rawField2Pts = (String, Seq[Map2[String, BigInt]])

  type field2Pts = (PaymentFieldType, Seq[Point])

  type field2PtsMap = Map[PaymentFieldType, Seq[Point]]

  def getMapFromFile(fileName: String): field2PtsMap = {
    getAsMap(fileName).map(jsonAsMapEntry => {
      extractEntry(jsonAsMapEntry)
    })
  }

  private def extractEntry(jsonAsMapEntry: (Any, Any)): field2Pts = {
    val field2PointsTuple: rawField2Pts =
      jsonAsMapEntry.asInstanceOf[rawField2Pts]

    val fieldType: PaymentFieldType =
      PaymentFieldType.valueOf(field2PointsTuple._1)
    val points: Seq[Point] = extractPoints(field2PointsTuple)

    (fieldType, points)
  }

  private def extractPoints(field2PointsTuple: rawField2Pts): Seq[Point] = {
    field2PointsTuple._2.map(pt => {
      val x = pt.get("x").get.intValue()
      val y = pt.get("y").get.intValue()
      new Point(x, y)
    })
  }

  private def getAsMap(fileName: String): Map[Any, Any] = {
    val jsonAsString = getStringFromFile(fileName)
    val jsonAsJValue: JValue = JsonParser.parse(jsonAsString)
    val jsonAsMap: Map[Any, Any] =
      jsonAsJValue.values.asInstanceOf[Map[Any, Any]]
    jsonAsMap
  }

  def getStringFromFile(fileName: String): String = {
    IOUtils.toString(getInputStreamFromFile(fileName))
  }

  def getInputStreamFromFile(fileName: String): InputStream = {
    Thread.currentThread.getContextClassLoader.getResourceAsStream(fileName)
  }

  def getFolderAsFile(location2: String): File = {
    val url: java.net.URL =
      Thread.currentThread.getContextClassLoader.getResource(location2)
    val location: String = url.getFile
    val file: File = new File(location)
    checkFile(file, true)
    file
  }

  def checkFile(file: File, checkIsDirectory: Boolean): Unit = {
    require(file.exists, "Location '" + file + "' does not exist")
    require(file.canRead,
      "Location '" + file + "' is not readable for application")
    require(!checkIsDirectory || file.isDirectory,
      "Location '" + file + "' is not a folder")
  }

  def writeToFiles(map: Map[PaymentFieldType, Seq[Point]]) = {
    map.foreach {
      case (paymentFieldType, locations) => {
        val fileName = paymentFieldType.toString + ".txt"

        val locationsString =
          locations
            .map(point => point.x + "," + point.y + System.lineSeparator)
            .reduce(_ + _)

        FileUtils.writeStringToFile(new File(fileName), locationsString)
      }
    }
  }

  def writeStatisticsToFiles(locale: Locale,
                             trainingData: mutable.Map[PaymentFieldType, mutable.Buffer[Map[String, Any]]]
                            ): Unit = {
    trainingData.foreach {
      case (paymentFieldType: PaymentFieldType, candidates: mutable.Buffer[Map[String, Any]]) => {
        val sb = new StringBuilder

        sb.append(candidates(0).keys.reduce(_ + "," + _))
        sb.append(System.lineSeparator)
        sb.append(candidates.map(_.values.reduce(_ + "," + _) + System.lineSeparator))

        val fileName = "statistics-" + locale.getLanguage + "-" + paymentFieldType.toString.toLowerCase + "-" + LocalDate.now().getMonthValue + "-" + LocalDate.now().getDayOfMonth + ".csv"
        FileUtils.writeStringToFile(new File(fileName), sb.toString, StandardCharsets.UTF_8)
      }
    }
  }

}