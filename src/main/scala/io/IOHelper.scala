package io

import java.awt._
import java.io.{File, InputStream}

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import org.apache.commons.io.IOUtils
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection.immutable.Map.Map2
import scala.collection.mutable.ArrayBuffer
import scala.collection.{Map, mutable}


object IOHelper {

  def getMapFromFile(fileName: String): Map[PaymentFieldType, Seq[Point]] = {

    val jsonAsMap: Map[Any, Any] = getAsMap(fileName)

    val retBuilder: mutable.MapBuilder[PaymentFieldType, Seq[Point], Map[PaymentFieldType, Seq[Point]]] = new scala.collection.mutable.MapBuilder[PaymentFieldType, Seq[Point], Map[PaymentFieldType, Seq[Point]]](Map.empty)

    for (jsonAsMapEntry <- jsonAsMap) {

      val retEntry: Tuple2[PaymentFieldType, Seq[Point]] = extractEntry(jsonAsMapEntry)

      retBuilder += retEntry
    }

    retBuilder.result()
  }

  private def extractPoints(field2PointsTuple: (String, Seq[Map2[String, BigInt]])): scala.Seq[_root_.java.awt.Point] = {

    val pointsBuilder = ArrayBuffer[Point]()

    val pointsXY: Seq[Map2[String, BigInt]] = field2PointsTuple._2

    for ( pointXY: Map2[String, BigInt] <- pointsXY ) {

      val x: Int = pointXY.get("x").get.intValue()

      val y: Int = pointXY.get("y").get.intValue()

      val p: Point = new Point(x, y)

      pointsBuilder += p
    }

    val points = pointsBuilder.toArray

    points
  }

  private def extractEntry(jsonAsMapEntry: (Any, Any)): (PaymentFieldType, scala.Seq[_root_.java.awt.Point]) = {

    val field2PointsTuple: Tuple2[String, Seq[Map2[String, BigInt]]] = jsonAsMapEntry.asInstanceOf[Tuple2[String, Seq[Map2[String, BigInt]]]]

    val fieldAsString: String = field2PointsTuple._1

    val fieldType: PaymentFieldType = PaymentFieldType.valueOf(fieldAsString)

    val points: Seq[Point] = extractPoints(field2PointsTuple)

    val retEntry: Tuple2[PaymentFieldType, Seq[Point]] = (fieldType, points)

    retEntry
  }

  private def getAsMap(fileName: String): _root_.scala.collection.Map[Any, Any] = {

    val jsonAsString = getStringFromFile(fileName)

    val jsonAsJValue: JValue = JsonParser.parse(jsonAsString)

    val jsonAsMap: Map[Any, Any] = jsonAsJValue.values.asInstanceOf[Map[Any, Any]]

    jsonAsMap
  }

  def getStringFromFile(fileName: String): String = {
    val inputStream = getInputStreamFromFile(fileName)
    val ret = IOUtils.toString(inputStream)
    ret
  }

  def getInputStreamFromFile(fileName: String): InputStream = {
    val inputStream: InputStream = Thread.currentThread.getContextClassLoader.getResourceAsStream(fileName)
    inputStream
  }

  def getFolderAsFile(location2: String): File = {
    val url: java.net.URL = Thread.currentThread.getContextClassLoader.getResource(location2)
    val location: String = url.getFile
    val file: File = new File(location)

    if (file == null) throw new IllegalArgumentException("No folder was found at '" + location + "'")

    IOHelper.checkFileExistsAndIsReadable(file, true)

    file
  }

  def checkFileExistsAndIsReadable(file: File, checkIsDirectory: Boolean): Unit = {
    if (!file.exists) {
      throw new RuntimeException("Location '" + file + "' does not exist")
    }

    if (!file.canRead) {
      throw new RuntimeException("Location '" + file + "' is not readable for application")
    }

    if (checkIsDirectory) {
      if (!file.isDirectory) {
        throw new RuntimeException("Location '" + file + "' is not a folder")
      }
    }
  }

}
