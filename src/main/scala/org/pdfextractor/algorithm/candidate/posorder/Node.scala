package org.pdfextractor.algorithm.candidate.posorder

import java.awt._

import org.pdfextractor.algorithm.candidate.Candidate

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Node(val cutDirection: CutDirection,
           val minX: Int,
           val maxX: Int,
           val minY: Int,
           val maxY: Int,
           var smallerValues: Option[Node] = None,
           var biggerValues: Option[Node] = None) {

  var locations: mutable.Buffer[Point] = ArrayBuffer.empty

  def getNumberOfLocations: Int = locations.size

  def addLocation(point: Point): Unit = {
    locations += point
  }

  def addLocations(paramLocations: TraversableOnce[Point]): Unit = {
    locations ++= paramLocations
    trySplit()
  }

  def trySplit(): Unit = {
    if (isSplit()) {
      doSplit()
      smallerValues.get.trySplit
      biggerValues.get.trySplit
    }
  }

  def isSplit(): Boolean = {
    locations.size > CellMinPoints &&
      (maxX - minX) >= 2 * CellMinLengthPx &&
      (maxY - minY) >= 2 * CellMinLengthPx
  }

  def doSplit() = {
    cutDirection match {
      case Vertical =>
        val cutLine = (minX + maxX) / 2
        smallerValues = Some(
          new Node(Horizontal, minX, cutLine - 1, minY, maxY))
        biggerValues = Some(new Node(Horizontal, cutLine, maxX, minY, maxY))
        val split = locations.partition(_.x < cutLine)
        smallerValues.get.addLocations(split._1)
        biggerValues.get.addLocations(split._2)
      case Horizontal =>
        val cutLine = (minY + maxY) / 2
        smallerValues = Some(new Node(Vertical, minX, maxX, minY, cutLine - 1))
        smallerValues = Some(new Node(Vertical, minX, maxX, cutLine, maxY))
        for (point <- locations) {
          if (point.getY < cutLine) smallerValues.get.addLocation(point)
          else biggerValues.get.addLocation(point)
        }
      case _ =>
        throw new IllegalStateException(
          "Unknown cut direction: " + cutDirection)
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(this.getStringRepresentation(""))
    sb.toString
  }

  def getStringRepresentation(prefixWhitespace: String): String = {
    val sb = new StringBuilder
    sb.append(prefixWhitespace).append('+').append(cutDirection)
    sb.append('(')
      .append("x:")
      .append(minX)
      .append(',')
      .append(maxX)
      .append(';')
    sb.append("y:").append(minY).append(',').append(maxY)
    sb.append(')').append(' ').append(getNumberOfLocations)
    if (this.smallerValues.isDefined) {
      sb.append('\n')
      sb.append(
        smallerValues.get.getStringRepresentation(prefixWhitespace + "\t"))
    }
    if (this.biggerValues.isDefined) {
      sb.append('\n')
      sb.append(
        biggerValues.get.getStringRepresentation(prefixWhitespace + "\t"))
    }
    sb.toString
  }

  def isInSmallValues(candidate: Candidate): Boolean = {
    require(candidate.x <= maxX && candidate.y <= maxY,
      "Location must be contained inside grid: " + candidate)

    cutDirection match {
      case Horizontal => candidate.y < (minY + maxY) / 2
      case Vertical => candidate.x < (minX + maxX) / 2
      case _ =>
        throw new IllegalStateException(
          "Unknown cut direction: " + cutDirection)
    }
  }

  def getMaxDepthForLocation(location: Candidate): Int = {
    if (isInSmallValues(location)) {
      smallerValues match {
        case Some(_) => smallerValues.get.getMaxDepthForLocation(location) + 1
        case None => 0
      }
    } else {
      biggerValues match {
        case Some(_) => biggerValues.get.getMaxDepthForLocation(location) + 1
        case None => 0
      }
    }
  }

  def getDataPointsAtLevelForLocation(location: Candidate, depth: Int): Int = {
    require(depth > 0)

    if (isInSmallValues(location)) {
      smallerValues match {
        case Some(_) =>
          smallerValues.get.getDataPointsAtLevelForLocation(location, depth)
        case None => getNumberOfLocations
      }
    } else {
      biggerValues match {
        case Some(_) =>
          biggerValues.get.getDataPointsAtLevelForLocation(location, depth)
        case None => getNumberOfLocations
      }
    }
  }

}
