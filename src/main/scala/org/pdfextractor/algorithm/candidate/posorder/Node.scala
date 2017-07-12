package org.pdfextractor.algorithm.candidate.posorder

import java.awt._

import org.pdfextractor.algorithm.candidate.Candidate

import scala.collection.mutable.ArrayBuffer


class Node(val cutDirection: CutDirection,
           val minX: Int,
           val maxX: Int,
           val minY: Int,
           val maxY: Int,
           var smallerValues: Option[Node] = Option.empty,
           var biggerValues: Option[Node] = Option.empty) {

  var locations: ArrayBuffer[Point] = ArrayBuffer.empty

  def getNumberOfLocations: Int = locations.size

  def addLocation(point: Point): Unit = {
    locations += point
  }

  def addLocations(paramLocations: TraversableOnce[_ <: Point]): Unit = {
    locations ++= paramLocations
    split()
  }

  def split(): Unit = {
    if (locations.size <= CellMinPoints) return
    if ((maxX - minX) < 2 * CellMinLengthPx) return
    if ((maxY - minY) < 2 * CellMinLengthPx) return
    var cutline = 0
    cutDirection match {
      case Vertical =>
        cutline = (minX + maxX) / 2
        smallerValues = Option(new Node(Horizontal, minX, cutline - 1, minY, maxY))
        biggerValues = Option(new Node(Horizontal, cutline, maxX, minY, maxY))
        for (point <- locations) {
          if (point.getX < cutline) smallerValues.get.addLocation(point)
          else biggerValues.get.addLocation(point)
        }
      case Horizontal =>
        cutline = (minY + maxY) / 2
        smallerValues = Option(new Node(Vertical, minX, maxX, minY, cutline - 1))
    smallerValues = Option(new Node(Vertical, minX, maxX, cutline, maxY))
        for (point <- locations) {
          if (point.getY < cutline) smallerValues.get.addLocation(point)
          else biggerValues.get.addLocation(point)
        }
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
    smallerValues.get.split
    biggerValues.get.split
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(this.getStringRepresentation(""))
    sb.toString
  }

  def getStringRepresentation(prefixWhitespace: String): String = {
    val sb = new StringBuilder
    sb.append(prefixWhitespace).append('+').append(cutDirection)
    sb.append('(').append("x:").append(minX).append(',').append(maxX).append(';')
    sb.append("y:").append(minY).append(',').append(maxY)
    sb.append(')').append(' ').append(getNumberOfLocations)
    if (this.smallerValues.isDefined) {
      sb.append('\n')
      sb.append(smallerValues.get.getStringRepresentation(prefixWhitespace + "\t"))
    }
    if (this.biggerValues.isDefined) {
      sb.append('\n')
      sb.append(biggerValues.get.getStringRepresentation(prefixWhitespace + "\t"))
    }
    sb.toString
  }

  @SuppressWarnings(Array("rawtypes")) def getMaxDepthForLocation(location: Candidate): Int = {
    checkLocationArgument(location)
    cutDirection match {
      case Horizontal =>
        if (location.y < (minY + maxY) / 2) if (smallerValues.isDefined) smallerValues.get.getMaxDepthForLocation(location) + 1
        else 0
        else if (biggerValues.isDefined) biggerValues.get.getMaxDepthForLocation(location) + 1
        else 0
      case Vertical =>
        if (location.x < (minX + maxX) / 2) if (smallerValues.isDefined) smallerValues.get.getMaxDepthForLocation(location) + 1
        else 0
        else if (biggerValues.isDefined) biggerValues.get.getMaxDepthForLocation(location) + 1
        else 0
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
  }

  def getDataPointsAtLevelForLocation(location: Candidate, depth: Int): Int = {
    checkLocationArgument(location)
    if (depth < 0) throw new IllegalArgumentException("Depth must be non-negative")
    cutDirection match {
      case Horizontal =>
        if (location.y < (minY + maxY) / 2) if (smallerValues.isDefined) smallerValues.get.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
        else if (biggerValues.isDefined) biggerValues.get.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
      case Vertical =>
        if (location.x < (minX + maxX) / 2) if (smallerValues.isDefined) smallerValues.get.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
        else if (biggerValues.isDefined) biggerValues.get.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
  }

  def checkLocationArgument(location: Candidate) = {
    if (location.x > maxX || location.y > maxY) throw new IllegalArgumentException("Location must be contained inside grid: " + location)
  }

}
