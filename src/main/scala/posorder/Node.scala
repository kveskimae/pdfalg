package posorder

import java.awt._

import candidate.Candidate
import dictionary.{CutDirectionType, HORIZONTAL, VERTICAL}
import parser.GridConstants

import scala.collection.mutable.ArrayBuffer


class Node(val cutDirection: CutDirectionType,
           val minX: Int,
           val maxX: Int,
           val minY: Int,
           val maxY: Int,
           var smallerValues: Node = null,
           var biggerValues: Node = null) {

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
    if (locations.size <= GridConstants.MIN_NUMBER_OF_DATA_POINTS_IN_CELL) return
    if ((maxX - minX) < 2 * GridConstants.MIN_CELL_LENGTH_PX) return
    if ((maxY - minY) < 2 * GridConstants.MIN_CELL_LENGTH_PX) return
    var cutline = 0
    cutDirection match {
      case VERTICAL =>
        cutline = (minX + maxX) / 2
        smallerValues = new Node(HORIZONTAL, minX, cutline - 1, minY, maxY)
        biggerValues = new Node(HORIZONTAL, cutline, maxX, minY, maxY)
        for (point <- locations) {
          if (point.getX < cutline) smallerValues.addLocation(point)
          else biggerValues.addLocation(point)
        }
      case HORIZONTAL =>
        cutline = (minY + maxY) / 2
        smallerValues = new Node(VERTICAL, minX, maxX, minY, cutline - 1)
    smallerValues = new Node(VERTICAL, minX, maxX, cutline, maxY)
        for (point <- locations) {
          if (point.getY < cutline) smallerValues.addLocation(point)
          else biggerValues.addLocation(point)
        }
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
    smallerValues.split
    biggerValues.split
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
    if (this.smallerValues != null) {
      sb.append('\n')
      sb.append(smallerValues.getStringRepresentation(prefixWhitespace + "\t"))
    }
    if (this.biggerValues != null) {
      sb.append('\n')
      sb.append(biggerValues.getStringRepresentation(prefixWhitespace + "\t"))
    }
    sb.toString
  }

  @SuppressWarnings(Array("rawtypes")) def getMaxDepthForLocation(location: Candidate): Int = {
    checkLocationArgument(location)
    cutDirection match {
      case HORIZONTAL =>
        if (location.y < (minY + maxY) / 2) if (smallerValues != null) smallerValues.getMaxDepthForLocation(location) + 1
        else 0
        else if (biggerValues != null) biggerValues.getMaxDepthForLocation(location) + 1
        else 0
      case VERTICAL =>
        if (location.x < (minX + maxX) / 2) if (smallerValues != null) smallerValues.getMaxDepthForLocation(location) + 1
        else 0
        else if (biggerValues != null) biggerValues.getMaxDepthForLocation(location) + 1
        else 0
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
  }

  def getDataPointsAtLevelForLocation(location: Candidate, depth: Int): Int = {
    checkLocationArgument(location)
    if (depth < 0) throw new IllegalArgumentException("Depth must be non-negative")
    cutDirection match {
      case HORIZONTAL =>
        if (location.y < (minY + maxY) / 2) if (smallerValues != null) smallerValues.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
        else if (biggerValues != null) biggerValues.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
      case VERTICAL =>
        if (location.x < (minX + maxX) / 2) if (smallerValues != null) smallerValues.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
        else if (biggerValues != null) biggerValues.getDataPointsAtLevelForLocation(location, depth)
        else getNumberOfLocations
      case _ =>
        throw new IllegalStateException("Unknown cut direction: " + cutDirection)
    }
  }

  def checkLocationArgument(location: Candidate) = {
    if (location == null) throw new NullPointerException("Parameter location is null")
    if (location.x > maxX || location.y > maxY) throw new IllegalArgumentException("Location must be contained inside grid: " + location)
  }

}
