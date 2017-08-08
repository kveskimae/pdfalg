package org.pdfextractor.algorithm.candidate.posorder

import java.awt.Point
import java.io.IOException

import org.pdfextractor.algorithm.candidate.Candidate
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection._

object PositionalComparator extends Ordering[Candidate] {

  type field2Points = mutable.HashMap[PaymentFieldType, Seq[Point]]

  val nodes: mutable.HashMap[PaymentFieldType, Node] = mutable.HashMap.empty

  try {
    val map: field2Points = mutable.HashMap.empty // TODO!! IOHelper.getMapFromFile("locations.json");

    map.keySet.foreach((key: PaymentFieldType) => {
      map.get(key) match {
        case Some(locations: TraversableOnce[Point]) => {
          val node: Node = new Node(Horizontal, 0, A4WidthPx, 0, A4HeightPx)
          node.addLocations(locations)
          nodes.put(key, node)
        }
        case _ =>
      }
    })
  } catch {
    case ioe: IOException =>
      throw new RuntimeException("Unable to load location data", ioe)
  }

  def compare(o1: Candidate, o2: Candidate): Int = {
    require(o1.paymentFieldType == o2.paymentFieldType)
    nodes.get(o1.paymentFieldType) match {
      case Some(node) =>
        val minDepth = Math.min(node.getMaxDepthForLocation(o1),
          node.getMaxDepthForLocation(o2))
        node.getDataPointsAtLevelForLocation(o2, minDepth) - node
          .getDataPointsAtLevelForLocation(o1, minDepth)
      case None => 0
    }
  }

}
