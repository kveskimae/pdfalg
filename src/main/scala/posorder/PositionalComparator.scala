package posorder

import java.awt.Point
import java.io.IOException

import candidate.Candidate
import org.pdfextractor.db.domain.dictionary.PaymentFieldType

import scala.collection._

object PositionalComparator extends Ordering[Candidate] {

  val nodes: mutable.HashMap[PaymentFieldType, Node] = mutable.HashMap.empty

  try {
    val map: mutable.HashMap[PaymentFieldType, Seq[Point]] = mutable.HashMap.empty[PaymentFieldType, Seq[Point]] // TODO!! IOHelper.getMapFromFile("locations.json");

    for (key <- map.keySet) {

      val locationsAsOption: Option[Seq[Point]] = map.get(key)

      if (locationsAsOption.nonEmpty)
      {
        val locations: TraversableOnce[Point] = locationsAsOption.get

        val node: Node = new Node(HORIZONTAL, 0, GridConstants.A4_WIDTH_PX, 0, GridConstants.A4_HEIGHT_PX)

        node.addLocations(locations)

        nodes.put(key, node)
      }
    }
  } catch {
    case ioe: IOException => throw new RuntimeException("Unable to load location data from locations.json", ioe)
  }

  def compare(o1: Candidate, o2: Candidate): Int = {
    if (o1.paymentFieldType == o2.paymentFieldType) {
      val nodeAsOption: Option[Node] = nodes.get(o1.paymentFieldType)
      if (nodeAsOption.nonEmpty) {
        val node: Node = nodeAsOption.get

        val depth1 = node.getMaxDepthForLocation(o1)

        val depth2 = node.getMaxDepthForLocation(o2)

        val minDepth = Math.min(depth1, depth2)

        val frequency1 = node.getDataPointsAtLevelForLocation(o1, minDepth)

        val frequency2 = node.getDataPointsAtLevelForLocation(o2, minDepth)

        return frequency2 - frequency1
      }
    }
    0
  }

}
