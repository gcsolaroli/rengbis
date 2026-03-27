package rengbis

object FormatAnnotation:
    case class Annotations(entries: Map[String, String]):
        def get(key: String): Option[String] = entries.get(key)
        def isEmpty: Boolean                 = entries.isEmpty

    object Annotations:
        val empty: Annotations = Annotations(Map.empty)
