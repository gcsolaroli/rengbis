package rengbis.lsp

enum DataFileType:
    case Json, Yaml, Xml

object DataFileType:
    def fromUri(uri: String): Option[DataFileType] =
        val lower = uri.toLowerCase
        if lower.endsWith(".json") then Some(DataFileType.Json)
        else if lower.endsWith(".yaml") || lower.endsWith(".yml") then Some(DataFileType.Yaml)
        else if lower.endsWith(".xml") then Some(DataFileType.Xml)
        else None
