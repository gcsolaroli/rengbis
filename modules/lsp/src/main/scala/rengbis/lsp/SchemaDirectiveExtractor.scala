package rengbis.lsp

import zio.json.ast.Json
import zio.json.ast.Json.{ Obj, Str }
import zio.json.DecoderOps

object SchemaDirectiveExtractor:
    private val YamlPattern = """#\s*rengbis-schema:\s*(.+?)\s*$""".r
    private val XmlPattern  = """<!--\s*rengbis-schema:\s*(.+?)\s*-->""".r

    def extract(text: String, fileType: DataFileType): Option[String] = fileType match
        case DataFileType.Json => extractFromJson(text)
        case DataFileType.Yaml => extractFromYaml(text)
        case DataFileType.Xml  => extractFromXml(text)

    private def extractFromJson(text: String): Option[String] =
        text.fromJson[Json]
            .toOption
            .flatMap:
                case Obj(fields) => fields.collectFirst { case ("$rengbis-schema", Str(path)) => path }
                case _           => None

    private def extractFromYaml(text: String): Option[String] =
        text.linesIterator
            .flatMap(line => YamlPattern.findFirstMatchIn(line))
            .map(_.group(1))
            .nextOption()

    private def extractFromXml(text: String): Option[String] = XmlPattern.findFirstMatchIn(text).map(_.group(1))
