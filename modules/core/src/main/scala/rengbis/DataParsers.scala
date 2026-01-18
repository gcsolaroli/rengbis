package rengbis

import java.nio.file.{ Files, Path }
import zio.Chunk
import zio.json.ast.Json
import zio.json.ast.Json.{ Arr, Bool, Num, Obj, Str }
import zio.json.DecoderOps
import com.github.tototoshi.csv.CSVReader

import dev.hnaderi.yaml4s.ziojson.ZioJsonWriter
import dev.hnaderi.yaml4s.Backend

import scala.xml.{ Elem, Node, XML }
import scala.util.{ Failure, Success, Try }
// import com.github.tototoshi.csv.CSVReader
import java.io.StringReader
import rengbis.Schema.{ ListOfValues, Schema }

object DataParsers:
    type Validator = ((Schema) => Either[String, Value])
    type Parser[A] = (A) => Validator

    def json(file: Path): Validator         = json(Files.readString(file)) //  TODO: parse directly from file
    def json(jsonString: String): Validator = (schema: Schema) =>
        jsonString
            .fromJson[Json]
            .map(fromJson)
            .flatMap(validateValue(schema))

    def yaml(file: Path): Validator         = yaml(Files.readString(file)) //  TODO: parse directly from file
    def yaml(yamlString: String): Validator = (schema: Schema) => Backend.parse[Json](yamlString).left.map(_.getMessage()).map(fromJson(_)).flatMap(validateValue(schema))

    def xml(file: Path): Validator        = xml(Files.readString(file)) //  TODO: parse directly from file
    def xml(xmlString: String): Validator = (schema: Schema) =>
        Try(xmlToValue(XML.loadString(xmlString))) match
            case Success(value)     => Right(value)
            case Failure(exception) => Left(exception.getMessage()).flatMap(validateValue(schema))

    def csv(file: Path): Validator        = csv(CSVReader.open(file.toFile))
    def csv(csvString: String): Validator = csv(CSVReader.open(new StringReader(csvString)))
    def csv(reader: CSVReader): Validator = (schema: Schema) =>
        schema match
            case Schema.ListOfValues(Schema.ObjectValue(objSchema), _) =>
                val csvRows   = reader.allWithHeaders()
                val keyNames  = objSchema.keys.map(_.label).toSet
                val rowValues = csvRows.map { row =>
                    val filteredRow = row.filter((k, _) => keyNames.contains(k))
                    Value.ObjectWithValues(filteredRow.map((k, v) => (k, Value.TextValue(v))))
                }
                Right(Value.ListOfValues(Chunk.fromIterable(rowValues))).flatMap(validateValue(schema))

            case Schema.ListOfValues(Schema.TupleValue(tupleSchemas*), _) =>
                val csvRows   = reader.all()
                val rowValues = csvRows.map { row =>
                    val tupleValues = row.take(tupleSchemas.size).map(Value.TextValue(_))
                    Value.TupleOfValues(Chunk.fromIterable(tupleValues))
                }
                Right(Value.ListOfValues(Chunk.fromIterable(rowValues))).flatMap(validateValue(schema))

            case _ =>
                Left("Schema not compatible with CSV data: root element must be a ListOfValues containing ObjectValue or TupleValue")

    def text(file: Path): Validator   = text(Files.readString(file))
    def text(text: String): Validator = (schema: Schema) => Right(rengbis.Value.TextValue(text)).flatMap(validateValue(schema))

    protected def validateValue(schema: Schema)(value: Value): Either[String, Value] = Validator.validateValue(schema, value).toEither.map(_ => value)

    private def fromJson(json: Json): Value = json match
        case Bool(value)   => Value.BooleanValue(value)
        case Num(value)    => Value.NumberValue(value)
        case Str(value)    => Value.TextValue(value)
        case Arr(elements) => Value.ListOfValues(elements.map(fromJson))
        case Obj(fields)   => Value.ObjectWithValues(fields.map((key, jsonValue) => (key, fromJson(jsonValue))).toMap)
        case Json.Null     => Value.NullValue()

    private def xmlToValue(node: Node): Value = node match
        case elem: Elem =>
            val attributes: Map[String, Value.TextValue] = elem.attributes.asAttrMap.map { case (k, v) => k -> Value.TextValue(v) }

            val childrenByName: Map[String, Value] = elem.child
                .filter(_.isInstanceOf[Elem])
                .groupBy(_.label)
                .map { case (label, nodes) =>
                    if nodes.length > 1 then label -> Value.ListOfValues(Chunk.fromIterable(nodes.map(n => xmlToValue(n))))
                    else label                     -> xmlToValue(nodes.head)
                }

            val textContent: String = elem.child
                .filter(node => node.isInstanceOf[scala.xml.Text] && node.text.trim.nonEmpty)
                .map(_.text.trim)
                .mkString(" ")

            val content: Map[String, Value] =
                if textContent.nonEmpty && childrenByName.isEmpty then Map("_text" -> Value.TextValue(textContent))
                else if textContent.nonEmpty then childrenByName + ("_text"        -> Value.TextValue(textContent))
                else childrenByName

            val obj: Map[String, Value] = attributes ++ content

            if obj.size == 1 && obj.contains("_text") && attributes.isEmpty then Value.TextValue(textContent)
            else Value.ObjectWithValues(obj)

        case _ =>
            if node.text.trim.nonEmpty then Value.TextValue(node.text.trim)
            else Value.NullValue()
