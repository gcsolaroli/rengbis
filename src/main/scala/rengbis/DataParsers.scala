package rengbis

import zio.Chunk

import zio.json.ast.Json
import zio.json.ast.Json.{ Arr, Bool, Num, Obj, Str }
import zio.json.DecoderOps

import dev.hnaderi.yaml4s.ziojson.ZioJsonWriter
import dev.hnaderi.yaml4s.Backend

import scala.xml.{ Elem, Node, XML }
import scala.util.{ Failure, Success, Try }

object DataParsers:
    def json(jsonString: String): Either[String, Value] = jsonString.fromJson[Json].map(fromJson)
    def yaml(yamlString: String): Either[String, Value] = Backend.parse[Json](yamlString).left.map(_.getMessage()).map(fromJson(_))
    def xml(xmlString: String): Either[String, Value]   = Try(xmlToValue(XML.loadString(xmlString))) match
        case Success(value)     => Right(value)
        case Failure(exception) => Left(exception.getMessage())
    def text(text: String): Either[String, Value]       = Right(rengbis.Value.TextValue(text))

    private def fromJson(json: Json): Value = json match
        case Bool(value)   => Value.BooleanValue(value)
        case Num(value)    => Value.NumberValue(value)
        case Str(value)    => Value.TextValue(value)
        case Arr(elements) => Value.ArrayOfValues(elements.map(fromJson))
        case Obj(fields)   => Value.ObjectWithValues(fields.map((key, jsonValue) => (key, fromJson(jsonValue))).toMap)
        case Json.Null     => Value.NullValue()

    private def xmlToValue(node: Node): Value = node match
        case elem: Elem =>
            val attributes: Map[String, Value.TextValue] = elem.attributes.asAttrMap.map { case (k, v) => k -> Value.TextValue(v) }

            val childrenByName: Map[String, Value] = elem.child
                .filter(_.isInstanceOf[Elem])
                .groupBy(_.label)
                .map { case (label, nodes) =>
                    if nodes.length > 1 then label -> Value.ArrayOfValues(Chunk.fromIterable(nodes.map(n => xmlToValue(n))))
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
