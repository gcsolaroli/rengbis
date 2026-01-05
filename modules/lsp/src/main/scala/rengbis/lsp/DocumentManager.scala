package rengbis.lsp

import scala.collection.concurrent.TrieMap

class DocumentManager:
    private val documents = TrieMap[String, DocumentState]()

    def openDocument(uri: String, text: String, version: Int): Unit   = documents(uri) = DocumentState(text, version)
    def closeDocument(uri: String): Unit                              = documents.remove(uri)
    def updateDocument(uri: String, text: String, version: Int): Unit = documents(uri) = DocumentState(text, version)
    def getDocument(uri: String): Option[DocumentState]               = documents.get(uri)
    def getAllDocuments: Map[String, DocumentState]                   = documents.toMap

case class DocumentState(text: String, version: Int)
