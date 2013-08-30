/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.dheinrich.farmer

import org.xml.sax.InputSource
import scala.xml._
import parsing._
import java.io.ByteArrayInputStream
import com.ning.http.client.Response

object HTML5Parser extends NoBindingFactoryAdapter {

  override def loadXML(source : InputSource, _p: SAXParser) = {
    loadXML(source)
  }

  def loadXML(source : InputSource) = {
    import nu.validator.htmlparser.{sax,common}
    import sax.HtmlParser
    import common.XmlViolationPolicy

    val reader = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(this)
    reader.parse(source)
    rootElem
  }

  def loadXML(source: String): Node = {
    val is = new ByteArrayInputStream(source.getBytes())
    loadXML(new InputSource(is))
  }
}

object HTML5 extends (Response => scala.xml.Node){
  def apply(r:Response) = (dispatch.as.String andThen HTML5Parser.loadXML)(r)
}


