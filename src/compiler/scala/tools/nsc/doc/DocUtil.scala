/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc
package doc

import java.io.StringReader
import org.xml.sax.InputSource

import scala.collection.immutable.{ListMap, TreeSet}
import scala.xml._

object DocUtil
{
  def load(str: String): NodeSeq =
    if ((str == null) || (str.length == 0))
      NodeSeq.Empty
    else {
      val xmlSrc =
        if (str.matches("^(<!--.*-->)*<[^>]+>.*<[^>]+>(<!--.*-->)*$")) str
        else "<div>" + str + "</div>"
      XML.load(new StringReader(xmlSrc))
    }

  def br(nodes: NodeSeq): NodeSeq = nodes ++ (<br/>)
  def hr(nodes: NodeSeq): NodeSeq = nodes ++ (<hr/>)

  trait UrlContext {
    def relative: String

    def aref(href0: String, target: String, text: String): NodeSeq = {
      if (href0 == null) return Text(text);
      
      val href = {
        if (href0.startsWith("http:") || href0.startsWith("file:")) "";
        else relative
      } + Utility.escape(href0)
      if ((target ne null) && target.indexOf('<') != -1) throw new Error(target)

      val t0 = Text(text)
      if (target ne null)
        (<a href={href} target={target}>{t0}</a>);
      else
        (<a href={href}>{t0}</a>);
    }

    // can't use platform default here or the generated XML may end up all MacRoman
    val encoding = Properties.sourceEncoding
    val generator = System.getProperty("doc.generator", "scaladoc (" + Properties.versionString + ")")
    val header =
      (<meta http-equiv="content-type" content={"text/html; charset=" + encoding}/>
      <meta name="generator" content={generator}/>
      <link rel="stylesheet" type="text/css" href={ relative + "style.css"}/>
      <script type="text/javascript" src={relative + "script.js"}></script>);

    def body0(hasBody: Boolean, nodes: NodeSeq): NodeSeq =
      if (!hasBody) nodes else (<body onload="init()">{nodes}</body>);

    val dtype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

    def page(title: String, body: NodeSeq, hasBody: Boolean): NodeSeq =
      (<html>
        <head><title>{Text(if (title eq null) "null title" else title)}</title>
        {header}
        </head>
        {body0(hasBody, body)}
      </html>)
  } // UrlContext

  def div0(title: String): NodeSeq =
    (<div class="doctitle-larger">{Text(title)}</div>);

  def merge[T](ts0: TreeSet[T], ts1: TreeSet[T]): TreeSet[T] = ts0 ++ ts1
  
  def merge[T,S](ts0: ListMap[T,TreeSet[S]], ts1: ListMap[T,TreeSet[S]]): ListMap[T,TreeSet[S]] = {
    (ts1 foldLeft ts0) { case (xs, (k, v)) =>
      if (xs contains k) xs.updated(k, xs(k) ++ v)
      else xs.updated(k, v)
    }
  }

  implicit def coerceIterable[T](list : Iterable[T]) = NodeWrapper(list.iterator)
  implicit def coerceIterator[T](list : Iterator[T]) = NodeWrapper(list)

  case class NodeWrapper[T](list: Iterator[T]) {
    def interleave(xs: Seq[NodeSeq], sep: NodeSeq): NodeSeq =
      if (xs.isEmpty) NodeSeq.Empty
      else if (xs.size == 1) xs.head
      else xs.head ++ sep ++ interleave(xs.tail, sep)

    def mkXML(begin: NodeSeq, separator: NodeSeq, end: NodeSeq)(f: T => NodeSeq): NodeSeq =
      begin ++ interleave(list.toSeq map f, separator) ++ end

    def mkXML(begin: String, separator: String, end: String)(f: T => NodeSeq): NodeSeq =
      this.mkXML(Text(begin), Text(separator), Text(end))(f)

    def surround(open: String, close: String)(f: T => NodeSeq) =
      if (list.hasNext) mkXML(open, ", ", close)(f)
      else NodeSeq.Empty
  }
}
