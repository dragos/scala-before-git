/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */
 
package scala.tools.nsc
package doc
package html

import model._
import comment._

import xml.{Unparsed, XML, NodeSeq}
import xml.dtd.{DocType, PublicID}
import scala.collection._
import scala.util.NameTransformer
import java.io.File

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage { thisPage =>
  
  /** The path of this page, relative to the API site. `path.tail` is a list of folder names leading to this page (from
    * closest package to one-above-root package), `path.head` is the file name of this page. Note that `path` has a
    * length of at least one. */
  def path: List[String]

  /** The title of this page. */
  protected def title: String

  /** Additional header elements (links, scripts, meta tags, etc.) required for this page. */
  protected def headers: NodeSeq

  /** The body of this page. */
  protected def body: NodeSeq

  /** Writes this page as a file. The file's location is relative to the generator's site root, and the encoding is
    * also defined by the generator.
    * @param generator The generator that is writing this page. */
  def writeFor(site: HtmlFactory): Unit = {
    val pageFile = new File(site.siteRoot, absoluteLinkTo(thisPage.path))
    val pageFolder = pageFile.getParentFile
    if (!pageFolder.exists) pageFolder.mkdirs()
    val doctype =
      DocType("html", PublicID("-//W3C//DTD XHTML 1.1//EN", "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"), Nil)
    val html =
      <html>
        <head>
          <title>{ title }</title>
          <meta http-equiv="content-type" content={ "text/html; charset=" + site.encoding }/>
		      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
          { headers }
        </head>
        { body }
      </html>
    XML.save(pageFile.getPath, html, site.encoding, xmlDecl = false, doctype = doctype)
  }

  def templateToPath(tpl: TemplateEntity): List[String] = {
    def doName(tpl: TemplateEntity): String =
      NameTransformer.encode(tpl.name) + (if (tpl.isObject) "$" else "")
    def downPacks(pack: Package): List[String] =
      if (pack.isRootPackage) Nil else (doName(pack) :: downPacks(pack.inTemplate))
    def downInner(nme: String, tpl: TemplateEntity): (String, Package) = {
      tpl.inTemplate match {
        case inPkg: Package => (nme + ".html", inPkg)
        case inTpl => downInner(doName(inTpl) + "$" + nme, inTpl)
      }
    }
    val (file, pack) =
      tpl match {
        case p: Package => ("package.html", p)
        case _ => downInner(doName(tpl), tpl)
      }
    file :: downPacks(pack)
  }

  /** A relative link from this page to some destination class entity.
    * @param destEntity The class or object entity that the link will point to. */
  def relativeLinkTo(destClass: TemplateEntity): String =
    relativeLinkTo(templateToPath(destClass))

  /** A relative link from this page to some destination page in the Scaladoc site.
    * @param destPage The page that the link will point to. */
  def relativeLinkTo(destPage: HtmlPage): String = {
    relativeLinkTo(destPage.path)
  }

  /** A relative link from this page to some destination path.
    * @param destPath The path that the link will point to. */
  def relativeLinkTo(destPath: List[String]): String = {
    def relativize(from: List[String], to: List[String]): List[String] = (from, to) match {
      case (f :: fs, t :: ts) if (f == t) => // both paths are identical to that point
        relativize(fs, ts)
      case (fss, tss) =>
        List.fill(fss.length - 1)("..") ::: tss
    }
    relativize(thisPage.path.reverse, destPath.reverse).mkString("/")
  }

  def absoluteLinkTo(destPath: List[String]): String = {
    destPath.reverse.mkString("/")
  }

  /** Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
    * node sequence if it is not. */
  def commentToHtml(comment: Option[Comment]): NodeSeq =
    (comment map (commentToHtml(_))) getOrElse NodeSeq.Empty
  
  /** Transforms a comment into an styled HTML tree representing its body. */
  def commentToHtml(comment: Comment): NodeSeq =
    bodyToHtml(comment.body)

  def bodyToHtml(body: Body): NodeSeq =
    body.blocks flatMap (blockToHtml(_))

  def blockToHtml(block: Block): NodeSeq = block match {
    case Title(in, 1) => <h1>{ inlineToHtml(in) }</h1>
    case Title(in, 2) => <h2>{ inlineToHtml(in) }</h2>
    case Title(in, 3) => <h3>{ inlineToHtml(in) }</h3>
    case Title(in, _) => <h4>{ inlineToHtml(in) }</h4>
    case Paragraph(in) => <p>{ inlineToHtml(in) }</p>
    case Code(data) => <p><code>{ Unparsed(data) }</code></p>
    case UnorderedList(items) =>
      <ul>{items map { i => <li>{ blockToHtml(i) }</li>}}</ul>
    case OrderedList(items) =>
      <ol>{items map { i => <li>{ blockToHtml(i) }</li>}}</ol>
    case DefinitionList(items) =>
      <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
    case HorizontalRule() =>
      <hr/>
  }

  def inlineToHtml(inl: Inline): NodeSeq = inl match {
    //case URLLink(url, text) => <a href={url}>{if(text.isEmpty)url else inlineSeqsToXml(text)}</a>
    case Chain(items) => items flatMap (inlineToHtml(_))
    case Italic(in) => <i>{ inlineToHtml(in) }</i>
    case Bold(in) => <b>{ inlineToHtml(in) }</b>
    case Underline(in) => <u>{ inlineToHtml(in) }</u>
    case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
    case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
    case Link(raw) => Unparsed(raw)//error("link not supported") // TODO
    case Monospace(text) => <code>{ Unparsed(text) }</code>
    case Text(text) => Unparsed(text)
  }

  def typeToHtml(tpe: model.TypeEntity, hasLinks: Boolean): NodeSeq = {
    val string = tpe.name
    def toLinksOut(inPos: Int, starts: List[Int]): NodeSeq = {
      if (starts.isEmpty && (inPos == string.length))
        NodeSeq.Empty
      else if (starts.isEmpty)
        xml.Text(string.slice(inPos, string.length))
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        xml.Text(string.slice(inPos, starts.head)) ++ toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): NodeSeq = {
      val (tpl, width) = tpe.refEntity(inPos)
      (tpl match {
        case dtpl:DocTemplateEntity if hasLinks =>
          <a href={ relativeLinkTo(tpl) } class="extype" name={ dtpl.qualifiedName }>{
            string.slice(inPos, inPos + width)
          }</a>
        case tpl =>
          <span class="extype" name={ tpl.qualifiedName }>{ string.slice(inPos, inPos + width) }</span>
      }) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      xml.Text(string)
  }
  
}
