/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package io

import java.io.{ FileInputStream, InputStream, IOException, File => JFile }
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.channels.{FileChannel, ReadableByteChannel, Channels}
import java.nio.charset.{CharsetDecoder, CoderResult}
import scala.tools.nsc.reporters._

/** This class implements methods to read and decode source files. */
class SourceReader(decoder: CharsetDecoder, reporter: Reporter) {

  import SourceReader.{decode, flush}

  //########################################################################
  // Private Fields

  /** The input byte buffer (small enough to fit in cache) */
  private val bytes: ByteBuffer = ByteBuffer.allocate(0x4000)

  /** The output character buffer */
  private var chars: CharBuffer = CharBuffer.allocate(0x4000)

  private def reportEncodingError(filename:String) = {
    reporter.error(util.NoPosition, 
                   "IO error while decoding "+filename+" with "+decoder.charset()+"\n"+
                   "Please try specifying another one using the -encoding option")
  }

  //########################################################################
  // Public Methods

  /** Reads the file with the specified name. */
  def read(filename: String): Array[Char]= read(new JFile(filename))

  /** Reads the specified file. */
  def read(file: JFile): Array[Char] = {
    val c = new FileInputStream(file).getChannel
    try {
      read(c)
    } catch {
      case e:Exception =>
        if (true) e.printStackTrace
        reportEncodingError(file.toString())
        new Array[Char](0)
    } finally {
      c.close()
    }
  }

  /** Reads the specified file.
   *
   *  @param file ...
   *  @return     ...
   */
  def read(file: AbstractFile): Array[Char] = {
    file match {
      case p:PlainFile =>
        read(p.file)                                                     // bq: (!!!)
      case z:ZipArchive#FileEntry => 
        val c = Channels.newChannel(z.getArchive.getInputStream(z.entry))
        read(c)
      case _ =>
        val b = ByteBuffer.wrap(file.toByteArray)
        try {
          read(b)
        } catch {
          case e:Exception =>
            if (true) e.printStackTrace
            reportEncodingError(file.toString())
            new Array[Char](0)
        }
    }
  }

  /** Reads the specified byte channel. */
  protected def read(input: ReadableByteChannel): Array[Char] = {
    val decoder: CharsetDecoder = this.decoder.reset()
    val bytes: ByteBuffer = this.bytes; bytes.clear()
    var chars: CharBuffer = this.chars; chars.clear()
    var endOfInput: Boolean = false
    while (!endOfInput ) {
      endOfInput = input.read(bytes) < 0
      bytes.flip()
      chars = decode(decoder, bytes, chars, endOfInput)
    }
    terminate(flush(decoder, chars))
  }

  /** Reads the specified byte buffer. */
  protected def read(bytes: ByteBuffer): Array[Char] = {
    val decoder: CharsetDecoder = this.decoder.reset()
    val chars: CharBuffer = this.chars; chars.clear()
    terminate(flush(decoder, decode(decoder, bytes, chars, true)))
  }

  //########################################################################
  // Private Methods

  /**
   * Sets the specified char buffer as the new output buffer and
   * reads and returns its content.
   */
  private def terminate(chars: CharBuffer): Array[Char] = {
    val result = new Array[Char](chars.length())
    chars.get(result)
    this.chars = chars
    result
  }

}

object SourceReader {

  /**
   * Decodes the content of the specified byte buffer with the
   * specified decoder into the specified char buffer, allocating
   * bigger ones if necessary, then compacts the byte buffer and
   * returns the last allocated char buffer. The "endOfInput"
   * argument indicates whether the byte buffer contains the last
   * chunk of the input file.
   */
  def decode(decoder: CharsetDecoder, bytes: ByteBuffer, chars: CharBuffer,
             endOfInput: Boolean): CharBuffer =
  {
    val result: CoderResult = decoder.decode(bytes, chars, endOfInput)
    if (result.isUnderflow()) {
      bytes.compact()
      chars
    } else {
      if (result.isError()) throw new IOException(result.toString())
      assert(result.isOverflow())
      decode(decoder, bytes, increaseCapacity(chars), endOfInput)
    }
  }

  /**
   * Flushes the specified decoder into the specified char buffer,
   * allocating bigger ones if necessary and then flips and returns
   * the last allocated char buffer.
   */
  def flush(decoder: CharsetDecoder, chars: CharBuffer): CharBuffer = {
    val result: CoderResult = decoder.flush(chars)
    if (result.isUnderflow()) {
      chars.flip()
      chars
    } else {
      if (result.isError()) throw new IOException(result.toString())
      assert(result.isOverflow())
      flush(decoder, increaseCapacity(chars))
    }
  }

  /**
   * Flips the specified buffer and returns a new one with the same
   * content but with an increased capacity.
   */
  private def increaseCapacity(buffer: CharBuffer): CharBuffer = {
    buffer.flip()
    val capacity = 2 * buffer.capacity()
    CharBuffer.allocate(capacity).put(buffer)
  }

}
