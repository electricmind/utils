package ru.wordmetrix.smartfile

import java.io.{File, FileInputStream, FileOutputStream, FileWriter, InputStream, ObjectInputStream, ObjectOutputStream, OutputStream, Writer}

import ru.wordmetrix.vector._

import scala.io.Source

object SmartFile {

  implicit def fromFile(f: File) = new SmartFile(f)

  implicit def fromString(s: String) = new SmartFile(new File(s))

  //implicit def string2Array(s: String): Array[Byte] = s.toArray.map(c => c.toByte)
  implicit def toFile(sf: SmartFile) = sf.file
}

abstract trait SmartFileOperations {
  val inputstream: InputStream
  val outputstream: OutputStream
  val writer: Writer

  def write(ss: Traversable[String]): Unit =
    write(ss.mkString("\n"))

  def write(s: String): Unit = {
    val fn = writer
    fn.write(s)
    fn.close()
  }

  def write(a: Array[Byte]) = {
    val fn = outputstream
    fn.write(a)
    fn.close()
  }

  def read() = {
    val fin = inputstream
    val buf = new Array[Byte](fin.available())
    fin.read(buf)
    fin.close()
    buf
  }

  def cache[O](ob: => O) = try {
    val fi = new ObjectInputStream(inputstream)
    val o = fi.readObject().asInstanceOf[O]
    fi.close()
    o
  } catch {
    case x: Throwable =>
      val fo = new ObjectOutputStream(outputstream)
      val o = ob
      fo.writeObject(o)
      fo.close()
      o
  }

  def readLines() =
    new Iterator[String] {
      private val source = Source.fromInputStream(inputstream)
      private val lines = source.getLines()

      def hasNext: Boolean =
        if (lines.hasNext) true
        else {
          source.close()
          false
        }

      def next(): String = lines.next
    }


  def copyTo(foutname: SmartFile) = {
    foutname.write(this.read())
  }

  def write(v: Vector[String]): Unit = write(v.toList.sortBy(-_._2).map {
    case (x, y) => "%-40s : %4.3f".format(x, /*xstring2word.inverted(x)x,*/ y)
  })

}

abstract class SmartFileBase(val file: File) {
  def /(f: SmartFile) = new SmartFile(new File(file, f.file.toString))

  def /(f: String) = new SmartFile(new File(file, f))

  def /(f: Int) = new SmartFile(new File(file, f.toString))

  def /(f: File) = new SmartFile(new File(file, f.toString))

  lazy val preparefile = {
    Option(file.getParentFile()) foreach {
      _.mkdirs()
    }
    file
  }

  lazy val outputstream = new FileOutputStream(preparefile)

  val writer: FileWriter
  //    def reader = new FileReader(file)

  lazy val inputstream = new FileInputStream(file)

  override def toString = file.toString

  def replace() = new SmartFile(file)

  def append() = new SmartFileAppend(file)
}

class SmartFileAppend(file: File)
  extends SmartFileBase(file) with SmartFileOperations {
  val writer = new FileWriter(preparefile, true)
}

class SmartFile(file: File)
  extends SmartFileBase(file) with SmartFileOperations {
  lazy val writer = new FileWriter(preparefile)
}
