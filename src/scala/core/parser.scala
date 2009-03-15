package core

import scala.collection.jcl.Conversions._
import java.util.{List,LinkedList,StringTokenizer}
import java.io.{File, FileReader, BufferedReader}
import scala.collection.mutable.{Queue,ListBuffer}
import java.math.BigDecimal

object DefaultLasParser extends LasParser {

  private val header_prefixes = Map("~V" -> "VersionHeader",
				    "~W" -> "WellHeader",
				    "~C" -> "CurveHeader",
				    "~P" -> "ParameterHeader")

  private val white_space = "\n\r\t "

  override def parseLasFile(path:String) = {
    parseLasFile(new File(path))
  }

  override def parseLasFile(file:File) = {
    var reader = new BufferedReader(new FileReader(file))
    try {
      val headers = parseHeaders(reader)
      val curveHeader = headers.find(_.getType == "CurveHeader").get
      val (index, curves) = parseCurves(curveHeader, reader)
      new DefaultLasFile(headers, index, curves)
    } finally {
      reader.close()
    }
  }

  private def parseCurves(curveHeader:Header,reader:BufferedReader) = {
    val descriptors = curveHeader.getDescriptors()
    val n = descriptors.size()
    val data:Queue[Number] = parseData(reader)
    val cdatas = new ListBuffer[List[Number]]
    for(_ <- 0 until n){
      cdatas += new LinkedList[Number]()
    }
    while(!data.isEmpty){
      (0 until n).foreach(i => {
	val d = data.dequeue
	cdatas(i).add(d)
      })
    }	
    val index = new DefaultCurve(descriptors(0), null, cdatas(0))
    val final_curves = new LinkedList[Curve]()
    for(i <- 1 until n){
      final_curves.add(new DefaultCurve(descriptors(i), index, cdatas(i)))
    }
    (index, final_curves)
  }

  private def parseHeaders(reader:BufferedReader):List[Header] = {
    val line = next_line(reader)
    val headers = new LinkedList[Header]()
    var prefix = line.trim.take(2)
    for(i <- 0 until 4){
      val (prefix_line, descriptors) = parseDescriptors(reader)
      val htype = header_prefixes(prefix)
      headers.add(new DefaultHeader(htype, prefix, descriptors))
      prefix = prefix_line.trim.take(2)
    }
    return headers
  }

  private def parseData(reader:BufferedReader):Queue[Number] = {
    val data = new Queue[Number]()
    while(reader.ready()) {
      val row = reader.readLine().trim().replaceAll("\t", " ")
      var tokenizer:StringTokenizer = new StringTokenizer(row, " ")
      while(tokenizer.hasMoreTokens){
	val token = tokenizer.nextToken()
	try {
	  data += new BigDecimal(token)
	} catch {
	  case (e:NumberFormatException) => 
	    println("number format exception : " + token)
	    e.printStackTrace()
	    throw e
	}
      }
    }
    return data
  }    
  
  private def parseDescriptors(reader:BufferedReader) = {
    var continue = true
    val descriptors = new LinkedList[Descriptor]
    var next_prefix:String = null
    while(reader.ready() && continue){
      val line = next_line(reader)
      if(hasPrefix(line)){
	continue = false
	next_prefix = line
      }
      else {
	descriptors.add(parseDescriptor(line))
      }
    }
    (next_prefix, descriptors)
  }

  private def parseDescriptor(line1:String):Descriptor = {
    val dot = line1.indexOf('.')
    val mnemonic = line1.substring(0, dot)
    val line2 = line1.substring(dot+1)
    val space = line2.indexOf(' ')
    val unit = line2.substring(0, space)
    val line3 = line2.substring(space+1)
    val colon = line3.lastIndexOf(':')
    val data = line3.substring(0, colon)
    val description = line3.substring(colon+1)
    return new DefaultDescriptor(mnemonic.trim, unit.trim, data.trim, description.trim)
  }

  private def isComment(line:String) = line.startsWith("#") || line.trim.startsWith("#")

  private def hasPrefix(line:String) = {
    ((header_prefixes.keySet).exists(line.startsWith) || 
     line.startsWith("~A"))

  }

  private def next_line(reader:BufferedReader) = {
    var line = reader.readLine()
    while(isComment(line)){
      line = reader.readLine()
    }
    line
  }

}
