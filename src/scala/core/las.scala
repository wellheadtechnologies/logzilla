package core

import core._
import scala.collection.jcl.Conversions._
import org.apache.commons.collections.list.UnmodifiableList
import java.util.{List, LinkedList}

object Compat {
  def unmodifiable[A](l: List[A]):List[A] = {
    UnmodifiableList.decorate(l).asInstanceOf[List[A]]
  }
  def toList[A](args: A*):List[A] = {
    val list = new LinkedList[A]()
    for(a <- args){
      list.add(a)
    }
    return Compat.unmodifiable(list)
  }
}

class DefaultLasFile(mheaders: List[Header], mcurves: List[Curve])
extends LasFile {
  private val headers = Compat.unmodifiable(mheaders)
  private val curves = Compat.unmodifiable(mcurves)

  override def equals(_that:Any):Boolean = {
    if(!_that.isInstanceOf[LasFile]) return false
    val that = _that.asInstanceOf[LasFile]
    if(this.headers != that.getHeaders) return false
    if(this.curves != that.getCurves) return false
    return true
  }

  override def getCurves = curves
  override def getHeaders = headers
  override def getVersionHeader = {
    headers.find(_.getType == "VersionHeader").get
  }
  override def getWellHeader = {
    headers.find(_.getType == "WellHeader").get
  }
  override def getCurveHeader = {
    headers.find(_.getType == "CurveHeader").get
  }
  override def getParameterHeader = {
    headers.find(_.getType == "ParameterHeader").get
  }
}

class DefaultCurve(descriptor:Descriptor, index:Curve, data:List[Number]) 
extends Curve {
  override def getDescriptor = descriptor
  override def getData = data
  override def getIndex = index
}

class DefaultHeader(htype:String, prefix:String, mdescriptors:List[Descriptor]) 
extends Header {
  val descriptors = Compat.unmodifiable(mdescriptors)
  override def getType = htype
  override def getPrefix = prefix
  override def getDescriptors = descriptors
}

class DefaultDescriptor(mnemonic:String, unit:Object, data:Object, description:String)
extends Descriptor {
  override def getMnemonic = mnemonic
  override def getUnit = unit
  override def getData = data
  override def getDescription = description
}


class VersionHeader(val version:String, val wrap:String) 
extends DefaultHeader("VersionHeader", "~V",
		      Compat.toList(
			new DefaultDescriptor("VERS", null, version, null),
			new DefaultDescriptor("WRAP", null, wrap, null)))

class WellHeader(descriptors:List[Descriptor]) 
extends DefaultHeader("WellHeader", "~W", descriptors)

class CurveHeader(descriptors:List[Descriptor])
extends DefaultHeader("CurveHeader", "~C", descriptors)

class ParameterHeader(descriptors:List[Descriptor])
extends DefaultHeader("ParameterHeader", "~P", descriptors)


