package core

import core._
import scala.collection.jcl.Conversions._
import org.apache.commons.collections.list.UnmodifiableList
import java.util.List

private[core] object Compat {
  def unmodifiable[A](l: List[A]):List[A] = {
    UnmodifiableList.decorate(l).asInstanceOf[List[A]]
  }
}

class DefaultLasFile(mheaders: List[Header], mcurves: List[Curve])
extends LasFile {
  private val headers = Compat.unmodifiable(mheaders)
  private val curves = Compat.unmodifiable(mcurves)

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

class DefaultCurve(descriptor:Descriptor, data:List[Number], index:Curve) 
extends Curve {
  override def getDescriptor = descriptor
  override def getData = data
  override def getIndex = index
}

class DefaultHeader(htype:String, prefix:String, mdescriptors:java.util.List[Descriptor]) 
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


