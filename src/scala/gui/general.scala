package gui

import java.lang.Math
import java.util.Collections
import java.awt._
import java.awt.event._
import java.awt.image.{BufferedImage}
import java.awt.geom.{Rectangle2D,Point2D}
import javax.swing._
import javax.swing.tree._
import javax.swing.border._
import java.util.{List,LinkedList,ArrayList}
import scala.collection.jcl.Conversions._
import org.jfree.chart._
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.entity.{ChartEntity,XYItemEntity}
import org.jfree.chart.{ChartMouseEvent, ChartMouseListener}
import org.jfree.data.xy.{AbstractXYDataset, XYDataset, 
			  XYSeries, XYSeriesCollection}
import org.jfree.ui.{RectangleEdge,TextAnchor}

import org.jdesktop.swingx.graphics.ShadowRenderer
import java.util.concurrent.locks.{ReadWriteLock,ReentrantReadWriteLock}
import org.jfree.chart.axis._
import org.jfree.chart.renderer.xy._
import org.jfree.chart.labels._
import org.jfree.chart.urls._

object ChartUtil {
  val currentTheme = new StandardChartTheme("JFree")
  def createXYLineChart(title:String,xAxisLabel:String,yAxisLabel:String, dataset:XYDataset,orientation:PlotOrientation,legend:Boolean) = {
    if (orientation == null) throw new IllegalArgumentException("Null 'orientation' argument.")
    val xAxis = new CustomNumberAxis(xAxisLabel)
    xAxis.setAutoRangeIncludesZero(false)
    xAxis.setInverted(true)
    val yAxis = new CustomNumberAxis(yAxisLabel)
    val renderer = new XYLineAndShapeRenderer(true, false)
    val plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setOrientation(orientation);
    val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend)
    currentTheme.apply(chart)
    chart
  }
}

class CustomNumberAxis(label:String) extends NumberAxis(label) {
  override protected def refreshTicksVertical(g2:Graphics2D, dataArea:Rectangle2D, edge:RectangleEdge):List[NumberTick] = {
    val result = new ArrayList[NumberTick]()
    result.clear()
    val tickLabelFont = getTickLabelFont()
    g2.setFont(tickLabelFont);
    if (isAutoTickUnitSelection()) {
      selectAutoTickUnit(g2, dataArea, edge)
    }
    val size = getTickUnit().getSize()
    val count = calculateVisibleTickCount()
    val lowestTickValue = calculateLowestVisibleTickValue()
    if (count <= ValueAxis.MAXIMUM_TICK_COUNT) {
      for(minorTick <- 1 until getMinorTickCount){
        val minorTickValue = lowestTickValue - getTickUnit().getSize() * minorTick / getMinorTickCount()
        if (getRange().contains(minorTickValue)){
          result.add(new NumberTick(TickType.MINOR, minorTickValue,
				    "", TextAnchor.TOP_CENTER, TextAnchor.CENTER,
				    0.0));
        }
      }
      for(i <- 1 until count){
        val currentTickValue = lowestTickValue + (i * size);
        var tickLabel:String = null
        val formatter = getNumberFormatOverride()
        if (formatter != null) {
          tickLabel = formatter.format(currentTickValue)
        }
        else {
          tickLabel = getTickUnit().valueToString(currentTickValue)
        }
        var anchor:TextAnchor = null
        var rotationAnchor:TextAnchor = null
        var angle = 0.0
        if (isVerticalTickLabels()) {
          if (edge == RectangleEdge.LEFT) {
            anchor = TextAnchor.BOTTOM_CENTER
            rotationAnchor = TextAnchor.BOTTOM_CENTER
            angle = -Math.PI / 2.0
          }
          else {
            anchor = TextAnchor.BOTTOM_CENTER
            rotationAnchor = TextAnchor.BOTTOM_CENTER
            angle = Math.PI / 2.0
          }
        } else {
          if (edge == RectangleEdge.LEFT) {
            anchor = TextAnchor.CENTER_RIGHT
            rotationAnchor = TextAnchor.CENTER_RIGHT
          } else {
            anchor = TextAnchor.CENTER_LEFT
            rotationAnchor = TextAnchor.CENTER_LEFT
          }
        }

        val tick = new NumberTick(currentTickValue,  tickLabel, anchor, rotationAnchor, angle)
        result.add(tick)
        val nextTickValue = lowestTickValue + ((i + 1)* size)
	for(minorTick <- 1 until getMinorTickCount){
          val minorTickValue = currentTickValue + (nextTickValue - currentTickValue) * minorTick / getMinorTickCount()
          if (getRange().contains(minorTickValue)){
            result.add(new NumberTick(TickType.MINOR,
                                      minorTickValue, "", TextAnchor.TOP_CENTER,
                                      TextAnchor.CENTER, 0.0));
          }
        }
      }
    }
    return result
  }
  override protected def refreshTicksHorizontal(g2:Graphics2D, dataArea:Rectangle2D, edge:RectangleEdge):List[NumberTick] = {
    val result = new ArrayList[NumberTick]
    val tickLabelFont = getTickLabelFont()
    g2.setFont(tickLabelFont)
    
    if (isAutoTickUnitSelection()) {
      selectAutoTickUnit(g2, dataArea, edge)
    }
    
    val size = getTickUnit().getSize()
    val count = calculateVisibleTickCount()
    val lowestTickValue = calculateLowestVisibleTickValue()
    
    if (count <= ValueAxis.MAXIMUM_TICK_COUNT) {
      for(minorTick <- 1 until getMinorTickCount){
        val minorTickValue = lowestTickValue - getTickUnit().getSize()  * minorTick / getMinorTickCount()
        if (getRange().contains(minorTickValue)){
          result.add(new NumberTick(TickType.MINOR, minorTickValue,
				    "", TextAnchor.TOP_CENTER, TextAnchor.CENTER,
				    0.0))
        }
      }
      for(i <- 0 until count){
        val currentTickValue = lowestTickValue + (i * size)
        var tickLabel:String = null
        val formatter = getNumberFormatOverride()
        if (formatter != null) {
          tickLabel = formatter.format(currentTickValue)
        }
        else {
          tickLabel = getTickUnit().valueToString(currentTickValue)
        }
        var anchor:TextAnchor = null
        var rotationAnchor:TextAnchor = null
        var angle = 0.0
        if (isVerticalTickLabels()) {
          anchor = TextAnchor.CENTER_RIGHT
          rotationAnchor = TextAnchor.CENTER_RIGHT
          if (edge == RectangleEdge.TOP) {
            angle = Math.PI / 2.0
          }
          else {
            angle = -Math.PI / 2.0
          }
        }
          else {
            if (edge == RectangleEdge.TOP) {
              anchor = TextAnchor.BOTTOM_CENTER
              rotationAnchor = TextAnchor.BOTTOM_CENTER
            }
            else {
              anchor = TextAnchor.TOP_CENTER
              rotationAnchor = TextAnchor.TOP_CENTER
            }
          }
	
        val tick = new NumberTick(currentTickValue, tickLabel, anchor, rotationAnchor, angle)
        result.add(tick)
        val nextTickValue = lowestTickValue + ((i + 1)* size)
	for(minorTick <- 1 until getMinorTickCount){
          val minorTickValue = currentTickValue + (nextTickValue - currentTickValue) * minorTick / getMinorTickCount()
          if (getRange().contains(minorTickValue)){
            result.add(new NumberTick(TickType.MINOR,
				      minorTickValue, "", TextAnchor.TOP_CENTER,
				      TextAnchor.CENTER, 0.0))
          }
        }
      }
    }
    return result
  }

  override protected def calculateLowestVisibleTickValue():Double = {
    val unit = getTickUnit.getSize
    val index = Math.ceil(getRange.getLowerBound / unit)
    return index * unit
  }

  override protected def calculateHighestVisibleTickValue():Double = {
    val unit = getTickUnit.getSize
    val index = Math.floor(getRange.getLowerBound / unit)
    return index * unit
  }

  override protected def calculateAnchorPoint(tick:ValueTick, cursor:Double, dataArea:Rectangle2D, edge:RectangleEdge) = {
    val insets = getTickLabelInsets()
    val result = new Array[Float](2)
    if (edge == RectangleEdge.TOP) {
      result(0) = valueToJava2D(tick.getValue(), dataArea, edge).floatValue
      result(1) = (cursor - insets.getBottom() - 2.0).floatValue
    }
    else if (edge == RectangleEdge.BOTTOM) {
      result(0) = valueToJava2D(tick.getValue(), dataArea, edge).floatValue
      result(1) = (cursor + insets.getTop() + 2.0).floatValue
    }
    else if (edge == RectangleEdge.LEFT) {
      result(0) = (cursor - insets.getLeft() - 2.0).floatValue
      result(1) = valueToJava2D(tick.getValue(), dataArea, edge).floatValue
    }
    else if (edge == RectangleEdge.RIGHT) {
      result(0) = (cursor + insets.getRight() + 2.0).floatValue
      result(1) = valueToJava2D(tick.getValue(), dataArea, edge).floatValue
    }
    result
  }

}

class CurveLabel(curve: Object, name:String, icon:ImageIcon, orientation:Int) extends JLabel(name, icon, orientation){
  def getCurve:Object = curve
}

trait NodePayload {
  def getFile:Object
}

class IconListCellRenderer extends JLabel with ListCellRenderer {

  override def getListCellRendererComponent(list:JList, value:Object, index:int, isSelected:boolean, hasFocus:boolean) = {
    val jlabel = value.asInstanceOf[JLabel]
    setOpaque(true)
    setText(jlabel.getText)
    setIcon(jlabel.getIcon)
    setComponentOrientation(list.getComponentOrientation)
    if(isSelected){
      setBackground(list.getSelectionBackground)
      setForeground(list.getSelectionForeground)
    }
    else{
      setBackground(list.getBackground)
      setForeground(list.getForeground)
    }
    if(hasFocus){
      setBorder(UIManager.getBorder("List.focusCellHighlightBorder"))
    }
    else {
      setBorder(new EmptyBorder(1,1,1,1))
    }
    this
  }

  override protected def paintComponent(g: Graphics){
    import RenderingHints._
    val g2 = g.asInstanceOf[Graphics2D]
    val hint = g2.setRenderingHint _
    hint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    hint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
    hint(KEY_RENDERING, VALUE_RENDER_QUALITY)
    super.paintComponent(g2)
  }

} 

object ImageUtil {
  def fastScale(img:BufferedImage, targetWidth: Int, targetHeight: Int) = {
    var itype:Int = BufferedImage.TYPE_INT_ARGB

    var ret = img
    var scratchImage:BufferedImage = null
    var g2:Graphics2D = null
    var (w,h) = (0,0)
      var prevW = ret.getWidth
    var prevH = ret.getHeight
    w = img.getWidth
    h = img.getHeight
    
    do {
      if(w > targetWidth){
	w /= 2
	if(w < targetWidth){
	  w = targetWidth
	}
      }
      
      if(h > targetHeight){
	h /= 2
	if(h < targetHeight){
	  h = targetHeight
	}
      }

      if(scratchImage == null){
	scratchImage = new BufferedImage(w,h,itype)
	g2 = scratchImage.createGraphics
      }

      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
			  RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2.drawImage(ret, 0, 0, w, h, 0, 0, prevW, prevH, null)
      prevW = w
      prevH = h
      
      ret = scratchImage
    } while(w != targetWidth || h != targetHeight)

    if(g2 != null){
      g2.dispose()
    }

    if(targetWidth != ret.getWidth || targetHeight != ret.getHeight) {
      scratchImage = new BufferedImage(targetWidth,
				       targetHeight, itype)
      g2 = scratchImage.createGraphics
      g2.drawImage(ret, 0, 0, null)
      g2.dispose()
      ret = scratchImage
    }
    
    ret
  }

}
