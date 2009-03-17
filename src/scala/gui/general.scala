package gui

import java.awt._
import java.awt.image.{BufferedImage}
import java.awt.geom.Rectangle2D
import javax.swing._
import javax.swing.border._
import java.util.{List,LinkedList}
import scala.collection.jcl.Conversions._
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart, ChartRenderingInfo}
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.data.xy.{AbstractXYDataset, XYDataset, 
			  XYSeries, XYSeriesCollection}
import core._
import core.Compat.fun2Run
import org.jdesktop.swingx.graphics.ShadowRenderer


class CurveList extends JList {
  val model = new DefaultListModel
  val curves = new LinkedList[Curve]
  setModel(model)
  val renderer = new IconListCellRenderer
  setCellRenderer(renderer)

  def addCurves(curves:List[Curve]){
    curves.foreach(addCurve)
  }
  
  def addCurve(curve:Curve){
    model.addElement(ChartUtil.curveToIcon(curve))
    curves.add(curve)
    SwingUtilities.invokeLater(() => {
      this.repaint()
    })
  }

  def getCurves:List[Curve] = Compat.unmodifiable(curves)

  def getSelectedCurves:List[Curve] = {
    val names = getSelectedValues.map{ 
      label => label.asInstanceOf[JLabel].getText()
    }
    val scurves = new LinkedList[Curve]
    for(name <- names){
      scurves.add(curves.find(_.getMnemonic == name).get)
    }
    return scurves
  }
    
}

class LasFileList extends JList {
  val model = new DefaultListModel
  val files = new LinkedList[LasFile]
  setModel(model)
  setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  setCellRenderer(new IconListCellRenderer)

  def addLasFiles(files:List[LasFile]){
    files.foreach(addLasFile)
  }

  def addLasFile(file:LasFile){
    model.addElement(new JLabel(file.getName))
    files.add(file)
    SwingUtilities.invokeLater(() => {
      this.repaint()
    })
  }

  def getLasFiles:List[LasFile] = Compat.unmodifiable(files)

  def getSelectedLasFile:LasFile = {
    val selected = getSelectedValue.asInstanceOf[JLabel]
    files.find(_.getName == selected.getText).get
  }

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


object ChartUtil {
  def time[A](msg: String)(f: => A):A = {
    val start = System.currentTimeMillis
    val result = f
    val end = System.currentTimeMillis
    println(msg + " " + (end - start))
    return result
  }

  def createChart(dataset:XYSeriesCollection, curve:Curve):JFreeChart = {
    val cname = curve.getMnemonic
    val iname = curve.getIndex.getMnemonic
    val chart = ChartFactory.createXYLineChart(
      cname + " Chart",
      iname, cname,
      dataset, PlotOrientation.HORIZONTAL,
      false, false, false)
    val plot = chart.getPlot.asInstanceOf[XYPlot]
    val renderer = plot.getRenderer
    renderer.setBasePaint(Color.blue)
    renderer.setSeriesPaint(0, Color.blue)
    plot.setBackgroundPaint(Color.white)
    chart
  }

  def createChart(curve:Curve):JFreeChart = {
    createChart(createDataset(curve), curve)
  }

  def createDataset(curve: Curve) = {
    val series = new XYSeries("Series")
    val ds = new XYSeriesCollection
    val index = curve.getIndex
    val cdata = curve.getLasData
    val idata = index.getLasData
    
    for(i <- 0 until idata.size){
      series.add(idata.get(i), 
		 cdata.get(i))
    }

    ds.addSeries(series)
    ds
  }

  def curveToImage(curve: Curve):BufferedImage = {
    createChart(curve).createBufferedImage(400,700)
  }

  def curveToIcon(curve: Curve):JLabel = {
    val chart = createChart(curve)
    val image = new BufferedImage(400, 700, BufferedImage.TYPE_INT_ARGB)

    var graphics = image.createGraphics
    chart.draw(graphics, new Rectangle2D.Double(0,0,400,700), null, null)    
    graphics.dispose()    

    val scaled = fastScale(image,64,64)

    val shadowRenderer = new ShadowRenderer()
    val shadow = shadowRenderer.createShadow(scaled)
    
    val stacked = stackImages(scaled, shadow)

    val icon = new ImageIcon(stacked)
    val name = curve.getMnemonic
    new JLabel(name, icon, SwingConstants.LEFT)
  }


  def stackImages(top:BufferedImage, bottom:BufferedImage) = {
    val graphics = bottom.createGraphics
    graphics.setComposite(AlphaComposite.SrcOver.derive(1.0f))
    graphics.drawImage(top, 0, 0, null)
    graphics.dispose()
    bottom
  }

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
      
