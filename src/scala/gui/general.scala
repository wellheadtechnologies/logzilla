package gui

import java.awt._
import javax.swing._
import java.util.{List,LinkedList}
import scala.collection.jcl.Conversions._
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.data.xy.{AbstractXYDataset, XYDataset, 
			  XYSeries, XYSeriesCollection}
import core._
import core.Compat.fun2Run


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


class IconListCellRenderer extends DefaultListCellRenderer {
  override def getListCellRendererComponent(list:JList, value:Object, index:int, isSelected:boolean, cellHasFocus:boolean) = {
    if(value.isInstanceOf[JLabel]){
      val jl = value.asInstanceOf[JLabel]
      setText(jl.getText)
      setIcon(jl.getIcon)
    } else if (value.isInstanceOf[String]) {
      setText(value.asInstanceOf[String])
    } else {
      setText(value.toString)
    }

    if(isSelected) {
      setBackground(list.getSelectionBackground)
    } else {
      setBackground(Color.white)
    }
    
    this
  }

  override protected def paintComponent(g: Graphics){
    import RenderingHints._
    val g2d = g.asInstanceOf[Graphics2D]
    val hint = g2d.setRenderingHint _
    hint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
    hint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)
    hint(KEY_RENDERING, VALUE_RENDER_QUALITY)
    super.paintComponent(g2d)
  }
}


object ChartUtil {
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
    return chart
  }

  def createChart(curve:Curve):JFreeChart = 
    createChart(createDataset(curve), curve)

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

  def curveToImage(curve: Curve):Image = {
    val dataset = createDataset(curve)
    val chart = createChart(dataset, curve)
    return chart.createBufferedImage(400,700)
  }

  def curveToIcon(curve: Curve):JLabel = {
    val scaled = curveToImage(curve).getScaledInstance(64,64, Image.SCALE_SMOOTH)
    val icon = new ImageIcon(scaled)
    val name = curve.getMnemonic
    return new JLabel(name, icon, SwingConstants.LEFT)
  }
    
}
      
