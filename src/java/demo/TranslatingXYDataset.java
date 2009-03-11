package demo;
import org.jfree.data.xy.AbstractXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.general.DatasetChangeListener;
import org.jfree.data.general.DatasetChangeEvent;

public class TranslatingXYDataset
    extends AbstractXYDataset 
    implements XYDataset, DatasetChangeListener {

    private XYDataset underlying;
    private double translate;

    public TranslatingXYDataset(XYDataset underlying){
	this.underlying = underlying;
	this.underlying.addChangeListener(this);
	this.translate = 0.0;
    }

    public double getTranslate() {
	return this.translate;
    }

    public void setTranslate(double t){
	this.translate = t;
	fireDatasetChanged();
    }

    public int getItemCount(int series){
	return this.underlying.getItemCount(series);
    }

    public double getXValue(int series, int item) {
	return this.underlying.getXValue(series, item) + translate;
    }

    public Number getX(int series, int item) {
	return new Double(getXValue(series, item));
    }

    public Number getY(int series, int item) {
	return new Double(getYValue(series, item));
    }

    public double getYValue(int series, int item) {
	return this.underlying.getYValue(series, item);
    }

    public int getSeriesCount() {
	return this.underlying.getSeriesCount();
    }

    public Comparable getSeriesKey(int series) {
	return underlying.getSeriesKey(series);
    }

    public void datasetChanged(DatasetChangeEvent event) {
	// underlying dataset has changed, so notify our listeners
	this.fireDatasetChanged();
    }
}

								    