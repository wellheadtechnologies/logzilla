package core;
import java.util.List;

public interface LasFile {
    String getName();
    List<Curve> getCurves();
    List<Header> getHeaders();
    Curve getIndex();
    Curve getCurve(String name);
    Header getVersionHeader();
    Header getWellHeader();
    Header getCurveHeader();
    Header getParameterHeader();
}    
    