package core;
import java.util.List;

public interface LasFile {
    List getCurves();
    List getHeaders();
    Curve getIndex();
    Curve getCurve(String name);
    Header getVersionHeader();
    Header getWellHeader();
    Header getCurveHeader();
    Header getParameterHeader();
}    
    