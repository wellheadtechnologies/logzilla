package core;
import java.util.List;

public interface LasFile {
    List getCurves();    
    List getHeaders();
    Header getVersionHeader();
    Header getWellHeader();
    Header getCurveHeader();
    Header getParameterHeader();
}    
    