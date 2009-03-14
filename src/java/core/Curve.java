package core;
import java.util.List;

public interface Curve {
    Descriptor getDescriptor();
    List getLasData();
    Curve getIndex();
    String getMnemonic();
    Object getUnit();
    Object getData();
    String getDescription();
}