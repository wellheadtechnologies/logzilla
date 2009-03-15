package core;
import java.util.List;

public interface Curve {
    Descriptor getDescriptor();
    List<Number> getLasData();
    Curve getIndex();
    String getMnemonic();
    Object getUnit();
    Object getData();
    String getDescription();
}