package core;
import java.util.List;
import java.math.BigDecimal;

public interface Curve {
    Descriptor getDescriptor();
    List<BigDecimal> getLasData();
    Curve getIndex();
    String getMnemonic();
    Object getUnit();
    Object getData();
    String getDescription();
}