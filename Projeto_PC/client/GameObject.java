import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GameObject {
    private final double x;
    private final double y;
    private final double radius;
    private final double angle;
    private final int id;

    public GameObject(OtpErlangObject o) {
        var t = (OtpErlangTuple) o;
        OtpErlangDouble o1 = (OtpErlangDouble) t.elementAt(0);
        x = o1.doubleValue();
        OtpErlangDouble o2 = (OtpErlangDouble) t.elementAt(1);
        y = o2.doubleValue();
        OtpErlangDouble o3 = (OtpErlangDouble) t.elementAt(2);
        radius = o3.doubleValue();
        OtpErlangDouble o4 = (OtpErlangDouble) t.elementAt(3);
        angle = o4.doubleValue();
        OtpErlangLong o5 = (OtpErlangLong) t.elementAt(4);
        id = (int) o5.longValue();
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public double getRadius() {
        return radius;
    }

    public int getId() {
        return id;
    }

    public double getAngle() {
        return angle;
    }

    public boolean isPlayer() {
        return id > 0;
    }

    public boolean isSun() {
        return id == 0;
    }
}
