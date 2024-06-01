import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;

public class Util {
    public static final OtpErlangAtom OK = new OtpErlangAtom("ok");
    public static final OtpErlangAtom WIN = new OtpErlangAtom("win");
    public static final OtpErlangAtom LOSS = new OtpErlangAtom("loss");
    public static final OtpErlangAtom DRAW = new OtpErlangAtom("draw");
    public static final OtpErlangAtom FUEL = new OtpErlangAtom("fuel");
    public static final OtpErlangAtom UPDATE = new OtpErlangAtom("update");
    public static final OtpErlangAtom START = new OtpErlangAtom("start");
    public static final OtpErlangAtom LEAVE = new OtpErlangAtom("leave");
    public static final OtpErlangAtom LOGIN = new OtpErlangAtom("login");
    public static final OtpErlangAtom SIGNUP = new OtpErlangAtom("signup");
    public static final OtpErlangAtom DELETE = new OtpErlangAtom("delete");
    public static final OtpErlangAtom LOGOUT = new OtpErlangAtom("logout");
    public static final OtpErlangAtom LEFT = new OtpErlangAtom("left");
    public static final OtpErlangAtom RIGHT = new OtpErlangAtom("right");
    public static final OtpErlangAtom UP = new OtpErlangAtom("up");
    public static final OtpErlangAtom TOP = new OtpErlangAtom("top");
    public static final OtpErlangAtom JOIN = new OtpErlangAtom("join");

    private static OtpInputStream read(InputStream in) {
        try {
            byte[] header = new byte[4];
            in.read(header);
            int length = ByteBuffer.wrap(header).getInt(0);
            byte[] data = new byte[length];
            in.read(data);
            return new OtpInputStream(data);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static OtpErlangTuple readTuple(InputStream in) {
        try {
            return new OtpErlangTuple(read(in));
        } catch (OtpErlangDecodeException e) {
            throw new RuntimeException(e);
        }
    }

    public static void send(OtpErlangObject o, OutputStream out) {
        var oos = new OtpOutputStream();
        o.encode(oos);
        byte[] data = oos.toByteArray();
        ByteBuffer header = ByteBuffer.allocate(4).putInt(data.length + 1);
        try {
            out.write(header.array());
            out.write(131);
            out.write(data);
            oos.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
