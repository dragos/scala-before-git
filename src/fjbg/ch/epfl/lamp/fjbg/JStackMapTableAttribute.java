// $Id: JCodeAttribute.java 5141 2003-08-25 15:17:27Z schinz $

package ch.epfl.lamp.fjbg;

import java.util.*;
import java.io.*;

/**
 * StackMapTable Attribute, according to JSR202. A stack map table allows
 * the VM to use the new (type-checking) verifier. A stack map table consists
 * of frames, each one giving the types of local variables and stack elements
 * at a certain position in bytecode. The table should contain a frame at the
 * beginning of each basic block.
 *
 * @author Hadrien Copponex, Iulian Dragos
 * @version 1.0
 */

public class JStackMapTableAttribute extends JAttribute {
	protected final JConstantPool cpool;
	
	protected ArrayList stackMapFrames = new ArrayList();
	
    public JStackMapTableAttribute(FJBGContext context, JMethod owner) {
        super(context, owner.getOwner());

		this.cpool = owner.getOwner().getConstantPool();
    }

	public void addFrame(JStackMapFrame frame) {
        stackMapFrames.add(frame);
	}

    public String getName() { return "StackMapTable"; }

    protected int getSize() {
        int size = 2;
		for(int i = 0; i < stackMapFrames.size(); i++)
			size += ((JStackMapFrame)stackMapFrames.get(i)).getSize();
		
		return size;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
		stream.writeShort(stackMapFrames.size());
		for(int i = 0; i < stackMapFrames.size(); i++)
			((JStackMapFrame)stackMapFrames.get(i)).writeContentsTo(stream);
    }

    public abstract static class JStackMapFrame {
        abstract public void writeContentsTo(DataOutputStream stream) throws IOException;
        abstract public int getSize();
    }

    public static class JSameFrame extends JStackMapFrame {
        protected int offset;

        public JSameFrame(int offset) {
            this.offset = offset;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            if (offset < 64)
                stream.writeByte(offset); // generate a SAME frame
            else {
                stream.writeByte(251); // generate a SAME_FRAME_EXTENDED frame
                stream.writeShort(offset);
            }
        }

        public int getSize() {
            if (offset < 64)
                return 1;
            return 3;
        }
    }

    public static class JSameLocalsOneStackItemFrame extends JStackMapFrame {
        protected int offset;
        protected JVerificationTypeInfo stack;

        public JSameLocalsOneStackItemFrame(int offset, JVerificationTypeInfo stack) {
            this.offset = offset;
            this.stack = stack;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            if (offset < 64)
                stream.writeByte(offset + 64);
            else {
                stream.writeByte(247);
                stream.writeShort(offset);
            }
            stack.writeContentsTo(stream);
        }

        public int getSize() {
            int stackSize = stack.getSize();
            if (offset < 64)
                return 1 + stackSize;
            return 3 + stackSize;
        }
    }

    public static class JChopFrame extends JStackMapFrame {
        protected int k;
        protected int offset;

        public JChopFrame(int offset, int k) {
            assert (k > 0 && k <= 3) : ("invalid k for chop stack map frame (must be 1, 2 or 3): " + k);

            this.k = k;
            this.offset = offset;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeByte(251 - k);
            stream.writeShort(offset);
        }

        public int getSize() {
            return 3;
        }
    }

    public static class JAppendFrame extends JStackMapFrame {
        protected int offset;
        protected JVerificationTypeInfo[] newLocals;

        public JAppendFrame(int offset, JVerificationTypeInfo[] newLocals) {
            assert (newLocals.length <=3) : ("too many new locals in append stack map frame (max 3): " + newLocals.length);

            this.offset = offset;
            this.newLocals = newLocals;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeByte(251 + newLocals.length);
            stream.writeShort(offset);
            for(int i = 0; i < newLocals.length; i++)
                newLocals[i].writeContentsTo(stream);
        }

        public int getSize() {
            int newLocalsSize = 0;
            for(int i = 0; i < newLocals.length; i++)
                newLocalsSize += newLocals[i].getSize();
            return newLocalsSize + 3;
        }
    }

    public static class JFullFrame extends JStackMapFrame {
        protected int offset;
        protected JVerificationTypeInfo[] locals;
        protected JVerificationTypeInfo[] stack;

        public JFullFrame(int offset, JVerificationTypeInfo[] locals, JVerificationTypeInfo[] stack) {
            this.offset = offset;
            this.locals = locals;
            this.stack = stack;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeByte(255); // generate a FULL frame
            stream.writeShort(offset);
            stream.writeShort(locals.length);
            for(int i = 0; i < locals.length; i++)
                locals[i].writeContentsTo(stream);
            stream.writeShort(stack.length);
            for(int i = 0; i < stack.length; i++)
                stack[i].writeContentsTo(stream);
        }

        public int getSize() {
            int totalSize = 0;
            for(int i = 0; i < locals.length; i++)
                totalSize += locals[i].getSize();
            for(int i = 0; i < stack.length; i++)
                totalSize += stack[i].getSize();
            return totalSize + 7; // 1 (frame type) + 2 (offset) + 2 (nLocals) + 2 (nStack)
        }
    }
}
