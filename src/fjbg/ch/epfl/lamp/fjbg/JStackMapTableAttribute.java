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
    protected final JCode code;
	protected final JConstantPool cpool;
	
	protected int curOffset = 0;
	
	protected JType[] currentLocals = null;
	protected JType[] currentStack = null;
	
	protected int nbCurrentLocals = 0;
	protected int nbCurrentStack = 0;
	
	protected ArrayList stackMapFrames = new ArrayList();
	
    public JStackMapTableAttribute(FJBGContext context, JClass clazz, JMethod owner) {
        super(context, clazz);
        this.code = owner.getCode();

        assert clazz == owner.getOwner();
		
		this.cpool = clazz.getConstantPool();
		
		// Init the current locals according to the method type (static or not) and its args
		currentLocals = new JType[owner.getMaxLocals()];
		JLocalVariable[] locals = owner.getLocalVariables();
		nbCurrentLocals = locals.length;
		for(int i = 0; i < nbCurrentLocals; i++)
			currentLocals[i] = locals[i].getType();
		
		// Init the stack to nothing
		currentStack = new JType[code.getMaxStackSize()];
    }

	// change this ctor
    public JStackMapTableAttribute(FJBGContext context,
                          JClass clazz,
                          Object owner,
                          String name,
                          int size,
                          DataInputStream stream)
        throws IOException {
        super(context, clazz);

        stream.readShort();     // skip max stack size
        stream.readShort();     // skip max locals

        this.code = context.JCode(clazz, (JMethod)owner, stream);

        int handlersCount = stream.readShort();
        for (int i = 0; i < handlersCount; ++i)
            code.addExceptionHandler(code.new ExceptionHandler(stream));

        List/*<JAttribute>*/ attributes =
            JAttribute.readFrom(context, clazz, owner, stream);
        Iterator attrIt = attributes.iterator();
        while (attrIt.hasNext())
            code.addAttribute((JAttribute)attrIt.next());

        assert name.equals(getName());
		
		this.cpool = clazz.getConstantPool();
    }
	
	protected int testLocalsEquality(JType[] locals) {
		int delta = locals.length - nbCurrentLocals;
		if (delta < -3 || delta > 3)
			return -100;
		
		int min = (locals.length < nbCurrentLocals) ? locals.length : nbCurrentLocals;
		
		for(int i = 0; i < min; i++) {
			if (currentLocals[i] != locals[i])
				return -100;
		}
		
		if (locals.length == nbCurrentLocals)
			return 0;
		
		int chop = min - nbCurrentLocals;
		if (min == locals.length && chop < 0 && chop >= -3)
			return chop;
		
		int append = locals.length - min;
		if (min == nbCurrentLocals && append > 0 && append <= 3)
			return append;
		
		return -100;
	}
	
	public void addFrame(int offset, JType[] locals, JType[] stack) {
		int offsetDelta = offset - curOffset;
		JStackMapFrame newFrame = null;
		int localsDelta = testLocalsEquality(locals);
		
		if (localsDelta == 0) {
			if (stack == null || stack.length == 0)
				newFrame = new JSameStackMapFrame(offsetDelta);
			else if (stack.length == 1)
				newFrame = new JSameLocalsOneStackItemStackMapFrame(offsetDelta, 
					JVerificationTypeInfo.getVerificationTypeInfo(stack[0], cpool));
		} else if (localsDelta < 0 && localsDelta != -100) {
			if (stack == null || stack.length == 0)
				newFrame = new JChopStackMapFrame(-localsDelta, offsetDelta);
		} else if (localsDelta > 0) {
			if (stack == null || stack.length == 0) {
				JType[] newLocals = new JType[localsDelta];
				System.arraycopy(locals, locals.length - localsDelta - 1, newLocals, 0, localsDelta);
				newFrame = new JAppendStackMapFrame(offsetDelta, JVerificationTypeInfo.convertJTypeArray(newLocals, cpool));
			}
		}
		
		if (newFrame == null)
			newFrame = new JFullStackMapFrame(offsetDelta,
				JVerificationTypeInfo.convertJTypeArray(locals, cpool),
				JVerificationTypeInfo.convertJTypeArray(stack, cpool));
		
		stackMapFrames.add(newFrame);
		
		curOffset = offset;
		currentLocals = new JType[currentLocals.length];
		System.arraycopy(locals, 0, currentLocals, 0, locals.length);
		nbCurrentLocals = locals.length;
		currentStack = new JType[currentStack.length];
		System.arraycopy(stack, 0, currentStack, 0, stack.length);
		nbCurrentStack = stack.length;
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
			((JStackMapFrame)stackMapFrames.get(i)).generate(stream);
    }
}

abstract class JStackMapFrame {
	abstract public void generate(DataOutputStream stream) throws IOException;
	abstract public int getSize();
}

class JSameStackMapFrame extends JStackMapFrame {
	protected int offset;
	
	public JSameStackMapFrame(int offset) {
		assert (offset >= 0 && offset <= 0xffff)
			: ("invalid offset for same stack map frame: " + offset);
		
		this.offset = offset;
	}
	
	public void generate(DataOutputStream stream) throws IOException {
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

class JSameLocalsOneStackItemStackMapFrame extends JStackMapFrame {
	protected int offset;
	protected JVerificationTypeInfo stack;
	
	public JSameLocalsOneStackItemStackMapFrame(int offset, JVerificationTypeInfo stack) {
		assert (offset >= 0 && offset <= 0xffff)
			: ("invalid offset for stack map frame: " + offset);
		
		this.offset = offset;
		this.stack = stack;
	}
	
	public void generate(DataOutputStream stream) throws IOException {
		if (offset < 64)
			stream.writeByte(offset + 64);
		else {
			stream.writeByte(247);
			stream.writeShort(offset);
		}
		stack.generate(stream);
	}
	
	public int getSize() {
		int stackSize = stack.getSize();
		if (offset < 64)
			return 1 + stackSize;
		return 3 + stackSize;
	}
}

class JChopStackMapFrame extends JStackMapFrame {
	protected int k;
	protected int offset;
	
	public JChopStackMapFrame(int k, int offset) {
		assert (k > 0 && k <= 3) : ("invalid k for chop stack map frame (must be 1, 2 or 3): " + k);
		assert (offset >= 0 && offset <= 0xffff) : ("invalid offset for stack map frame: " + offset);
		
		this.k = k;
		this.offset = offset;
	}
	
	public void generate(DataOutputStream stream) throws IOException {
		stream.writeByte(251 - k);
		stream.writeShort(offset);
	}
	
	public int getSize() {
		return 3;
	}
}

class JAppendStackMapFrame extends JStackMapFrame {
	protected int offset;
	protected JVerificationTypeInfo[] newLocals;
	
	public JAppendStackMapFrame(int offset, JVerificationTypeInfo[] newLocals) {
		assert (offset >= 0 && offset <= 0xffff) : ("invalid offset for stack map frame: " + offset);
		assert (newLocals.length <=3) : ("too much new locals defined for append stack map frame (max 3): " + newLocals.length);
		
		this.offset = offset;
		this.newLocals = newLocals;
	}
	
	public void generate(DataOutputStream stream) throws IOException {
		stream.writeByte(251 + newLocals.length);
		stream.writeShort(offset);
		for(int i = 0; i < newLocals.length; i++)
			newLocals[i].generate(stream);
	}
	
	public int getSize() {
		int newLocalsSize = 0;
		for(int i = 0; i < newLocals.length; i++)
			newLocalsSize += newLocals[i].getSize();
		return newLocalsSize + 3;
	}
}

class JFullStackMapFrame extends JStackMapFrame {
	protected int offset;
	protected JVerificationTypeInfo[] locals;
	protected JVerificationTypeInfo[] stack;
	
	public JFullStackMapFrame(int offset, JVerificationTypeInfo[] locals, JVerificationTypeInfo[] stack) {
		assert (offset >= 0 && offset <= 0xffff)
			: ("invalid offset for stack map frame: " + offset);
		
		this.offset = offset;
		this.locals = locals;
		this.stack = stack;
	}
	
	public void generate(DataOutputStream stream) throws IOException {
		stream.writeByte(255); // generate a FULL frame
		stream.writeShort(offset);
		stream.writeShort(locals.length);
		for(int i = 0; i < locals.length; i++)
			locals[i].generate(stream);
		stream.writeShort(stack.length);
		for(int i = 0; i < stack.length; i++)
			stack[i].generate(stream);
	}
	
	public int getSize() {
		int totalSize = 0;
		for(int i = 0; i < locals.length; i++)
			totalSize += locals[i].getSize();
		for(int i = 0; i < stack.length; i++)
			totalSize += stack[i].getSize();
		return totalSize + 5;
	}
}

abstract class JVerificationTypeInfo {
	abstract public void generate(DataOutputStream stream) throws IOException;
	public int getSize() { return 1; }
	
	public static JVerificationTypeInfo ITEM_Top = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(0); }
	};
	public static JVerificationTypeInfo ITEM_Integer = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(1); }
	};
	public static JVerificationTypeInfo ITEM_Float = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(2); }
	};
	public static JVerificationTypeInfo ITEM_Long = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(4); }
	};
	public static JVerificationTypeInfo ITEM_Double = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(3); }
	};
	public static JVerificationTypeInfo ITEM_Null = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(5); }
	};
	public static JVerificationTypeInfo ITEM_UninitializedThis = new JVerificationTypeInfo() {
		public void generate(DataOutputStream stream) throws IOException { stream.writeByte(6); }
	};
	static class JObjectVerificationTypeInfo extends JVerificationTypeInfo {
		protected int cpool_index;
		protected JObjectVerificationTypeInfo(int cpool_index) {
			super();
			this.cpool_index = cpool_index;
		}
		public void generate(DataOutputStream stream) throws IOException {
			stream.writeByte(7);
			stream.writeShort(cpool_index);
		}
		public int getSize() {
			return 3;
		}
	}
	
	protected JVerificationTypeInfo() {} // used only by static instances...
	
	public static JVerificationTypeInfo getVerificationTypeInfo(JType type, JConstantPool cpool) {
		if (type == JType.INT) return ITEM_Integer;
		if (type == JType.FLOAT) return ITEM_Float;
		if (type == JType.LONG) return ITEM_Long;
		if (type == JType.DOUBLE) return ITEM_Double;
		//if (type == JObjectType.JAVA_NULL_VALUE) return ITEM_Null;
		if (type instanceof JObjectType) {
			JObjectType objtype = (JObjectType)type;
			int cpool_idx = cpool.addClass(objtype.getName());
			return new JObjectVerificationTypeInfo(cpool_idx);
		}
		return null;
	}
	
	public static JVerificationTypeInfo[] convertJTypeArray(JType[] types, JConstantPool cpool) {
		JVerificationTypeInfo[] result = new JVerificationTypeInfo[types.length];
		for(int i = 0; i < result.length; i++) {
			result[i] = getVerificationTypeInfo(types[i], cpool);
		}
		return result;
	}
}
