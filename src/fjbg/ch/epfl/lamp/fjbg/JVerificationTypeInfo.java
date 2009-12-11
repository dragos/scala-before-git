package ch.epfl.lamp.fjbg;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * A verification type info, as used by the StackMapTableAttribute. See JSR202.
 */
public abstract class JVerificationTypeInfo {
	abstract public void writeContentsTo(DataOutputStream stream) throws IOException;
	public int getSize() { return 1; }

	public static JVerificationTypeInfo ITEM_Top = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(0); }
    };
	public static JVerificationTypeInfo ITEM_Integer = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(1); }
    };
	public static JVerificationTypeInfo ITEM_Float = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(2); }
    };
	public static JVerificationTypeInfo ITEM_Long = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(4); }
    };
	public static JVerificationTypeInfo ITEM_Double = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(3); }
    };
	public static JVerificationTypeInfo ITEM_Null = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(5); }
    };
	public static JVerificationTypeInfo ITEM_UninitializedThis = new JVerificationTypeInfo() {
		public void writeContentsTo(DataOutputStream stream) throws IOException { stream.writeByte(6); }
    };

    public static class JObjectVerificationTypeInfo extends JVerificationTypeInfo {
		protected int cpool_index;
        
		public JObjectVerificationTypeInfo(int cpool_index) {
			super();
			this.cpool_index = cpool_index;
		}
		public void writeContentsTo(DataOutputStream stream) throws IOException {
			stream.writeByte(7);
			stream.writeShort(cpool_index);
		}
		public int getSize() {
			return 3;
		}
    }

    public static class JUninitializedTypeInfo extends JVerificationTypeInfo {
        private int offset = 0;

        /**
         * @param offset the bytecode offset of this frame
         * @param pos the bytecode position of the corresponding 'new' instruction 
         */
        public JUninitializedTypeInfo(int offset) {
            this.offset = offset;
        }

        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.write(8);
            stream.writeShort(offset);
        }

        public int getSize() {
            return 3;
        }
    }
}
