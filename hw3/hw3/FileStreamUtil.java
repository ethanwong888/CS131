// //fileStreamUtil.java
// //good - maybe try and move these functions out


// import java.util.zip.Deflater;
// import java.io.PushbackInputStream;
// import java.nio.ByteOrder;
// import java.io.IOException;
// import java.io.OutputStream;
// import java.nio.ByteBuffer;
// import java.nio.Buffer;



// public class FileStreamUtil {
  
  
//   public static boolean moreBytes(PushbackInputStream x){
//     try{
//       int res = x.read();
//       if (res > -1) {
//         x.unread(res);
//         return true;
//       } 
//       else {
//         return false;
//       }
//     } 
//     catch (Throwable e){ 
//       return false;
//     }
//   }

//   public static void perror(String err){
//     System.err.println(err);
//     System.exit(1);
//   }

//   public static void errorExit(Throwable err){
//     System.err.println(err.toString());
//     System.exit(1);
//   }



//   //adapted the starter code to create a slightly modified one
//   public final static int GZIP_MAGIC = 0x8b1f;
//   public static int writeHeader(OutputStream x) throws IOException { 
//     x.write(new byte[] {
//       (byte) GZIP_MAGIC,
//       (byte) (GZIP_MAGIC >> 8),
//       Deflater.DEFLATED,
//       0, 
//       0, 
//       0, 
//       0, 
//       0,
//       0,
//       0
//     });
//     return 10;
//   }

//   //wrote this before starter code was released oops... it's different but I think it works
//   public static int writeTrailer(OutputStream x, int value, int remain) throws IOException{
//     ByteBuffer buf = ByteBuffer.allocate(10);
//     buf.order(ByteOrder.LITTLE_ENDIAN);
//     buf.putInt(value);
//     buf.putInt(remain);
//     try{
//       //need to convert "ByteBuffer" to "byte" in order to write (basically just writing the stuff from putInt())
//       x.write(buf.array(), 0, 8); 
//     }
//     catch(Throwable err){
//       System.err.println(err.toString());
//       System.exit(1);
//     }
//     return 8;
//   }
// }