import java.util.zip.Deflater;
import java.io.PushbackInputStream;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.zip.CRC32;
import java.io.FilterOutputStream;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;
import java.nio.ByteBuffer;
import java.nio.Buffer;
import java.nio.ByteOrder;

public class parallelThread{ 
  private static final int dict = 32768;
  private PushbackInputStream innie;
  private OutputStream outtie;
  private CRC32 crc = new CRC32();
  private JHelper con;
  

  public parallelThread(InputStream x, OutputStream y, JHelper z){
    innie = new PushbackInputStream(x);
    outtie = y;
    con = z;
  }

  //adapted the starter code to create a slightly modified one
  public final static int GZIP_MAGIC = 0x8b1f;
  public static int writeHeader(OutputStream x) throws IOException { 
    x.write(new byte[] {
      (byte) GZIP_MAGIC,
      (byte) (GZIP_MAGIC >> 8),
      Deflater.DEFLATED,
      0, 
      0, 
      0, 
      0, 
      0,
      0,
      0
    });
    return 10;
  }

  //wrote this before starter code was released oops... it's different but I think it works
  public static int writeTrailer(OutputStream x, int value, int remain) throws IOException{
    ByteBuffer buf = ByteBuffer.allocate(10);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.putInt(value);
    buf.putInt(remain);
    try{
      //need to convert "ByteBuffer" to "byte" in order to write (basically just writing the stuff from putInt())
      x.write(buf.array(), 0, 8); 
    }
    catch(Throwable err){
      System.err.println(err.toString());
      System.exit(1);
    }
    return 8;
  }


  public void inputCompressor(){
    int keepAliveTime = 1;
    float load = 0.75f;
    int poolSize = con.getProcessID();
    int concurrencyLevel = poolSize;
    //queue is used to hold tasks before they are eecuted (think of assembly line / waiting area for threads)
    ArrayBlockingQueue <Runnable> waitQueue = new ArrayBlockingQueue<Runnable>(poolSize);
    //use ThreadPoolExecutor to keep threads organized 
    ThreadPoolExecutor pool = new ThreadPoolExecutor(poolSize, poolSize, keepAliveTime, TimeUnit.SECONDS, waitQueue);

    //hash maps to keep track of reads and writes from children threads
    LinkedList <Runnable> threadList = new LinkedList<Runnable>();
    ByteArrayOutputStream notCompressed = new ByteArrayOutputStream();
    ConcurrentMap <Integer, byte[]> mapDict = new ConcurrentHashMap <Integer,byte[]> (poolSize, load, concurrencyLevel);
    ConcurrentMap <Integer, byte[]> mapUncompress = new ConcurrentHashMap <Integer,byte[]> (poolSize, load, concurrencyLevel);
    ConcurrentMap <Integer, byte[]> mapCompress = new ConcurrentHashMap <Integer,byte[]> (poolSize, load, concurrencyLevel);

    int blockSize = con.getBlockSize();
    int idx = 0;
    boolean first = true;
    boolean last = true;
    byte[] dictionary;
    byte[] buf1 = new byte[blockSize];
    int nextByte = 0;
    int threadID = 0;
    int readAll = 0;
    int poolSizeMinus = poolSize - 1;
    int cStart = 1;
    int uStart = poolSize;
    boolean lastBlock = false;
   
    try{
      //Try to write header, exit if there is error
      try{
        writeHeader(outtie);
      } 
      catch(Throwable err){
        System.err.println(err.toString());
        System.exit(1);
      }


      while ((poolSizeMinus > threadID) && ((nextByte = innie.read(buf1, readAll, blockSize - readAll)) > -1)){
        readAll += nextByte;
        if ((readAll < blockSize) && (innie.available() != 0)){
          //continue -- break current iteration of while loop, proceed to next one
          continue;
        }
        nextByte = readAll;
        readAll = 0;
        
        //set everything up for this iteration
        byte[] readbytes = new byte[nextByte];
        idx = con.addProcessCounter();
        notCompressed.write(buf1, 0, nextByte);
        System.arraycopy(buf1, 0, readbytes, 0, nextByte);
        if (innie.available() != 0){
          last = false;
        }
        if (innie.available() == 0){
          last = true;
        }

        //create a new thread for this block -- don't do anything with it yet though
        threadList.add(new pThread(con, readbytes, mapDict, idx, nextByte, first, last, innie, mapCompress, mapUncompress));
        //created another thread, increment
        threadID += 1;

        if ((last == false) && (blockSize == nextByte)){
          int s = blockSize - dict;
          dictionary = new byte[dict];
          System.arraycopy(buf1, s, dictionary, 0, dict);
          mapDict.put(idx, dictionary); 
        }

        //after first loop, no longer on first block
        first = false;
      }


      //pops thread off of threadList for execution
      while(threadList.size() >= 1){
        pool.execute(threadList.remove());
      }
        

      while ((lastBlock == false) && ((mapCompress.size() > 0) || (pool.getActiveCount() > 0) || (mapUncompress.size() > 0))){
        //write the uncompressed data to buffer
        if (mapUncompress.containsKey(uStart)){
          //create temporary buffer "uByte" to hold contents at "uStart" and write into "notCompressed"
          byte[] uByte = mapUncompress.get(uStart);
          mapUncompress.remove(uStart);
          notCompressed.write(uByte);
          //move on to the next block
          uStart += 1;
        }

        //write the compressed data to output
        if (mapCompress.containsKey(cStart)){
          //create temporary buffer "cByte" to hold contents at "cStart" and write into "outtie"
          byte[] cByte = mapCompress.get(cStart);
          mapCompress.remove(cStart);
          outtie.write(cByte);
          
          //cStart is currently at the final block
          if (con.getFinalBlockIdx() == cStart){
            lastBlock = true;
          }
          //move on to the next block
          cStart += 1;
        }
      }
      //drain the pool... after everything has finished
      pool.shutdown();


      //compute checksum from the "uncompressed" array -- pretty much the same as singleThread version
      byte[] ncArray = notCompressed.toByteArray();
      byte[] blank = {0x03, 0x00};

      //array is empty, just print an empty block
      if (ncArray.length == 0){
        outtie.write(blank, 0, blank.length);
      }
      //update crc to check that no data was corrupted during compression; writeTrailer
      crc.update(ncArray);
      int value = (int) crc.getValue();
      writeTrailer(outtie, value, ncArray.length);
    }

    //something went wrong above, exit with error
    catch (Throwable err){
      System.err.println(err.toString());
      System.exit(1);
    }
  }
}