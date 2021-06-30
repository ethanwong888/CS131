//helper functions that are used a lot throughout other files. 
//just implementing them here so other files don't get cluttered
import java.util.concurrent.Semaphore;
import java.util.zip.Deflater;
import java.io.PushbackInputStream;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.CRC32;
import java.io.FilterOutputStream;
import java.nio.ByteBuffer;
import java.nio.Buffer;
import java.nio.ByteOrder;

public class JHelper{
  //Process ID
  private volatile int processID;
  public int getProcessID(){
    return this.processID;
  }
  public void setProcessID(int val){
    this.processID = val;
  }


  //Process Counter
  private volatile int processCounter = 0;
  public synchronized int addProcessCounter(){
    processCounter++;
    return processCounter;
  }


  //Final Block Index
  private volatile int finalBlockIdx = 0;
  public synchronized int getFinalBlockIdx(){
    return finalBlockIdx;
  }
  public synchronized void setFinalBlockIdx(int x){
    finalBlockIdx = x;
  } 
  

  //Block (size)
  private static final int block = 131072;
  public static int getBlockSize(){
    return block;
  }


  //Semaphore
  private Semaphore sema = new Semaphore(1, true);
  public void getSema() throws InterruptedException{
    sema.acquire();
  }
  public void freeSema() throws InterruptedException{
    sema.release();
  }
}