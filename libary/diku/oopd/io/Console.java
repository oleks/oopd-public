package diku.oopd.io;

import java.util.*;
import java.util.regex.*;

public class Console
{
  private static Console instance;
  private static Console getInstance()
  {
    if (Console.instance == null)
      Console.instance = new Console();
    return Console.instance;
  }

  private Scanner scanner;
  private static Pattern WHITESPACE = Pattern.compile("[\\s\\r\\n\\t]*");

  private Console()
  {
    this.scanner = new Scanner(System.in);
  }

  /***
  
  \time{System.out.println(Object)}
  
  ***/

  public static void writeLine(Object toWrite)
  {
    System.out.println(toWrite);
  }

  public static void write(Object toWrite)
  {
    System.out.print(toWrite);
  }

  /***
  
  \time{Scanner.nextLine()}
  
  \null{If no line was found or the stream is already closed. This should
  rarely happen with System.in, but it might. null is returned instead of an
  empty string because an empty string is a valid line.}
  
  ***/

  public static String readLine()
  {
    try
    {
      return Console.getInstance().scanner.nextLine();
    }
    catch (Exception _)
    {
      return null;
    }
  }

  public static Integer readInteger()
  {
    try
    {
      return Console.getInstance().scanner.nextInt();
    }
    catch (Exception _)
    {
      return null;
    }
  }

  public static void skipWhitespace()
  {
    try
    {
      Console.getInstance().scanner.next(Console.WHITESPACE);
    }
    catch (Exception _)
    {
      return;
    }
  }
}
