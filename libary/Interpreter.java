import diku.oopd.io.*;

public class Interpreter
{

  public static void main(String[] args)
  {
    Console.writeLine("Hello, this is an interpreter.");
    new Interpreter().start();
  }

  private Interpreter()
  {
    // Intentionally left blank.
  }

  private void start()
  {
    while (true)
      this.parse();
  }

  private void parse()
  {
    Console.skipWhitespace();
    Integer firstValue = Console.readInteger();
    Console.writeLine(firstValue);
    /*if (firstValue == null)
    {
      Console.writeLine("Expression should start with an int!");
      Console.readLine();
    }*/
  }
}
