import diku.oopd.io.*;

public class Test
{
  public static void main(String[] args)
  {
    String x = "    Console.writeLine(\"import diku.oopd.io.*;\\n\\npublic class Test\\n{\\n  public static void main(String[] args)\\n  {\");\n    Console.write(\"    String x = \\\"\");\n    Console.write(x.replaceAll(\"\\n\", \"\\\\n\");\n    Console.write(\"\\\";\\n\");\n    Console.write(x);\n    Console.writeLine(\"\\n  }\\n}\");";
    Console.writeLine("import diku.oopd.io.*;\n\npublic class Test\n{\n  public static void main(String[] args)\n  {");
    Console.write("    String x = \"");
    Console.write(x.replaceAll("\n", "\\n"));
    Console.write("\";\n");
    Console.write(x);
    Console.writeLine("\n  }\n}");
  }

  public Test()
  {
    //Intentionally left blank.
  }
}
