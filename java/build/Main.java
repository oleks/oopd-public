import java.io.File;
import java.util.List;
import java.util.ArrayList;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import javax.tools.StandardJavaFileManager;
import javax.tools.JavaFileObject;

public class Main {
  private List<File> files;

  public static void main(final String[] arguments) {
    if (arguments.length > 1) {
      new Main(arguments[1]);
    } else {
      new Main(".");
    }
  }

  private Main(final String root) {
    this.files = new ArrayList<File>();
    this.fillFiles(new File(root));
    this.compile();
  }

  private void fillFiles(final File folder) {
    File[] files = folder.listFiles();
    if (files == null) {
      return;
    }
    for (File file : files) {
      if (file.isDirectory()) {
        this.fillFiles(file);
      } else {
        if (file.getName().endsWith(".java")) {
          this.files.add(file);
        }
      }
    }
  }

  private void compile() {
    final JavaCompiler compiler =
      ToolProvider.getSystemJavaCompiler();
    final StandardJavaFileManager fileManager =
      compiler.getStandardFileManager(null, null, null);
    final Iterable<? extends JavaFileObject> compilationUnits =
      fileManager.getJavaFileObjectsFromFiles(this.files);

    compiler
      .getTask(null, fileManager, null, null, null, compilationUnits)
      .call();
  }
}
