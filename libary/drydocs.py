import os, re

WIKIDOC_PATTERN = re.compile(\
  "\/\*\*\*\s+(?P<drydoc>.+?)\*\*\*\/\s*(?P<declaration>"+\
  "(?P<isPreStatic>static\s+)?"+\
  "((?P<access>public|private|protected)\s+)?"+\
  "(?P<isPostStatic>static\s+)?"+\
  "((?P<type>.+?)\s+)?"+\
  "(?P<name>.+?)\s*"+\
  "\((?P<arguments>.*?)\))",\
  re.DOTALL | re.IGNORECASE)

ARGUMENTS_PATTERN = re.compile(\
  "\s*(?P<type>.+?)\s+(?P<name>.+?)(,|$)",\
  re.DOTALL | re.IGNORECASE)

DRYDOC_PATTERN = re.compile(\
  r"\\(?P<command>[a-z]+)\{(?P<value>.*?[^\\])\}",
  re.DOTALL | re.IGNORECASE)

class Main:
  def __init__(self):
    for (dirpath, directories, filenames) in os.walk("."):
      if dirpath.startswith("./.git"):
        continue
      DirectoryWriter(dirpath, directories, filenames).write()

class HtmlWriter:
  def __init__(self, dirpath):
    self.writerDestinaiton = None
    self.dirpath = dirpath
    self.package = self.dirpath.split("/")[1:]

  def writeTag(self, tag, content, attributes = None):
    self.html.write("<")
    self.html.write(tag)
    if not attributes == None:
      self.html.write(str(attributes))
    self.html.write(">")
    self.writeContent(content)
    self.html.write("</")
    self.html.write(tag)
    self.html.write(">")

  def writeDiv(self, cssClass, content):
    self.html.write("<div class='")
    self.html.write(cssClass)
    self.html.write("'>")
    self.writeContent(content)
    self.html.write("</div>")

  def writeContent(self, content):
    if (content.__class__.__name__ in ['function', 'method']):
      content()
    else:
      self.html.write(str(content))

  def write(self):
    if self.writerDestination == None:
      return
    self.html = open(self.writerDestination, 'w')
    self.html.writeTag = self.writeTag
    self.html.writeDiv = self.writeDiv
    self.writeHtml()
    self.html.close()

  def writeHtml(self):
    def aux():
     self.writeHead()
     self.writeBody()   
    self.writeTag("html", aux)

  def writeHead(self):
    def aux():
      self.writeTag("title", self.writeTitle)
      self.writeStyles()
    self.writeTag("head", aux)

  def writeTitle(self):
    pass

  def writeStyles(self):
    self.writeFontStyles()
    self.writeRootStyle()

  def writeFontStyles(self):
    self.writeGoogleFontStyle("Gentium+Book+Basic")
    self.writeGoogleFontStyle("Ubuntu+Mono")

  def writeGoogleFontStyle(self, font):
    self.writeStyle("http://fonts.googleapis.com/css?family=" + font)

  def writeRootStyle(self):
    nestingLevel = self.writerDestination.count('/') - 1
    pathToCss = ("../" * nestingLevel) + "style.css"
    self.writeStyle(pathToCss)

  def writeStyle(self, url):
    self.html.write("<link href='")
    self.html.write(url)
    self.html.write("' rel='stylesheet' type='text/css'/>")

  def writeBody(self):
    self.writeTag("body", self.writeBodyContent)

  def writeBodyContent(self):
    pass
 
  def writeGenericHeader(self):
    self.writeDiv("title", self.writeName)
    self.writeDiv("package", self.writePackage)

  def writeName(self):
    self.html.write("~")

  def writePackage(self):
    self.writeDiv("label", "Package:")
    nesting = len(self.package)
    self.writePackageAnchor(nesting, "~")
    nesting -= 1
    if nesting < 0:
      return
    self.html.write(".")
    for pack in self.package:
      self.writePackageAnchor(nesting, pack)
      nesting -= 1
      if not nesting < 0:
        self.html.write(".")

  def writePackageAnchor(self, nesting, name):
    self.html.write("<a href='")
    self.html.write(nesting * "../")
    self.html.write("index.html'>")
    self.html.write(name)
    self.html.write("</a>")
  
class DirectoryWriter(HtmlWriter):
  def __init__(self, dirpath, directories, filenames):
    super(DirectoryWriter, self).__init__(dirpath)
    self.dirpath = dirpath
    self.setName()
    self.directories = directories
    self.filenames = filenames
    self.writerDestination = os.path.join(self.dirpath, "index.html")

  def setName(self):
    splitDirPath = self.dirpath.split("/")[1:]
    if len(splitDirPath) == 0:
      self.name = "~"
    else:
      self.name = ".".join(splitDirPath)

  def writeTitle(self):
    self.html.write(self.name)

  def writeBodyContent(self):
    def aux():
      self.writeGenericHeader()
      self.writeDirectories()
      self.writeFiles()
    self.writeDiv("content", aux)

  def writeName(self):
    self.html.write(self.name)

  def writePackageAnchor(self, nesting, name):
    if nesting == 0:
      self.html.write(name)
      return
    super(DirectoryWriter, self).writePackageAnchor(nesting, name)
  

  def writeDirectories(self):
    if len(self.directories) == 0:
      return
    def aux():
      self.writeDiv("header", "Packages")
      for directory in self.directories:
        if directory == ".git":
          continue
        self.writeDirectory(directory)
    self.writeDiv("packages", aux)

  def writeDirectory(self, directory):
    self.html.write("<a href='")
    self.html.write(directory)
    self.html.write("/index.html'>")
    self.html.write(directory)
    self.html.write("</a>")

  def writeFiles(self):
    javaFilenames = []
    for filename in self.filenames:
      if filename.endswith(".java"):
        javaFilenames.append(filename)

    if len(javaFilenames) == 0:
      return
    def aux():
      self.writeDiv("header", "Classes")
      for filename in javaFilenames:
        self.writeFile(filename)
        FileParser(self.dirpath, filename)
    self.writeDiv("classes", aux)

  def writeFile(self, filename):
    className = filename[:-5]
    self.html.write("<a href='")
    self.html.write(className)
    self.html.write(".html'>")
    self.html.write(className)
    self.html.write("</a>")    

  def close(self):
    self.html.close()

class FileParser:
  def __init__(self, dirpath, filename):
    self.dirpath = dirpath
    self.filename = filename
    self.classEntry = ClassEntry(dirpath, filename)
    self.parseFile()
    self.documentFile()

  def parseFile(self):
    pathToSource = os.path.join(self.dirpath, self.filename)
    source = open(pathToSource).read()
    for match in WIKIDOC_PATTERN.finditer(source):
      self.classEntry.methods.append(MethodEntry(match))

  def documentFile(self):
    FileWriter(self.dirpath, self.filename, self.classEntry).write()

class FileWriter(HtmlWriter):
  def __init__(self, dirpath, filename, classEntry):
    super(FileWriter, self).__init__(dirpath)
    self.classEntry = classEntry    
    self.writerDestination = os.path.join(dirpath, filename[:-4] + "html")

  def writeTitle(self):
    self.html.write(self.classEntry.name)

  def writeBodyContent(self):
    def aux():
      self.writeGenericHeader()
      self.writeDiv("methods", self.writeMethods)
    self.writeDiv("class", aux)

  def writeMethods(self):
    for method in self.classEntry.methods:
      MethodHtmlWriter(self.html, method).write()

  def writeName(self):
    self.html.write(self.classEntry.name)

class MethodHtmlWriter:
  def __init__(self, html, method):
    self.html = html
    self.method = method

  def write(self):
    self.html.writeDiv("method", self.writeMethodContent)
    
  def writeMethodContent(self):
    self.html.writeDiv("access", self.method.access)  
    self.writeStatic()
    self.method.type.__html__(self.html)
    self.html.writeDiv("name", self.writeMethodName)
    self.writeArguments()
    self.writeDrydocs()

  def writeStatic(self):
    if not self.method.static:
      return
    self.html.writeDiv("static", "static")

  def writeMethodName(self):
    self.html.write(self.method.name)

  def writeArguments(self):
    if len(self.method.arguments) == 0:
      return
    self.html.writeDiv("arguments", lambda : \
      [self.writeArgument(argument) for argument in self.method.arguments])
 
  def writeArgument(self, argument):
    self.html.write("<div class='argument'>")
    argument.type.__html__(self.html) 
    self.html.writeDiv("name", argument.name)
    self.html.write("</div>")

  def writeDrydocs(self):
    self.html.writeDiv("drydocs", lambda : 
      [entry.__html__(self.html) for entry in self.method.drydoc])

class ClassEntry:
  def __init__(self, dirpath, filename):
    self.package = dirpath.split("/")[1:]
    self.name = filename[:-5]
    self.variables = []
    self.methods = []

class MethodEntry:
  def __init__(self, match):
    self.match = match
    self.parseAccess()
    self.parseStatic()
    self.parseName()
    self.parseType()
    self.parseArguments()
    self.parseDrydoc()

  def parseAccess(self):
    self.access = Access(self.match.group("access"))

  def parseStatic(self):
    self.static = self.match.group("isPreStatic") == None or \
      self.match.group("isPostStatic") == None

  def parseName(self):
    self.name = self.match.group("name")

  def parseType(self):
    self.type = Type(self.match.group("type"))

  def parseArguments(self):
    self.arguments = []
    arguments = self.match.group("arguments")
    for match in ARGUMENTS_PATTERN.finditer(arguments):
      type = match.group("type")
      name = match.group("name")
      self.arguments.append(Argument(type, name))

  def parseDrydoc(self):
    self.drydoc = []
    for match in DRYDOC_PATTERN.finditer(self.match.group("drydoc")):
      self.parseDrydocCommand(match)

  def parseDrydocCommand(self, match):
    name = match.group("command")
    value = match.group("value")
    if name == "null":
      self.drydoc.append(NullCommand(value))
    elif name == "time":
      self.drydoc.append(TimeCommand(value))

class NullCommand:
  def __init__(self, value):
    self.value = value

  def __html__(self, html):
    def aux():
      html.writeDiv("header", "Possibly null")
      html.writeDiv("description", self.value)
    html.writeDiv("drydoc null", aux)

class TimeCommand:
  def __init__(self, value):
    self.value = value

  def __html__(self, html):
    def aux():
      html.writeDiv("header", "Time complexity")
      html.writeDiv("description", self.complexityMap())
    html.writeDiv("drydoc time", aux)

  def complexityMap(self):
    if self.value == "System.out.println(Object)":
      return "O(n), where n is the length of the text written."
    elif self.value == "Scanner.nextLine()":
      return "O(n), where n is the length of the line."

class Argument:
  def __init__(self, typeString, nameString):
    self.type = Type(typeString)
    self.name = nameString

class Access:
  PRIVATE = "public"
  PROTECTED = "protected"
  PACKAGE = "package"
  PUBLIC = "public"

  def __init__(self, accessString):
    self.parse(accessString.lower())

  def parse(self, accessString):
    if accessString == "private":
      self.value = Access.PRIVATE
    elif accessString == "public":
      self.value = Access.PUBLIC
    elif accessString == "protected":
      self.value = Access.PROTECTED
    else:
      self.value = Access.PACKAGE

  def __str__(self):
    return self.value

class Type:
  CLASS = 0
  CONSTRUCTOR = 1
  VOID = "void"
  INT = "int"
  STRING = "String"
  OBJECT = "Object"

  def __init__(self, typeString):
    self.parse(typeString)

  def parse(self, typeString):
    lowerTypeString = typeString.lower()
    self.isPrimitive = True
    if lowerTypeString == "class":
      self.value = Type.CLASS
    elif typeString == None or len(typeString) == 0:
      self.value = Type.CONSTRUCTOR
    elif lowerTypeString == "int":
      self.value = Type.INT
    elif lowerTypeString == "void":
      self.value = Type.VOID
    elif lowerTypeString == "object":
      self.value = Type.OBJECT
      self.isPrimitive = False
    elif lowerTypeString == "string":
      self.value = Type.STRING
      self.isPrimitive = False
    else:
      self.value = typeString
      self.isPrimitive = False

  def __html__(self, html):
    if self.value == Type.CONSTRUCTOR:
      return
    cssClass = "type"
    if self.isPrimitive:
      cssClass += " prime" 
    html.writeDiv(cssClass, lambda : html.write(str(self.value)))

  def __str__(self):
    return str(self.value)

if __name__ == "__main__":
  Main()
