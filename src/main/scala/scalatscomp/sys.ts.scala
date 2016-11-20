package scalatscomp

object Sys {
  type FileWatcherCallback = ((String, Boolean) => Unit)
  type DirectoryWatcherCallback = ((String) => Unit)

  trait WatchedFile {
    var fileName: String
    var callback: FileWatcherCallback
    var mtime: Date
  }

  trait System {
    var args: Array[String]
    var newLine: String
    var useCaseSensitiveFileNames: Boolean

    def write(s: String): Unit

    def readFile(path: String, encoding: String): String

    def getFileSize(path: String): Int

    def writeFile(path: String, data: String, writeByteOrderMark: Boolean): Unit

    def watchFile(path: String, callback: FileWatcherCallback): FileWatcher

    def watchDirectory(path: String, callback: DirectoryWatcherCallback, recursive: Boolean): FileWatcher

    def resolvePath(path: String): String

    def fileExists(path: String): Boolean

    def directoryExists(path: String): Boolean

    def createDirectory(path: String): Unit

    def getExecutingFilePath(): String

    def getCurrentDirectory(): String

    def getDirectories(path: String): Array[String]

    def readDirectory(path: String, extensions: Array[String], exclude: Array[String], include: Array[String]): Array[String]

    def getModifiedTime(path: String): Date

    def createHash(data: String): String

    def getMemoryUsage(): Int

    def exit(exitCode: Int): Unit

    def realpath(path: String): String

    def getEnvironmentVariable(name: String): String

    def tryEnableSourceMapsForHost(): Unit

    def setTimeout(callback: ((Array[Any]) => Unit), ms: Int, args: Array[Any]): Any

    def clearTimeout(timeoutId: Any): Unit
  }

  trait FileWatcher {
    def close(): Unit
  }

  trait DirectoryWatcher extends FileWatcher {
    var directoryName: String
    var referenceCount: Int
  }

  var require: Any = zeroOfMyType
  var process: Any = zeroOfMyType
  var global: Any = zeroOfMyType
  var ___filename: String = zeroOfMyType

  class Enumerator {
    def atEnd(): Boolean

    def moveNext(): Boolean

    def item(): Any

    def this(o: Any) = this()
  }

  var ChakraHost: {var args: Array[String]
    var currentDirectory: String
    var executingFile: String
    var newLine: String
    var useCaseSensitiveFileNames: Boolean
    def echo(s: String): Unit
    def quit(exitCode: Int): Unit
    def fileExists(path: String): Boolean
    def directoryExists(path: String): Boolean
    def createDirectory(path: String): Unit
    def resolvePath(path: String): String
    def readFile(path: String): String
    def writeFile(path: String, contents: String): Unit
    def getDirectories(path: String): Array[String]
    def readDirectory(path: String, extensions: Array[String], basePaths: Array[String], excludeEx: String, includeFileEx: String, includeDirEx: String): Array[String]
    def watchFile(path: String, callback: FileWatcherCallback): FileWatcher
    def watchDirectory(path: String, callback: DirectoryWatcherCallback, recursive: Boolean): FileWatcher
    def realpath(path: String): String
    def getEnvironmentVariable(name: String): String
  } = zeroOfMyType
  var sys: System = ((() => {
    def getWScriptSystem(): System = {
      val fso = new ActiveXObject("Scripting.FileSystemObject")
      val shell = new ActiveXObject("WScript.Shell")
      val fileStream = new ActiveXObject("ADODB.Stream")
      (fileStream.Type = 2)
      val binaryStream = new ActiveXObject("ADODB.Stream")
      (binaryStream.Type = 1)
      val args: Array[String] = Array() {
        var i = 0
        while ((i < WScript.Arguments.length)) {
          {
            (args(i) = WScript.Arguments.Item(i))

          }
          (i += 1)
        }
      }
      def readFile(fileName: String, encoding: String): String = {
        if ((!fso.FileExists(fileName))) {
          return undefined

        }
        fileStream.Open()
        try {
          if (encoding) {
            (fileStream.Charset = encoding)
            fileStream.LoadFromFile(fileName)

          }
          else {
            (fileStream.Charset = "x-ansi")
            fileStream.LoadFromFile(fileName)
            val bom = (fileStream.ReadText(2) || "")
            (fileStream.Position = 0)
            (fileStream.Charset = (if (((bom.length >= 2) && ((((bom.charCodeAt(0) === 255) && (bom.charCodeAt(1) === 254)) || ((bom.charCodeAt(0) === 254) && (bom.charCodeAt(1) === 255)))))) "unicode" else "utf-8"))

          }
          return fileStream.ReadText()

        } catch {
          case e: Throwable => {
            throw e
          }
        } finally {
          fileStream.Close()

        }

      }
      def writeFile(fileName: String, data: String, writeByteOrderMark: Boolean): Unit = {
        fileStream.Open()
        binaryStream.Open()
        try {
          (fileStream.Charset = "utf-8")
          fileStream.WriteText(data)
          if (writeByteOrderMark) {
            (fileStream.Position = 0)

          }
          else {
            (fileStream.Position = 3)

          }
          fileStream.CopyTo(binaryStream)
          binaryStream.SaveToFile(fileName, 2)

        } finally {
          binaryStream.Close()
          fileStream.Close()

        }

      }
      def getNames(collection: Any): Array[String] = {
        val result: Array[String] = Array() {
          var e = new Enumerator(collection)
          while ((!e.atEnd())) {
            {
              result.push(e.item().Name)

            }
            e.moveNext()
          }
        }
        return result.sort()

      }
      def getDirectories(path: String): Array[String] = {
        val folder = fso.GetFolder(path)
        return getNames(folder.subfolders)

      }
      def getAccessibleFileSystemEntries(path: String): FileSystemEntries = {
        try {
          val folder = fso.GetFolder((path || "."))
          val files = getNames(folder.files)
          val directories = getNames(folder.subfolders)
          return Map("files" -> files,
            "directories" -> directories)

        } catch {
          case e: Throwable => {
            return Map("files" -> Array(),
              "directories" -> Array())

          }
        }

      }
      def readDirectory(path: String, extensions: Array[String], excludes: Array[String], includes: Array[String]): Array[String] = {
        return matchFiles(path, extensions, excludes, includes, false, shell.CurrentDirectory, getAccessibleFileSystemEntries)

      }
      val wscriptSystem: System = Map("args" -> args,
        "newLine" -> "\r\n",
        "useCaseSensitiveFileNames" -> false,
        "write" -> ((s: String) => {
          WScript.StdOut.Write(s)

        }),
        "readFile" -> readFile,
        "writeFile" -> writeFile,
        "resolvePath" -> ((path: String) => {
          return fso.GetAbsolutePathName(path)

        }),
        "fileExists" -> ((path: String) => {
          return fso.FileExists(path)

        }),
        "directoryExists" -> ((path: String) => {
          return fso.FolderExists(path)

        }),
        "createDirectory" -> ((directoryName: String) => {
          if ((!wscriptSystem.directoryExists(directoryName))) {
            fso.CreateFolder(directoryName)

          }

        }),
        "getExecutingFilePath" -> (() => {
          return WScript.ScriptFullName

        }),
        "getCurrentDirectory" -> (() => {
          return shell.CurrentDirectory

        }),
        "getDirectories" -> getDirectories,
        "getEnvironmentVariable" -> ((name: String) => {
          return new ActiveXObject("WScript.Shell").ExpandEnvironmentStrings( s"""%${name}%""")

        }),
        "readDirectory" -> readDirectory,
        "exit" -> ((exitCode: Int) => {
          try {
            WScript.Quit(exitCode)

          } catch {
            case e: Throwable => {
            }
          }

        }))
      return wscriptSystem

    }
    def getNodeSystem(): System = {
      val _fs = require("fs")
      val _path = require("path")
      val _os = require("os")
      val _crypto = require("crypto")
      val useNonPollingWatchers = process.env("TSC_NONPOLLING_WATCHER")
      def createWatchedFileSet() = {
        val dirWatchers = createMap[DirectoryWatcher]()
        val fileWatcherCallbacks = createMap[Array[FileWatcherCallback]]()
        return Map("addFile" -> addFile,
          "removeFile" -> removeFile)
        def reduceDirWatcherRefCountForFile(fileName: String) = {
          val dirName = getDirectoryPath(fileName)
          val watcher = dirWatchers(dirName)
          if (watcher) {
            (watcher.referenceCount -= 1)
            if ((watcher.referenceCount <= 0)) {
              watcher.close()
              dirWatchers.remove(dirName)

            }

          }

        }
        def addDirWatcher(dirPath: String): Unit = {
          var watcher = dirWatchers(dirPath)
          if (watcher) {
            (watcher.referenceCount += 1)
            return

          }
          (watcher = _fs.watch(dirPath, Map("persistent" -> true), ((eventName: String, relativeFileName: String) => fileEventHandler(eventName, relativeFileName, dirPath))))
          (watcher.referenceCount = 1)
          (dirWatchers(dirPath) = watcher)
          return

        }
        def addFileWatcherCallback(filePath: String, callback: FileWatcherCallback): Unit = {
          multiMapAdd(fileWatcherCallbacks, filePath, callback)

        }
        def addFile(fileName: String, callback: FileWatcherCallback): WatchedFile = {
          addFileWatcherCallback(fileName, callback)
          addDirWatcher(getDirectoryPath(fileName))
          return Map("fileName" -> fileName,
            "callback" -> callback)

        }
        def removeFile(watchedFile: WatchedFile) = {
          removeFileWatcherCallback(watchedFile.fileName, watchedFile.callback)
          reduceDirWatcherRefCountForFile(watchedFile.fileName)

        }
        def removeFileWatcherCallback(filePath: String, callback: FileWatcherCallback) = {
          multiMapRemove(fileWatcherCallbacks, filePath, callback)

        }
        def fileEventHandler(eventName: String, relativeFileName: String, baseDirPath: String) = {
          val fileName = (if ((typeof(relativeFileName) !== "string")) undefined else ts.getNormalizedAbsolutePath(relativeFileName, baseDirPath))
          if (((((eventName === "change") || (eventName === "rename"))) && fileWatcherCallbacks(fileName))) {
            (fileWatcherCallbacks(fileName)).foreach { fresh1 =>
              val fileCallback = zeroOfMyType
                = fresh1 {
                fileCallback(fileName)

              }
            }

          }

        }

      }
      val watchedFileSet = createWatchedFileSet()
      def isNode4OrLater(): Boolean = {
        return (parseInt(process.version.charAt(1)) >= 4)

      }
      def isFileSystemCaseSensitive(): Boolean = {
        if (((platform === "win32") || (platform === "win64"))) {
          return false

        }
        return ((!fileExists(___filename.toUpperCase())) || (!fileExists(___filename.toLowerCase())))

      }
      val platform: String = _os.platform()
      val useCaseSensitiveFileNames = isFileSystemCaseSensitive()
      def readFile(fileName: String, _encoding: String): String = {
        if ((!fileExists(fileName))) {
          return undefined

        }
        val buffer = _fs.readFileSync(fileName)
        var len = buffer.length
        if ((((len >= 2) && (buffer(0) === 254)) && (buffer(1) === 255))) {
          (len &= (~1)) {
            var i = 0
            while ((i < len)) {
              {
                val temp = buffer(i)
                (buffer(i) = buffer((i + 1)))
                (buffer((i + 1)) = temp)

              }
              (i += 2)
            }
          }
          return buffer.`toString`("utf16le", 2)

        }
        if ((((len >= 2) && (buffer(0) === 255)) && (buffer(1) === 254))) {
          return buffer.`toString`("utf16le", 2)

        }
        if (((((len >= 3) && (buffer(0) === 239)) && (buffer(1) === 187)) && (buffer(2) === 191))) {
          return buffer.`toString`("utf8", 3)

        }
        return buffer.`toString`("utf8")

      }
      def writeFile(fileName: String, data: String, writeByteOrderMark: Boolean): Unit = {
        if (writeByteOrderMark) {
          (data = ("\uFEFF" + data))

        }
        var fd: Int = zeroOfMyType
        try {
          (fd = _fs.openSync(fileName, "w"))
          _fs.writeSync(fd, data, undefined, "utf8")

        } finally {
          if ((fd !== undefined)) {
            _fs.closeSync(fd)

          }

        }

      }
      def getAccessibleFileSystemEntries(path: String): FileSystemEntries = {
        try {
          val entries = _fs.readdirSync((path || ".")).sort()
          val files: Array[String] = Array()
          val directories: Array[String] = Array()
          (entries).foreach { fresh2 =>
            val entry = zeroOfMyType
              = fresh2 {
              if (((entry === ".") || (entry === ".."))) {
                continue

              }
              val name = combinePaths(path, entry)
              var stat: Any = zeroOfMyType
              try {
                (stat = _fs.statSync(name))

              } catch {
                case e: Throwable => {
                  continue

                }
              }
              if (stat.isFile()) {
                files.push(entry)

              }
              else if (stat.isDirectory()) {
                directories.push(entry)

              }

            }
          }
          return Map("files" -> files,
            "directories" -> directories)

        } catch {
          case e: Throwable => {
            return Map("files" -> Array(),
              "directories" -> Array())

          }
        }

      }
      def readDirectory(path: String, extensions: Array[String], excludes: Array[String], includes: Array[String]): Array[String] = {
        return matchFiles(path, extensions, excludes, includes, useCaseSensitiveFileNames, process.cwd(), getAccessibleFileSystemEntries)

      }
      sealed abstract class FileSystemEntryKind
      object FileSystemEntryKind {

        case object File extends FileSystemEntryKind

        case object Directory extends FileSystemEntryKind

      }
      def fileSystemEntryExists(path: String, entryKind: FileSystemEntryKind): Boolean = {
        try {
          val stat = _fs.statSync(path)
          entryKind match {
            case FileSystemEntryKind.File =>
              return stat.isFile()
            case FileSystemEntryKind.Directory =>
              return stat.isDirectory()
            case _ =>
          }

        } catch {
          case e: Throwable => {
            return false

          }
        }

      }
      def fileExists(path: String): Boolean = {
        return fileSystemEntryExists(path, FileSystemEntryKind.File)

      }
      def directoryExists(path: String): Boolean = {
        return fileSystemEntryExists(path, FileSystemEntryKind.Directory)

      }
      def getDirectories(path: String): Array[String] = {
        return filter[String](_fs.readdirSync(path), (dir => fileSystemEntryExists(combinePaths(path, dir), FileSystemEntryKind.Directory)))

      }
      val nodeSystem: System = Map("args" -> process.argv.slice(2),
        "newLine" -> _os.EOL,
        "useCaseSensitiveFileNames" -> useCaseSensitiveFileNames,
        "write" -> ((s: String) => {
          process.stdout.write(s)

        }),
        "readFile" -> readFile,
        "writeFile" -> writeFile,
        "watchFile" -> ((fileName, callback) => {
          if (useNonPollingWatchers) {
            val watchedFile = watchedFileSet.addFile(fileName, callback)
            return Map("close" -> (() => watchedFileSet.removeFile(watchedFile)))

          }
          else {
            _fs.watchFile(fileName, Map("persistent" -> true,
              "interval" -> 250), fileChanged)
            return Map("close" -> (() => _fs.unwatchFile(fileName, fileChanged)))

          }
          def fileChanged(curr: Any, prev: Any) = {
            if (((+curr.mtime) <= (+prev.mtime))) {
              return

            }
            callback(fileName)

          }

        }),
        "watchDirectory" -> ((directoryName, callback, recursive) => {
          var options: Any = zeroOfMyType
          if ((!directoryExists(directoryName))) {
            return

          }
          if ((isNode4OrLater() && (((process.platform === "win32") || (process.platform === "darwin"))))) {
            (options = Map("persistent" -> true,
              "recursive" -> (!(!recursive))))

          }
          else {
            (options = Map("persistent" -> true))

          }
          return _fs.watch(directoryName, options, ((eventName: String, relativeFileName: String) => {
            if ((eventName === "rename")) {
              callback((if ((!relativeFileName)) relativeFileName else normalizePath(combinePaths(directoryName, relativeFileName))))

            };
          }))

        }),
        "resolvePath" -> ((path: String) => {
          return _path.resolve(path)

        }),
        "fileExists" -> fileExists,
        "directoryExists" -> directoryExists,
        "createDirectory" -> ((directoryName: String) => {
          if ((!nodeSystem.directoryExists(directoryName))) {
            _fs.mkdirSync(directoryName)

          }

        }),
        "getExecutingFilePath" -> (() => {
          return ___filename

        }),
        "getCurrentDirectory" -> (() => {
          return process.cwd()

        }),
        "getDirectories" -> getDirectories,
        "getEnvironmentVariable" -> ((name: String) => {
          return (process.env(name) || "")

        }),
        "readDirectory" -> readDirectory,
        "getModifiedTime" -> (path => {
          try {
            return _fs.statSync(path).mtime

          } catch {
            case e: Throwable => {
              return undefined

            }
          }

        }),
        "createHash" -> (data => {
          val hash = _crypto.createHash("md5")
          hash.update(data)
          return hash.digest("hex")

        }),
        "getMemoryUsage" -> (() => {
          if (global.gc) {
            global.gc()

          }
          return process.memoryUsage().heapUsed

        }),
        "getFileSize" -> (path => {
          try {
            val stat = _fs.statSync(path)
            if (stat.isFile()) {
              return stat.size

            }

          } catch {
            case e: Throwable => {}
          }
          return 0

        }),
        "exit" -> ((exitCode: Int) => {
          process.exit(exitCode)

        }),
        "realpath" -> ((path: String) => {
          return _fs.realpathSync(path)

        }),
        "tryEnableSourceMapsForHost" -> (() => {
          try {
            require("source-map-support").install()

          } catch {
            case e: Throwable => {
            }
          }

        }),
        "setTimeout" -> setTimeout,
        "clearTimeout" -> clearTimeout)
      return nodeSystem

    }
    def getChakraSystem(): System = {
      val realpath = (ChakraHost.realpath && (((path: String) => ChakraHost.realpath(path))))
      return Map("newLine" -> (ChakraHost.newLine || "\r\n"),
        "args" -> ChakraHost.args,
        "useCaseSensitiveFileNames" -> (!(!ChakraHost.useCaseSensitiveFileNames)),
        "write" -> ChakraHost.echo,
        "readFile" -> ((path: String, _encoding: String) => {
          return ChakraHost.readFile(path)

        }),
        "writeFile" -> ((path: String, data: String, writeByteOrderMark: Boolean) => {
          if (writeByteOrderMark) {
            (data = ("\uFEFF" + data))

          }
          ChakraHost.writeFile(path, data)

        }),
        "resolvePath" -> ChakraHost.resolvePath,
        "fileExists" -> ChakraHost.fileExists,
        "directoryExists" -> ChakraHost.directoryExists,
        "createDirectory" -> ChakraHost.createDirectory,
        "getExecutingFilePath" -> (() => ChakraHost.executingFile),
        "getCurrentDirectory" -> (() => ChakraHost.currentDirectory),
        "getDirectories" -> ChakraHost.getDirectories,
        "getEnvironmentVariable" -> (ChakraHost.getEnvironmentVariable || ((() => ""))),
        "readDirectory" -> ((path: String, extensions: Array[String], excludes: Array[String], includes: Array[String]) => {
          val pattern = getFileMatcherPatterns(path, excludes, includes, (!(!ChakraHost.useCaseSensitiveFileNames)), ChakraHost.currentDirectory)
          return ChakraHost.readDirectory(path, extensions, pattern.basePaths, pattern.excludePattern, pattern.includeFilePattern, pattern.includeDirectoryPattern)

        }),
        "exit" -> ChakraHost.quit,
        "realpath" -> realpath)

    }
    def recursiveCreateDirectory(directoryPath: String, sys: System) = {
      val basePath = getDirectoryPath(directoryPath)
      val shouldCreateParent = ((directoryPath !== basePath) && (!sys.directoryExists(basePath)))
      if (shouldCreateParent) {
        recursiveCreateDirectory(basePath, sys)

      }
      if ((shouldCreateParent || (!sys.directoryExists(directoryPath)))) {
        sys.createDirectory(directoryPath)

      }

    }
    var sys: System = zeroOfMyType
    if ((typeof(ChakraHost) !== "undefined")) {
      (sys = getChakraSystem())

    }
    else if (((typeof(WScript) !== "undefined") && (typeof(ActiveXObject) === "function"))) {
      (sys = getWScriptSystem())

    }
    else if (((((typeof(process) !== "undefined") && process.nextTick) && (!process.browser)) && (typeof(require) !== "undefined"))) {
      (sys = getNodeSystem())

    }
    if (sys) {
      val originalWriteFile = sys.writeFile
      (sys.writeFile = ((path, data, writeBom) => {
        val directoryPath = getDirectoryPath(normalizeSlashes(path))
        if ((directoryPath && (!sys.directoryExists(directoryPath)))) {
          recursiveCreateDirectory(directoryPath, sys)

        }
        originalWriteFile.call(sys, path, data, writeBom)

      }))

    }
    return sys

  })) ()
  if ((sys && sys.getEnvironmentVariable)) {
    (Debug.currentAssertionLevel = (if (java.util.regex.Pattern.compile(raw"""^development$$""", "i").test(sys.getEnvironmentVariable("NODE_ENV"))) AssertionLevel.Normal else AssertionLevel.None))

  }
}
