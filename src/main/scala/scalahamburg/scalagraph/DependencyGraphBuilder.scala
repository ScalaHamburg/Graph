package scalahamburg.scalagraph
import java.io.FileReader
import java.io.BufferedReader

class DependencyGraphBuilder {
  val prefix = "de\\.|org\\."
  def readFile(filePath:String):(String, List[String])= {
    val fileReader = new BufferedReader(new FileReader(filePath))
    var line = fileReader.readLine();
    var bundle = ""
   	var dependent = List[String]()
   	var readDep = false
    while (line != null) {
      line match{
        case Bundle(b) => bundle = b.trim()
        case Depends(d) => {
          dependent = d::dependent
          readDep = true
        }
        case depLine => if(readDep == true){
            if(depLine.trim.startsWith("de.") || depLine.trim.startsWith("org.")){
            	dependent = depLine.trim::dependent
            }else{
              readDep = false
            }
        }
      }
      line = fileReader.readLine();
    }
   	(bundle, dependent)
  }
}

object Bundle{
	val Name = "Bundle-Name:"
  def unapply(str: String): Option[String] = {
    if(str.startsWith(Name)){
    	Some(str.substring(Name.length).trim)
    } else {
      None
    }
  }
}
object Depends{
	val Depends = "Require-Bundle:"
		def unapply(str: String): Option[String] = {
			if(str.startsWith(Depends)){
				Some(str.substring(Depends.length).trim)
			} else {
				None
			}
	}
}

object DependencyGraphBuilder {
  def main(args: Array[String]) {
    val (bundle, dependent) = new DependencyGraphBuilder().readFile("d:/persdat/workspacePTRscala/Graph/src/main/mf/MANIFEST.MF")
    println(bundle+"->"+dependent)
  }
}