import sbt._

class ScalabosProject(info: ProjectInfo) extends DefaultProject(info)
{

  val jfreechart = "jfree" % "jfreechart" % "1.0.13"
  val jcommon= "jfree" % "jcommon" % "1.0.15"

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
        
  override def testScalaSourcePath = "test"
  override def testResourcesPath = "test-resources"

}
