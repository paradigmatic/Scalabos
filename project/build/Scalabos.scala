import sbt._

class ScalabosProject(info: ProjectInfo) extends DefaultProject(info)
{

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
        
  override def testScalaSourcePath = "test"
  override def testResourcesPath = "test-resources"

}