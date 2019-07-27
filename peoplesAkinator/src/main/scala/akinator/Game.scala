package akinator

import akinator.io.CommandsIO._
import akinator.io.IO
import akinator.tree._

object Game {

  def addNewPerson(startTree: Tree[String], path: String, newNode : Node[String]): Tree[String] = startTree match {
    case Empty => Empty
    case Node(_, Empty, Empty) => newNode
    case Node(x, l, r) => if(path.startsWith("l")){
      Node(x,addNewPerson(l, path.drop(1), newNode),r)
    } else {
      Node(x,l, addNewPerson(r, path.drop(1), newNode))
    }
  }
  /*
  def askForNewNode(person : String) : IO[Tree[String]] = for {
    newPerson <- promptInput("Who was your person?")
    newQuestion <- promptInput(s"How can I differentiate $person from $newPerson?")
    yesAnswer <- promptYesAnswer(person,newPerson)
  } if(yesAnswer) Node(newQuestion,Leaf(person),Leaf(newPerson)) else Node(newQuestion,Leaf(newPerson),Leaf(person))
*/
  def constructNewTree(startTree : Tree[String], path:String,  person: String) : IO[Tree[String]] = for {
    newPerson <- promptForCorrectAnswer()
    newQuestion <- promptForDifference(person, newPerson)
    yesAnswer <- promptYesAnswer(person,newPerson)
    pLeaf = Node(person)
    pNewLeaf = Node(newPerson)
    newNode = if(yesAnswer) Node(newQuestion,pLeaf,pNewLeaf)
              else Node(newQuestion,pNewLeaf,pLeaf)
    //newNode <- askForNewNode(person)
    newTree = addNewPerson(startTree, path, newNode)
    //_ <- Console.putStrLn(newTree.toString())
  } yield newTree

  def play(gameTree: Tree[String], startTree: Tree[String], path: String) : IO[Unit] = gameTree match {
    case Node(person, Empty, Empty) => for {
      isCorrectGuess <- promptGuess(person)
      _ <- if(isCorrectGuess) promptSuccess()
      else
        constructNewTree(startTree, path, person)
          .flatMap(endGame)
    } yield ()
    case Node(question, l, r) => for {
      answer <- promptQuestion(question)
      _ <- if(answer) play(l, startTree, path ++ "l") else play(r, startTree, path ++ "r")
    } yield ()
  }

  private def startGame(filePath : String = "tree.txt") : IO[Tree[String]] = for {
    _ <- promptStart()
    treeString = FileEngine.readStringFromFile(filePath)
  } yield Serializer.deserialize(treeString)

  private def endGame(tree : Tree[String]) : IO[Unit] = {
    FileEngine.writeTreeToFile(tree)
    promptPersonSaved()
  }

  def loop: IO[Unit] = for {
    tree <- startGame()
    _ <- play(tree, tree, "")
    shouldContinue <- promptForContinuation
    _ <- if (shouldContinue) loop else promptEnd()
  } yield ()

  def main(args: Array[String]): Unit = {
    loop.unsafeRun()
  }
}
