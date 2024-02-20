package tutorial

import java.security.KeyStore.Entry.Attribute
import munit._
import munit.Clue.generate

object VarianceSuite {

  sealed trait Animal {
    def sound: String
  }

  // Mammal
  sealed trait Mammal extends Animal

  case class Dog(name: String) extends Mammal {
    override def sound: String = "Woof"
  }

  case class Cat(name: String) extends Mammal {
    override def sound: String = "Meow"
  }

  // Bird
  sealed trait Bird extends Animal

  case class Penguin(name: String) extends Bird {
    override def sound: String = "Honk"
  }

  case class Sparrow(name: String) extends Bird {
    override def sound: String = "Chirp"
  }

  // Vet
  class Vet[-A] {
    def treat(animal: A): Unit = println(
      s"Treating ${animal.getClass.getSimpleName}"
    )
  }

  // PetShop
  class PetShop[+A](private val pet: Option[A] = None) {
    def buyPet[P >: A](p: P): PetShop[P] = {
      println(s"Bought a ${p.getClass.getSimpleName}")
      new PetShop(Some(p))
    }

    def getPet(): Option[A] = pet
  }

}

class VarianceSuite extends munit.FunSuite {

  import VarianceSuite._

  test("buyPet should return a new PetShop instance with the purchased pet") {
    val petShop = new PetShop[Animal]()
    val cat = Cat("Whiskers")
    val updatedPetShop = petShop.buyPet(cat)
    assertEquals(updatedPetShop.getPet(), Some(cat))
  }

  test("getPet should return None when no pet has been bought") {
    val petShop = new PetShop[Animal]()
    assertEquals(petShop.getPet(), None)
  }

  test("getPet should return the purchased pet") {
    val petShop = new PetShop[Animal]()
    val dog = Dog("Buddy")
    val updatedPetShop = petShop.buyPet(dog)
    assertEquals(updatedPetShop.getPet(), Some(dog))
  }

  test("compilation error when assigning PetShop[Animal] to PetShop[Dog]") {

    assertNoDiff(
      compileErrors("val petShop: PetShop[Dog] = new PetShop[Animal]()"),
      """|error:
         |Found:    tutorial.VarianceSuite.PetShop[tutorial.VarianceSuite.Animal]
         |Required: tutorial.VarianceSuite.PetShop[tutorial.VarianceSuite.Dog]
         |
         |The following import might make progress towards fixing the problem:
         |
         |  import munit.Clue.generate
         |
         |val petShop: PetShop[Dog] = new PetShop[Animal]()
         |                           ^
         |""".stripMargin
    )
  }

}
