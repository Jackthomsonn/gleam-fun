import gleam/bytes_builder
import gleam/json.{object, string, array, int}

pub type Person {
  Person(name: String, age: Int, location: List(String))
}

pub fn construct_person_to_json(person: Person) {
  object([
    #("name", string(person.name)),
    #("age", int(person.age)),
    #("location", person.location |> array(of: string))
  ])
  |> json.to_string
  |> bytes_builder.from_string
}