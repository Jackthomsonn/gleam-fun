import gleam/bytes_builder.{type BytesBuilder}
import gleam/http/elli
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import person.{Person, construct_person_to_json}

pub fn main_svc(_request: Request(t)) -> Response(BytesBuilder) {
  Person(name: "Jack", age: 29, location: ["UK", "Cardiff"])
  |> construct_person_to_json
  |> send_response("application/json")
}

pub fn send_response(data: BytesBuilder, t: String) {
  response.new(200)
  |> response.set_header("content-type", t)
  |> response.set_body(data)
}

pub fn main() {
  elli.become(main_svc, on_port: 3000)
}
