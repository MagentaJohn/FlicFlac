package shared
import scalatags.Text.all.*
import scalatags.Text.tags2

case class Todo(checked: Boolean, text: String) derives upickle.default.ReadWriter

object Shared:
  def renderBody(todos: Seq[Todo], state: String) =
    val filteredTodos = state match
      case "all"       => todos.zipWithIndex
      case "active"    => todos.zipWithIndex.filter(!_._1.checked)
      case "completed" => todos.zipWithIndex.filter(_._1.checked)
    div(
      header(cls := "header")(
        h1("todos please change me"),
        input(cls := "new-todo", placeholder := "What needs to be done?", autofocus := "")
      ),
      tags2.section(cls := "main")(
        input(
          id := "toggle-all",
          cls := "toggle-all",
          `type` := "checkbox",
          if todos.filter(_.checked).size != 0 then checked else ()
        ),
        label(`for` := "toggle-all")("Mark all as complete"),
        ul(cls := "todo-list")(
          for ((todo, index) <- filteredTodos)
            yield li(
              if todo.checked then cls := "completed" else (),
              div(cls := "view")(
                input(
                  cls := "toggle",
                  `type` := "checkbox",
                  if todo.checked then checked else (),
                  data("todo-index") := index
                ),
                label(todo.text),
                button(cls := "destroy", data("todo-index") := index)
              ),
              input(cls := "edit", value := todo.text)
            )
        )
      ),
      footer(cls := "footer")(
        span(cls := "todo-count")(
          strong(todos.filter(!_.checked).size),
          " items left"
        ),
        ul(cls := "filters")(
          li(cls := "todo-all")(
            a(if state == "all" then cls := "selected" else ())("All")
          ),
          li(cls := "todo-active")(
            a(if state == "active" then cls := "selected" else ())("Active")
          ),
          li(cls := "todo-completed")(
            a(if state == "completed" then cls := "selected" else ())("Completed")
          )
        ),
        button(cls := "clear-completed")("Clear completed")
      )
    )
  end renderBody
end Shared
